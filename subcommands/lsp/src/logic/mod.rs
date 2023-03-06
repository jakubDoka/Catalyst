use crate::*;

use std::{
    collections::hash_map::Entry,
    default::default,
    io, mem,
    ops::Not,
    path::Path,
    sync::{
        atomic::AtomicUsize,
        mpsc::{Receiver, TryRecvError},
    },
    thread,
    time::{Duration, Instant},
};

use crossbeam::select;
use lsp_types::{notification::*, request::WorkDoneProgressCreate, *};

use lsp_server::*;
use notify::Watcher;

macro_rules! dispatch_message {
    (
        $name:ident,
        $type:ty,
        $caster:ident,
        $self:ident,
        $connection:ident,
        $(
            ($pat:pat)$method:ty => $handler:expr,
        )*
    ) => {
        pub fn $name(&mut $self, req: $type, $connection: &Connection) -> Result<(), ExtractError<$type>> {
            $(
                let req = match $caster::<$method>(req) {
                    Ok($pat) => {
                        $handler;
                        #[allow(unreachable_code)]
                        return Ok(())
                    },
                    Err(err @ ExtractError::JsonError { .. }) => return Err(err),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
            )*
            Err(ExtractError::MethodMismatch(req))
        }
    }
}

const LANG_NAME: &str = "Catalyst";

#[derive(Default)]
pub struct LspArgs {
    terminator: Option<Receiver<()>>,
}

pub struct LspRuntime<'m> {
    middleware: &'m mut Middleware,
    args: LspArgs,
    middleware_args: MiddlewareArgs,
    progress_start: Instant,
}

impl<'m> LspRuntime<'m> {
    pub fn immediate(middleware: &'m mut Middleware) -> io::Result<()> {
        let (connection, io_threads) = Connection::stdio();

        Self::new(LspArgs::default(), middleware)?.run(connection);

        io_threads.join()?;

        Ok(())
    }

    pub fn new(args: LspArgs, middleware: &'m mut Middleware) -> io::Result<Self> {
        Ok(Self {
            middleware,
            args,
            middleware_args: MiddlewareArgs {
                path: default(),
                jit_isa: Isa::host(true)
                    .map_err(|err| io::Error::new(io::ErrorKind::Unsupported, err))?,
                isa: Isa::host(false)
                    .map_err(|err| io::Error::new(io::ErrorKind::Unsupported, err))?,
                incremental_path: Some("incremental/lsp.rkyv".into()),
                max_cores: None,
                dump_ir: false,
                dump_mir: false,
                dump_tir: false,
                check: true,
                quiet: true,
            },
            progress_start: Instant::now(),
        })
    }

    pub fn run(mut self, connection: Connection) -> Option<!> {
        eprintln!("Catalyst LSP server started");
        let params = self.initialize(&connection)?;
        eprintln!("Catalyst LSP server initialized");
        self.middleware_args.path = params
            .root_uri
            .or_else(|| {
                eprintln!("no root uri provided");
                None
            })?
            .to_file_path()
            .ok()
            .or_else(|| {
                eprintln!("root uri is not a file path");
                None
            })?;

        let (input, out) = crossbeam::channel::bounded(0);
        let mut watcher =
            notify::recommended_watcher(move |ev: Result<notify::Event, notify::Error>| {
                let Ok(ev) = ev else {return};

                if !matches!(
                    ev.kind,
                    notify::EventKind::Modify(..)
                        | notify::EventKind::Remove(..)
                        | notify::EventKind::Create(..)
                ) {
                    return;
                }

                if ev
                    .paths
                    .iter()
                    .filter_map(|p| p.extension())
                    .all(|e| e != "ctl" && e != "ctlm")
                {
                    return;
                }

                let _ = input.send(ev);
            })
            .map_err(|err| eprintln!("failed to initialize watcher: {err}"))
            .ok()?;

        watcher
            .watch(Path::new("."), notify::RecursiveMode::Recursive)
            .map_err(|err| eprintln!("failed to start watching workspace: {err}"))
            .ok()?;

        eprintln!("listening for messages");
        loop {
            select! {
                recv(out) -> msg => {
                    let Ok(..) = msg else {break};
                    self.recompile(&connection);
                }
                recv(connection.receiver) -> msg => {
                    let Ok(msg) = msg else {break};
                    self.should_terminate()?;

                    eprintln!("received message: {msg:?}");

                    match msg {
                        Message::Request(req) => self.handle_request(req, &connection)?,
                        Message::Response(resp) => self.handle_response(resp, &connection),
                        Message::Notification(not) => self.handle_notification(not, &connection),
                    }
                }
            }
        }

        eprintln!("server terminated");

        None
    }

    fn handle_response(&mut self, resp: lsp_server::Response, _connection: &Connection) {
        eprintln!("received response: {resp:?}");
    }

    fn handle_notification(&mut self, not: lsp_server::Notification, connection: &Connection) {
        if let Err(err) = self.dispatch_notification(not, connection) {
            eprintln!("failed to handle notification: {err}");
        }
    }

    fn recompile(&mut self, connection: &Connection) -> MiddlewareOutput {
        self.start_progress(connection);
        let (output, ref mut view) = self
            .middleware
            .update(&self.middleware_args, &mut OsResources);
        Self::clear_diagnostics(connection, view);
        if let MiddlewareOutput::Failed = output {
            Self::publish_diagnostics(connection, view);
        }

        self.end_progress(connection);
        output
    }

    const COMPILATION_TOKEN: i32 = 0;
    fn start_progress(&mut self, connection: &Connection) {
        self.progress_start = Instant::now();
        connection.request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: NumberOrString::Number(Self::COMPILATION_TOKEN),
        });
    }

    fn end_progress(&self, connection: &Connection) {
        const EDITOR_BREATHER: Duration = Duration::from_millis(50);
        if let Some(sleep_time) = EDITOR_BREATHER.checked_sub(self.progress_start.elapsed()) {
            thread::sleep(sleep_time);
        }
        connection.notify::<Progress>(ProgressParams {
            token: NumberOrString::Number(Self::COMPILATION_TOKEN),
            value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                message: None,
            })),
        });
    }

    fn clear_diagnostics(connection: &Connection, view: &DiagnosticView) {
        for path in view.changed_files() {
            let Ok(uri) = Url::from_file_path(path) else {
                eprintln!("failed to convert path to uri: {path:?}");
                continue;
            };

            let params = PublishDiagnosticsParams {
                uri,
                diagnostics: vec![],
                version: None,
            };
            connection.notify::<PublishDiagnostics>(params);
        }
    }

    fn publish_diagnostics(connection: &Connection, view: &mut DiagnosticView) {
        let mut diags = Map::<_, PublishDiagnosticsParams>::default();

        let mut display = SnippetDisplayImpl::default();
        let mut display_buffer = String::new();
        let mut snippet = CtlSnippet::default();

        for diag in view.workspace.drain() {
            diag.fill_snippet(&mut snippet);

            let main_source_diag = snippet
                .source_annotations
                .iter()
                .position(|a| snippet.title.annotation_type == a.annotation_type)
                .map(|i| snippet.source_annotations.swap_remove(i))
                .or_else(|| snippet.source_annotations.pop());

            let Some(main_source_diag) = main_source_diag else {
                display_buffer.clear();
                display.display(&*diag, view.resources, &mut display_buffer);
                connection.notify::<ShowMessage>(ShowMessageParams {
                    typ: message_type(snippet.title.annotation_type),
                    message: display_buffer.clone(),
                });
                continue;
            };

            let range = view
                .resources
                .project_span(main_source_diag.origin, main_source_diag.span);
            let Some(t_diags) = Self::get_diag(view.resources, &mut diags, main_source_diag.origin) else {
                continue;
            };
            let uri = t_diags.uri.clone();
            let related_information = snippet
                .footer
                .drain(..)
                .map(|annotation| DiagnosticRelatedInformation {
                    location: Location {
                        uri: uri.clone(),
                        range,
                    },
                    message: annotation.label,
                })
                .chain(snippet.source_annotations.iter().filter_map(|annotation| {
                    let diags = Self::get_diag(view.resources, &mut diags, annotation.origin)?;
                    Some(DiagnosticRelatedInformation {
                        location: Location {
                            uri: diags.uri.clone(),
                            range: view
                                .resources
                                .project_span(annotation.origin, annotation.span),
                        },
                        message: annotation.label.to_owned(),
                    })
                }))
                .collect();

            let report = Diagnostic {
                range,
                severity: severity(main_source_diag.annotation_type).into(),
                source: Some(LANG_NAME.into()),
                message: mem::take(&mut snippet.title.label),
                related_information: Some(related_information),
                ..default()
            };

            for annotation in snippet.source_annotations.drain(..) {
                let report = Diagnostic {
                    range: view
                        .resources
                        .project_span(annotation.origin, annotation.span),
                    severity: severity(annotation.annotation_type).into(),
                    source: Some(LANG_NAME.into()),
                    message: annotation.label,
                    related_information: Some(vec![DiagnosticRelatedInformation {
                        location: Location {
                            uri: uri.clone(),
                            range,
                        },
                        message: "origina diagnostic".into(),
                    }]),
                    ..default()
                };

                let Some(diags) = Self::get_diag(view.resources, &mut diags, annotation.origin) else {
                    continue;
                };

                diags.diagnostics.push(report);
            }

            let Some(diags) = Self::get_diag(view.resources, &mut diags, main_source_diag.origin) else {
                continue;
            };

            diags.diagnostics.push(report);
        }

        for (.., file) in diags {
            connection.notify::<PublishDiagnostics>(file);
        }
    }

    fn get_diag<'a>(
        resources: &Resources,
        map: &'a mut Map<VRef<Source>, PublishDiagnosticsParams>,
        origin: VRef<Source>,
    ) -> Option<&'a mut PublishDiagnosticsParams> {
        match map.entry(origin) {
            Entry::Occupied(entry) => Some(entry.into_mut()),
            Entry::Vacant(entry) => {
                let path = resources.source_path(origin);
                let Ok(uri) = Url::from_file_path(path) else {
                    eprintln!("failed to convert path to uri: {path:?}");
                    return None;
                };
                Some(entry.insert(PublishDiagnosticsParams {
                    uri,
                    diagnostics: vec![],
                    version: None,
                }))
            }
        }
    }

    fn handle_request(&mut self, req: lsp_server::Request, connection: &Connection) -> Option<()> {
        connection.handle_shutdown(&req).ok()?.not().then_some(())?;

        self.dispatch_request(req, connection)
            .inspect_err(|e| eprintln!("failed to dispatch request: {e:?}"))
            .ok()
    }

    dispatch_message!(
        dispatch_request,
        lsp_server::Request,
        cast_request,
        self,
        _connection,
    );

    dispatch_message! {
        dispatch_notification,
        lsp_server::Notification,
        cast_notification,
        self, connection,
        (_)DidChangeWatchedFiles => {
            self.recompile(connection);
        },
    }

    fn should_terminate(&mut self) -> Option<()> {
        matches!(
            self.args.terminator.as_mut().map(|ch| ch.try_recv()),
            Some(Ok(..) | Err(TryRecvError::Disconnected)) | None
        )
        .then_some(())
    }

    fn initialize(&mut self, connection: &Connection) -> Option<InitializeParams> {
        let server_capabilities = serde_json::to_value(ServerCapabilities {
            ..Default::default()
        })
        .inspect_err(|e| eprintln!("failed to serialize server capabilities: {e}"))
        .ok()?;

        let initialization_params = connection
            .initialize(server_capabilities)
            .inspect_err(|e| eprintln!("failed to initialize connection: {e}"))
            .ok()?;

        serde_json::from_value(initialization_params)
            .inspect_err(|e| eprintln!("failed to deserialize initialization params: {e}"))
            .ok()
    }
}

// fn cast_request<R>(
//     req: lsp_server::Request,
// ) -> Result<(RequestId, R::Params), ExtractError<lsp_server::Request>>
// where
//     R: lsp_types::request::Request,
//     R::Params: rkyv::de::DeserializeOwned,
// {
//     req.extract(R::METHOD)
// }

fn cast_notification<R>(
    req: lsp_server::Notification,
) -> Result<R::Params, ExtractError<lsp_server::Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

static REQUEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

trait LspConnectionExt {
    fn notify<T: lsp_types::notification::Notification>(&self, params: T::Params) -> Option<()>;
    fn request<T: lsp_types::request::Request>(&self, params: T::Params) -> Option<usize>;
}

impl LspConnectionExt for Connection {
    fn notify<T: lsp_types::notification::Notification>(&self, params: T::Params) -> Option<()> {
        let not = lsp_server::Notification::new(T::METHOD.to_string(), params);
        self.sender
            .send(Message::Notification(not))
            .inspect_err(|e| eprintln!("failed to send notification: {e:?}"))
            .ok()
    }

    fn request<T: lsp_types::request::Request>(&self, params: T::Params) -> Option<usize> {
        let id = REQUEST_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let req =
            lsp_server::Request::new(RequestId::from(id as i32), T::METHOD.to_string(), params);
        self.sender
            .send(Message::Request(req))
            .inspect_err(|e| eprintln!("failed to send request: {e:?}"))
            .ok()
            .map(|_| id)
    }
}

trait LspSpanProjector {
    fn project_span(&self, source: VRef<Source>, span: Span) -> lsp_types::Range;
    fn project_pos(&self, source: VRef<Source>, pos: u32) -> Position;
}

impl LspSpanProjector for Resources {
    fn project_span(&self, source: VRef<Source>, span: Span) -> lsp_types::Range {
        lsp_types::Range {
            start: self.project_pos(source, span.start),
            end: self.project_pos(source, span.end),
        }
    }

    fn project_pos(&self, source: VRef<Source>, pos: u32) -> Position {
        let (line, col) = self.sources[source].line_mapping.line_info_at(pos as usize);
        Position {
            line: line as u32 - 1,
            character: col as u32,
        }
    }
}

fn severity(annotation_type: CtlAnnotationType) -> DiagnosticSeverity {
    match annotation_type {
        CtlAnnotationType::Error => DiagnosticSeverity::ERROR,
        CtlAnnotationType::Warning => DiagnosticSeverity::WARNING,
        CtlAnnotationType::Info => DiagnosticSeverity::INFORMATION,
        CtlAnnotationType::Note => DiagnosticSeverity::HINT,
        CtlAnnotationType::Help => DiagnosticSeverity::HINT,
    }
}

fn message_type(annotation_type: CtlAnnotationType) -> MessageType {
    match annotation_type {
        CtlAnnotationType::Error => MessageType::ERROR,
        CtlAnnotationType::Warning => MessageType::WARNING,
        CtlAnnotationType::Info => MessageType::INFO,
        CtlAnnotationType::Note => MessageType::INFO,
        CtlAnnotationType::Help => MessageType::INFO,
    }
}
