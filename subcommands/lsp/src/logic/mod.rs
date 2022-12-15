use crate::*;

use std::{
    collections::hash_map::Entry,
    default::default,
    io,
    ops::Not,
    sync::mpsc::{Receiver, TryRecvError},
};

use lsp_types::{notification::*, *};

use lsp_server::*;

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

pub struct LspRuntime {
    middleware: Middleware,
    args: LspArgs,
    middleware_args: MiddlewareArgs,
}

impl LspRuntime {
    pub fn immediate() -> io::Result<()> {
        let (connection, io_threads) = Connection::stdio();

        Self::new(LspArgs::default())?.run(connection);

        io_threads.join()?;

        Ok(())
    }

    pub fn new(args: LspArgs) -> io::Result<Self> {
        Ok(Self {
            middleware: default(),
            args,
            middleware_args: MiddlewareArgs {
                path: default(),
                jit_isa: Isa::host(true)
                    .map_err(|err| io::Error::new(io::ErrorKind::Unsupported, err))?,
                isa: Isa::host(false)
                    .map_err(|err| io::Error::new(io::ErrorKind::Unsupported, err))?,
                incremental_path: None,
                max_cores: None,
                dump_ir: false,
                check: true,
            },
        })
    }

    pub fn run(mut self, connection: Connection) -> Option<!> {
        let params = self.initialize(&connection)?;
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

        for msg in &connection.receiver {
            self.should_terminate()?;

            match msg {
                Message::Request(req) => self.handle_request(req, &connection)?,
                Message::Response(resp) => self.handle_response(resp, &connection),
                Message::Notification(not) => self.handle_notification(not, &connection),
            }
        }

        eprintln!("middleware terminated");

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

    fn recompile(&mut self, connection: &Connection) -> Option<MiddlewareOutput> {
        let output = self.middleware.update(&self.middleware_args);
        if output.is_none() && let Some(ref mut view) = self.middleware.diagnostic_view() {
            Self::clear_diagnostics(connection, view);
            Self::publish_diagnostics(connection, view);
        }
        output
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

        fn pass<T>(v: T) -> T {
            v
        }

        for mut diag in view.workspace.drain() {
            for mut slice in diag.slices.drain(..).filter_map(pass) {
                let entry = diags.entry(slice.origin);

                let diags = match entry {
                    Entry::Occupied(entry) => entry.into_mut(),
                    Entry::Vacant(entry) => {
                        let path = view.resources.source_path(slice.origin);
                        let Ok(uri) = Url::from_file_path(path) else {
                            eprintln!("failed to convert path to uri: {path:?}");
                            continue;
                        };
                        entry.insert(PublishDiagnosticsParams {
                            uri,
                            diagnostics: vec![],
                            version: None,
                        })
                    }
                };

                for annotation in slice.annotations.drain(..).filter_map(pass) {
                    let report = Diagnostic {
                        range: view.resources.project_span(slice.origin, annotation.range),
                        severity: severity(annotation.annotation_type).into(),
                        source: Some(LANG_NAME.into()),
                        message: {
                            let mut label = annotation.label.to_string();
                            if let Some(header) = diag.title.take() {
                                if header.annotation_type == annotation.annotation_type && let Some(ref header_label) = header.label {
                                    label = format!("{header_label}: {label}");
                                } else {
                                    diag.title = Some(header);
                                }
                            }
                            label
                        },
                        ..default()
                    };
                    diags.diagnostics.push(report);
                }
            }
        }

        for (.., file) in diags {
            connection.notify::<PublishDiagnostics>(file);
        }
    }

    fn handle_request(&mut self, req: lsp_server::Request, connection: &Connection) -> Option<()> {
        connection.handle_shutdown(&req).ok()?.not().then_some(())?;

        self.dispatch_request(req, connection)
            .inspect(|e| eprintln!("failed to dispatch request: {e:?}"))
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
        (_)DidChangeTextDocument => {
            self.recompile(connection);
        },
    }

    fn should_terminate(&mut self) -> Option<()> {
        matches!(
            self.args.terminator.as_mut()?.try_recv(),
            Ok(..) | Err(TryRecvError::Disconnected)
        )
        .then_some(())
    }

    fn initialize(&mut self, connection: &Connection) -> Option<InitializeParams> {
        let server_capabilities = serde_json::to_value(&ServerCapabilities {
            definition_provider: Some(OneOf::Left(true)),
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

fn cast_request<R>(
    req: lsp_server::Request,
) -> Result<(RequestId, R::Params), ExtractError<lsp_server::Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notification<R>(
    req: lsp_server::Notification,
) -> Result<R::Params, ExtractError<lsp_server::Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

trait LspNotifier {
    fn notify<T: lsp_types::notification::Notification>(&self, params: T::Params) -> Option<()>;
}

impl LspNotifier for Connection {
    fn notify<T: lsp_types::notification::Notification>(&self, params: T::Params) -> Option<()> {
        let not = lsp_server::Notification::new(T::METHOD.to_string(), params);
        self.sender
            .send(Message::Notification(not))
            .inspect(|e| eprintln!("failed to send notification: {e:?}"))
            .ok()
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
            line: line as u32,
            character: col as u32,
        }
    }
}

fn severity(annotation_type: AnnotationType) -> DiagnosticSeverity {
    match annotation_type {
        AnnotationType::Error => DiagnosticSeverity::ERROR,
        AnnotationType::Warning => DiagnosticSeverity::WARNING,
        AnnotationType::Info => DiagnosticSeverity::INFORMATION,
        AnnotationType::Note => DiagnosticSeverity::HINT,
        AnnotationType::Help => DiagnosticSeverity::HINT,
    }
}
