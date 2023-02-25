use std::{collections::HashMap, default::default, error::Error, fmt::Write, option::Option};

use middleware::{cli::CliInput, ImportsAst, SourceInfo};

use crate::*;

#[derive(Default)]
pub struct FmtRuntimeCtx {
    workers: Vec<FmtWorker>,
}

pub struct FmtRuntime<'m> {
    middleware: &'m mut Middleware,
    ctx: &'m mut FmtRuntimeCtx,
}

impl<'m> FmtRuntime<'m> {
    command_info! {
        HELP
        [
            "ctl fmt [...]\n",
            "Command forces Catalyst to format your code. TODO: configuration",
        ]: MiddlewareArgs::HELP;
        flags {
        }
        values {
        }
    }

    pub fn test_format(
        name: &str,
        middleware: &'m mut Middleware,
        ctx: &'m mut FmtRuntimeCtx,
        resources: &mut dyn ResourceDb,
    ) {
        let mut runtime = FmtRuntime::new(middleware, ctx);
        let cli_input = CliInput::from_string(&format!(
            ". _ {name} --max-cores 1 -quiet -check -no-incremental"
        ))
        .expect("then this needs some tunning");
        let args = MiddlewareArgs::from_cli_input(&cli_input, Self::HELP).expect("same here");
        runtime
            .run_low(FmtCfg::default(), &args, resources)
            .unwrap();
    }

    pub fn new(middleware: &'m mut Middleware, ctx: &'m mut FmtRuntimeCtx) -> Self {
        Self { middleware, ctx }
    }

    pub fn run(mut self, input: CliInput) -> Result<(), Box<dyn Error>> {
        let args = MiddlewareArgs::from_cli_input(&input, Self::HELP)?;
        let cfg = FmtCfg::default();
        let mut resources = OsResources;

        self.run_low(cfg, &args, &mut resources)
    }

    fn run_low(
        &mut self,
        cfg: FmtCfg,
        args: &MiddlewareArgs,
        resources: &mut dyn ResourceDb,
    ) -> Result<(), Box<dyn Error>> {
        let (out, view) = self.middleware.update(args, resources);

        if let Err(err) = view.dump_diagnostics(true, &out) {
            writeln!(args.display(), "{err}")?;
        }

        let thread_count = args.thread_count();
        self.ctx.workers.resize_with(thread_count, default);
        self.ctx
            .workers
            .iter_mut()
            .for_each(|worker| worker.cfg = cfg.clone());

        let Some(view) = self
            .middleware
            .traverse_source_ast(args, &mut self.ctx.workers)
            else {
                if !args.quiet {
                    eprintln!("failed to format the source code");
                }
                return Ok(());
            };

        self.ctx.workers.iter_mut().fold(Ok(()), |acc, worker| {
            acc.and(worker.apply(resources, view.resources))
        })
    }
}

#[derive(Default)]
struct FmtWorker {
    files: HashMap<VRef<Source>, String>,
    cfg: FmtCfg,
    ctx: FmtCtx,
    replacer: SpaceReplacer,
}

impl FmtWorker {
    fn apply(
        &mut self,
        db: &mut dyn ResourceDb,
        resources: &Resources,
    ) -> Result<(), Box<dyn Error>> {
        for (source_id, mut text) in self.files.drain() {
            self.replacer.replace(&mut text, self.cfg.tab_width);
            let path = resources.source_path(source_id);
            db.write_to_string(path, &text)?;
        }

        Ok(())
    }
}

macro build_fmt($self:expr, $ctx:expr) {{
    let source = $ctx.shared.resources.modules[$ctx.module].source;
    let buffer = $self.files.entry(source).or_default();
    Fmt {
        fmt_ctx: &mut $self.ctx,
        buffer,
        cfg: &$self.cfg,
        source: &$ctx.shared.resources.sources[source].content,
    }
}}

impl AstHandler for FmtWorker {
    type Meta = u32;

    type Imports<'a> = (Option<ImportsAst<'a, u32>>, Option<SourceInfo<u32>>);

    type Chunk<'a> = ItemsAstResult<'a, u32>;

    fn imports(&mut self, (imports, header): Self::Imports<'_>, ctx: BaseSourceCtx) {
        build_fmt!(self, ctx).imports(header, imports);
    }

    fn chunk(
        &mut self,
        chunk: Self::Chunk<'_>,
        ctx: BaseSourceCtx,
        _macros: MacroSourceCtx,
    ) -> bool {
        let last = chunk.last.is_none();
        build_fmt!(self, ctx).source(chunk);
        last
    }

    fn parse_imports<'a>(
        &mut self,
        mut parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Imports<'a>> {
        parser.imports()
    }
    fn parse_chunk<'a>(
        &mut self,
        mut parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<Self::Chunk<'a>> {
        parser.items()
    }

    fn parse_manifest<'a>(
        &mut self,
        mut parser: Parser<'_, 'a, Self::Meta>,
    ) -> Option<ManifestAst<'a, Self::Meta>> {
        parser.manifest()
    }

    fn manifest(
        &mut self,
        manifest: ManifestAst<Self::Meta>,
        source: VRef<Source>,
        resources: &Resources,
    ) {
        let buffer = self.files.entry(source).or_default();
        let source = &resources.sources[source].content;
        Fmt {
            fmt_ctx: &mut self.ctx,
            buffer,
            cfg: &self.cfg,
            source,
        }
        .manifest(manifest);
    }

    fn should_skip_manifest(&mut self, package: VRef<Package>, resources: &Resources) -> bool {
        resources.packages[package].is_external
    }
}
