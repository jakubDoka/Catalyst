use std::{collections::HashMap, default::default, option::Option};

use middleware::{ImportsAst, SourceInfo};

use crate::*;

pub struct FmtRuntime<'m> {
    middleware: &'m mut Middleware,
    workers: Vec<FmtWorker>,
}

impl<'m> FmtRuntime<'m> {
    pub fn new(middleware: &'m mut Middleware) -> Self {
        Self {
            middleware,
            workers: default(),
        }
    }

    pub fn run(&mut self, cfg: FmtCfg, args: &MiddlewareArgs, resources: &mut dyn ResourceDb) {
        let (out, view) = self.middleware.update(args, resources);

        if out.is_failed() && let Err(err) = view.dump_diagnostics(true, out) {
            eprintln!("{err}");
            return;
        }

        let thread_count = args.thread_count();
        self.workers.resize_with(thread_count, default);
        self.workers
            .iter_mut()
            .for_each(|worker| worker.cfg = cfg.clone());

        let view = self
            .middleware
            .traverse_source_ast(args, &mut self.workers)
            .expect("thread count should be nonzero and build should be clean");

        self.workers
            .iter_mut()
            .for_each(|worker| worker.apply(resources, view.resources))
    }
}

#[derive(Default)]
struct FmtWorker {
    files: HashMap<VRef<Source>, String>,
    cfg: FmtCfg,
    ctx: FmtCtx,
}

impl FmtWorker {
    fn apply(&mut self, db: &mut dyn ResourceDb, resources: &Resources) {
        for (source_id, text) in self.files.drain() {
            let path = resources.source_path(source_id);
            if let Err(err) = db.write_to_string(path, &text) {
                eprintln!("failed to write to {}: {}", path.display(), err);
            };
        }
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

impl SourceAstHandler for FmtWorker {
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
}
