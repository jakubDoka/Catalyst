use std::default::default;

use crate::*;

pub struct FmtRuntime<'m> {
    middleware: &'m mut Middleware,
    ctx: FmtCtx,
    cfg: FmtCfg,
}

impl<'m> FmtRuntime<'m> {
    pub fn new(cfg: FmtCfg, middleware: &'m mut Middleware) -> Self {
        Self {
            middleware,
            ctx: default(),
            cfg,
        }
    }

    pub fn run(&mut self, args: &MiddlewareArgs, resources: &mut dyn ResourceDb) {
        let (out, view) = self.middleware.update(args, resources);
        if let Err(err) = view.dump_diagnostics(true, out) {
            eprintln!("{err}");
            return;
        }

        for package in view.resources.packages.values() {
            let source = &view.resources.sources[package.source];
        }
    }
}
