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

    pub fn run(&mut self) {}
}
