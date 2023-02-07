use std::{default::default, iter, str::FromStr};

use crate::{
    ctx::{DropFrame, FuncMirCtx},
    *,
};
use diags::*;
use lexing_t::*;
use mir_t::*;
use packaging_t::{Module, Resources};
use storage::*;
use typec_t::*;

mod control_flow;
mod data;
pub mod moves;

type NodeRes = OptVRef<ValueMir>;

pub struct MirCompilationCtx<'i, 'm> {
    pub module_ent: &'m mut ModuleMir,
    pub reused: &'m mut ReusedMirCtx,
    pub mir: &'m mut Mir,
    pub typec: &'m mut Typec,
    pub interner: &'m mut Interner,
    pub workspace: &'m mut Workspace,
    pub arena: &'i Arena,
    pub resources: &'i Resources,
}

pub fn compile_functions(
    module: VRef<Module>,
    module_ref: FragRef<ModuleMir>,
    funcs: &[(FragRef<Func>, TirFunc)],
    ctx: &mut MirCompilationCtx<'_, '_>,
) {
    for &(func, tir) in funcs {
        let Func {
            signature: Signature { args, ret, .. },
            flags,
            ..
        } = ctx.typec[func];

        let meta = MirBuildMeta {
            source: ctx.resources.modules[module].source,
            module,
            no_moves: flags.contains(FuncFlags::NO_MOVES),
        };

        let generics = ctx
            .typec
            .pack_func_param_specs(func)
            .collect::<BumpVec<_>>();
        let Some(body) = MirBuilder::new(
            ret,
            &generics,
            ExternalMirCtx {
                typec: ctx.typec,
                interner: ctx.interner,
                workspace: ctx.workspace,
                arena: ctx.arena,
                resources: ctx.resources
            },
            meta,
            ctx.module_ent,
            module_ref,
            ctx.reused,
        )
        .build(args, tir.body) else {continue};

        ctx.mir.bodies.insert(BodyOwner::Func(func), body);
    }
}

pub fn compile_constants(
    module: VRef<Module>,
    module_ref: FragRef<ModuleMir>,
    funcs: &[(FragRef<Const>, TirNode)],
    ctx: MirCompilationCtx<'_, '_>,
) {
    for &(r#const, tir) in funcs {
        let meta = MirBuildMeta {
            source: ctx.resources.modules[module].source,
            module,
            no_moves: false,
        };

        let ret = ctx.typec[r#const].ty;

        let Some(body) = MirBuilder::new(
            ret,
            &[],
            ExternalMirCtx {
                typec: ctx.typec,
                interner: ctx.interner,
                workspace: ctx.workspace,
                arena: ctx.arena,
                resources: ctx.resources
            },
            meta,
            ctx.module_ent,
            module_ref,
            ctx.reused,
        )
        .build(default(), tir) else {continue};
        ctx.mir.bodies.insert(BodyOwner::Const(r#const), body);
    }
}

pub struct MirBuilder<'i, 'm> {
    block: OptVRef<BlockMir>,
    depth: u32,
    ext: ExternalMirCtx<'m, 'i>,
    meta: MirBuildMeta,
    reused: &'m mut ReusedMirCtx,
    func: FuncMirCtx<'m, 'i>,
}

impl<'i, 'm> MirBuilder<'i, 'm> {
    pub fn new(
        ret: Ty,
        generics: &'i [FragSlice<Spec>],
        ext: ExternalMirCtx<'m, 'i>,
        meta: MirBuildMeta,
        module: &'m mut ModuleMir,
        module_ref: FragRef<ModuleMir>,
        reused: &'m mut ReusedMirCtx,
    ) -> Self {
        Self {
            block: None,
            depth: 0,
            ext,
            meta,
            func: FuncMirCtx::new(ret, generics, module_ref, module, reused),
            reused,
        }
    }

    pub fn build(mut self, args: FragSlice<Ty>, body: TirNode) -> Option<FuncMir> {
        let frame = self.start_scope_frame();
        let entry = self.func.create_block();
        self.select_block(entry);
        let args = self.push_args(args);
        self.node(body, None);
        self.discard_scope_frame(frame);
        Some(self.func.finish(args, entry, self.reused))
    }

    fn push_args(&mut self, args: FragSlice<Ty>) -> BumpVec<VRef<ValueMir>> {
        self.ext.typec[args]
            .iter()
            .map(|&ty| self.func.create_var(ty, self.reused))
            .collect::<BumpVec<_>>()
    }

    fn node(
        &mut self,
        TirNode { kind, ty, span, .. }: TirNode,
        dest: OptVRef<ValueMir>,
    ) -> NodeRes {
        macro pass($dest:ident => $node:expr) {{
            let $dest = dest.unwrap_or_else(|| self.create_value(ty));
            $node
        }}

        use TirKind::*;
        match kind {
            Int(i) => pass!(dest => self.number(i, dest, span, InstMir::Int)),
            Float(f) => pass!(dest => self.number(f, dest, span, InstMir::Float)),
            Char => pass!(dest => self.char(dest, span)),
            Bool(b) => pass!(dest => self.bool(b, dest, span)),
            Block(b) => self.block(b, dest, span),
            Return(r) => self.r#return(r, span),
            Call(&c) => pass!(dest => self.call(c, ty, dest, span)),
            ConstAccess(c) => pass!(dest => {
                self.inst(InstMir::ConstAccess(c, dest), span);
                Some(dest)
            }),
            Access(a) => self.access(a, dest, span),
            Ctor(c) => self.ctor(c, ty, dest, span),
            Deref(&d) => pass!(dest => self.deref(d, dest, span)),
            Ref(&r) => pass!(dest => self.r#ref(r, dest, span)),
            Match(&m) => pass!(dest => self.r#match(m, dest, span)),
            If(&i) => pass!(dest => self.r#if(i, dest, span)),
            Field(&f) => self.field(f, ty, dest, span),
            Let(&l) => self.r#let(l),
            Assign(&a) => self.assign(a, span),
            Loop(&l) => pass!(dest => self.r#loop(l, dest, span)),
            Continue(c) => self.r#continue(c, span),
            Break(&b) => self.r#break(b, span),
        }
    }

    fn block(&mut self, nodes: &[TirNode], dest: OptVRef<ValueMir>, span: Span) -> NodeRes {
        let Some((&last, nodes)) = nodes.split_last() else {
            return Some(self.func.unit);
        };

        let frame = self.start_scope_frame();
        let res = (|| {
            for &node in nodes {
                let value = self.node(node, None)?;
                self.drop(value, node.span);
            }

            self.node(last, dest)
        })();

        match res {
            Some(..) => self.end_scope_frame(frame, span),
            None => self.discard_scope_frame(frame),
        }

        res
    }

    fn select_block(&mut self, block: VRef<BlockMir>) -> OptVRef<BlockMir> {
        self.block.replace(block)
    }

    fn inst(&mut self, kind: InstMir, span: Span) -> Option<()> {
        self.block?;
        self.reused.inst(kind, span);
        Some(())
    }

    // TODO: rename to start_drop_frame
    fn start_scope_frame(&mut self) -> DropFrame {
        self.reused.create_drop_frame()
    }

    // TODO: rename to discard_drop_frame
    fn discard_scope_frame(&mut self, frame: DropFrame) {
        self.reused.disard_drop_frame(frame);
    }

    fn create_params(&mut self, params: &[Ty]) -> VSlice<VRef<MirTy>> {
        self.func.create_params(params, self.reused)
    }

    fn field(
        &mut self,
        FieldTir { field, header }: FieldTir,
        ty: Ty,
        dest: OptVRef<ValueMir>,
        span: Span,
    ) -> NodeRes {
        let node = self.node(header, None)?;
        Some(self.gen_field(node, dest, field, ty, span))
    }
}

fn parse_char(repr: &mut impl Iterator<Item = char>) -> Option<char> {
    let char = repr.next()?;

    Some(match char {
        '\\' => match repr.next()? {
            'n' => '\n',
            _ => return None,
        },
        c => c,
    })
}

ctl_errors! {
    #[err => "match is not exhaustive"]
    #[info => "missing patterns: {gaps}"]
    #[help => "adding '_ {{}}' will make the match exhaustive"]
    error NonExhaustive: fatal {
        #[err loc]
        gaps ref: String,
        loc: SourceLoc,
    }
}
