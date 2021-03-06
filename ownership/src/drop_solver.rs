use crate::*;
use storage::*;
use typec_types::*;

use crate::DropSolver;

impl DropSolver<'_> {
    pub fn solve(&mut self, func: Func) {
        let FuncMeta { body, .. } = self.funcs[func.meta()];

        if body.is_reserved_value() {
            return;
        }

        self.traverse(body);
    }

    fn traverse(&mut self, root: Tir) {
        let mut seen = EntitySet::new();
        let mut frontier = self.vec_pool.alloc(&[root]);
        let mut drop_buffer = self.vec_pool.get();
        while let Some(node) = frontier.pop() {
            if !seen.insert(node) {
                continue;
            }
            let TirEnt { kind, flags, .. } = self.tir_data.ents[node];
            match kind {
                TirKind::Match(.., branches) => frontier.push(branches),
                TirKind::MultiEntryBlock(block) => frontier.push(block),

                TirKind::Continue(header, ..) => {
                    let list = self.collect_drops(node, &mut drop_buffer);
                    self.tir_data.ents[node].kind = TirKind::Continue(header, list);
                }
                TirKind::Block(items, ..) => {
                    frontier.extend_from_slice(self.tir_data.cons.get(items));

                    if !flags.contains(TirFlags::TERMINATING) {
                        let list = self.collect_drops(node, &mut drop_buffer);
                        self.tir_data.ents[node].kind = TirKind::Block(items, list);
                    }
                }
                TirKind::Break(header, ret, ..) => {
                    if let Some(ret) = ret.expand() {
                        frontier.push(ret);
                    }
                    let list = self.collect_drops(node, &mut drop_buffer);
                    self.tir_data.ents[node].kind = TirKind::Break(header, ret, list);
                }
                TirKind::Return(ret, ..) => {
                    if let Some(ret) = ret.expand() {
                        frontier.push(ret);
                    }
                    let list = self.collect_drops(node, &mut drop_buffer);
                    self.tir_data.ents[node].kind = TirKind::Return(ret, list);
                }
                TirKind::Assign(to, from, ..) => {
                    frontier.extend_from_slice(&[to, from]);
                    let list = self.collect_drops(node, &mut drop_buffer);
                    self.tir_data.ents[node].kind = TirKind::Assign(to, from, list);
                }

                TirKind::Loop(header)
                | TirKind::DerefPtr(header)
                | TirKind::TakePtr(header)
                | TirKind::BitCast(header)
                | TirKind::Variable(header)
                | TirKind::FieldAccess(header, ..) => frontier.push(header),

                TirKind::If(conf, then, otherwise, ..) => {
                    frontier.extend_from_slice(&[conf, then, otherwise]);
                    let pre_computes = self.o_ctx.pre_eval[node];
                    let list = self
                        .tir_data
                        .cons
                        .push(self.o_ctx.pre_eval_lists.get(pre_computes));
                    self.tir_data.ents[node].kind = TirKind::If(conf, then, otherwise, list);
                }

                TirKind::Constructor(args) | TirKind::Call(.., args) => {
                    frontier.extend_from_slice(self.tir_data.cons.get(args))
                }

                TirKind::IndirectCall(call, args) => {
                    frontier.push(call);
                    frontier.extend_from_slice(self.tir_data.cons.get(args))
                }

                TirKind::Access(..)
                | TirKind::GlobalAccess(..)
                | TirKind::IntLit(..)
                | TirKind::Argument(_)
                | TirKind::BoolLit(..)
                | TirKind::CharLit(..)
                | TirKind::FuncPtr(..)
                | TirKind::Uninit => (),

                TirKind::LoopInProgress(..) | TirKind::Invalid => unreachable!(),
            }
        }
    }

    fn collect_drops(&mut self, target: Tir, buffer: &mut Vec<(Tir, DropNode)>) -> TirList {
        buffer.extend(
            self.o_ctx
                .drop_nodes
                .get_iter(self.o_ctx.drops[target])
                .map(|(k, node)| (node.meta.unwrap(), k)),
        );

        let mut i = 0;
        while let Some(&(tir, node)) = buffer.get(i) {
            let ty = self.tir_data.ents[tir].ty;

            let DropNodeEnt { children, .. } = self.o_ctx.drop_nodes[node];

            // if !self.types[ty].flags.contains(TyFlags::DROP) {

            //     buffer.remove(i);
            // } else {
            // }
            i += 1;

            for (k, &child) in self.o_ctx.drop_nodes.get_iter(children) {
                let meta = child.meta.unwrap_err();
                let field = {
                    let kind = TirKind::FieldAccess(tir, meta);
                    let ty = ty_factory!(self).subtype(ty, self.ty_comps[meta].ty);
                    self.scope_context.use_type(ty, self.types);
                    let span = self.tir_data.ents[tir].span;
                    let ent = TirEnt::new(kind, ty, span);
                    self.tir_data.ents.push(ent)
                };
                buffer.push((field, k));
            }
        }

        self.tir_data
            .cons
            .push_iter(buffer.drain(..).map(|(tir, _)| tir))
    }
}
