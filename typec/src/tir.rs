use std::{default::default, ops::Not};

use crate::*;
use ast::*;
use lexer::*;
use matching::*;
use module_types::*;
use storage::*;
use typec_types::*;

type Expected = Option<Ty>;

impl TirBuilder<'_> {
    pub fn global(&mut self, global: Global) -> errors::Result<Func> {
        self.global = global;

        let ast = self.scope_context.global_ast[self.global];
        let value = self.expr(ast, None)?;
        let ret = self.tir_data.ents[value].ty;

        let body = {
            let kind = TirKind::Return(value.into(), default());
            let ent = TirEnt::new(kind, ret, self.ast_data.nodes[ast].span);
            self.tir_data.ents.push(ent)
        };

        self.globals[self.global].ty = ret;

        let id = ID::new("<global>") + self.globals[self.global].id;

        let func_ent = FuncEnt {
            id,
            flags: FuncFlags::ANONYMOUS,
        };
        let func_meta = FuncMeta {
            sig: Sig { ret, ..default() },
            name: self.globals[global].name,
            body,

            ..default()
        };
        let func = self.funcs.push(func_ent, func_meta);

        self.globals[self.global].init = Some(func).into();
        self.func_instances.insert(id, func);

        Ok(func)
    }

    pub fn func(&mut self, func: Func) -> errors::Result {
        self.func = func;
        let FuncMeta {
            kind,
            params,
            sig: Sig { ret, args, .. },
            ..
        } = self.funcs[self.func.meta()];
        let flags = self.funcs[self.func].flags;

        self.scope_context.generic = flags.contains(FuncFlags::GENERIC);

        //println!("{}", self.func_meta[self.func].name.log(self.sources));

        let ast = self.scope_context.func_ast[self.func];
        let header = self.ast_data.children(ast);
        let &[generics, .., ret_ast, _] = header else {
            unreachable!();
        };
        let &body_ast = header.last().unwrap();
        if body_ast.is_reserved_value() {
            if flags.contains(FuncFlags::EXTERNAL) {
                self.funcs[self.func.meta()].kind = FuncKind::External;
            }
            return Ok(());
        }

        self.scope.mark_frame();
        if let FuncKind::Owned(ty) = kind {
            let span = self.ast_data.nodes[ast].span;
            self.scope.push_item("Self", ScopeItem::new(ty, span));
        }

        self.generics(params, generics);

        // println!("{}", self.ast_data.nodes[ast].span.log(self.sources));

        let args = {
            let args = self.ty_comps.get(args);
            let mut tir_args = self.vec_pool.with_capacity(args.len());
            for (i, &TyCompEnt { ty, name, .. }) in args.iter().enumerate() {
                // println!("=== {} {} {:?}", ty_display!(self, ty), self.ast_data.nodes[ast].span.log(self.sources), self.types[ty].flags);
                self.scope_context.use_type(ty, self.types);

                let arg = {
                    let kind = TirKind::Argument(i as u32);
                    let ent = TirEnt::new(kind, ty, name);
                    self.tir_data.ents.push(ent)
                };

                {
                    let id = {
                        let str = self.sources.display(name);
                        ID::new(str)
                    };
                    let item = ScopeItem::new(arg, name);
                    self.scope.push_item(id, item);
                }

                tir_args.push(arg);
            }
            self.tir_data.cons.push(&tir_args)
        };

        let root = self.block_low(body_ast, Some((ret, ret_ast)), None)?;
        self.funcs[self.func.meta()].body = root;
        self.funcs[self.func.meta()].args = args;

        self.scope.pop_frame();

        Ok(())
    }

    fn generics(&mut self, params: TyList, generics: Ast) {
        if generics.is_reserved_value() {
            return;
        }

        let ast = self.ast_data.children(generics);
        let bounds = self.ty_lists.get(params);
        for (&item, &bound_combo) in ast.iter().zip(bounds) {
            let name = self.ast_data.children(item)[0];
            let span = self.ast_data.nodes[name].span;
            let str = self.sources.display(span);
            self.scope.push_item(str, ScopeItem::new(bound_combo, span));

            let TyKind::Param(_, bounds, ..) = self.types[bound_combo].kind else {
                unreachable!();
            };

            for &bound in self.ty_lists.get(bounds) {
                let TyKind::Bound(funcs) = self.types[bound].kind else {
                    unreachable!();
                };

                // TODO: can caching ids make less cache misses?
                for &func in self.func_lists.get(funcs) {
                    let name = self.funcs[func.meta()].name;
                    let id = {
                        let id = self.sources.id_of(name);
                        let bound = self.types[bound_combo].id;
                        ID::owned(bound, id)
                    };
                    self.scope.push_item(id, ScopeItem::new(func, name));
                }
            }
        }
    }

    fn block(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        self.block_low(ast, None, expected)
    }

    fn block_low(&mut self, ast: Ast, root_data: Option<(Ty, Ast)>, expected: Expected) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;

        self.tir_stack.mark_frame();
        self.scope.mark_frame();

        let mut final_expr = None;
        let mut terminal_flags = TirFlags::empty();
        for (i, &stmt) in self.ast_data.children(ast).iter().enumerate() {
            let expected = (i == self.ast_data.children(ast).len() - 1).then_some(expected).flatten();
            let Ok(expr) = self.expr(stmt, expected) else {
                continue; // Recover here
            };
            self.tir_stack.push(expr);
            final_expr = Some(expr);
            terminal_flags |= self.terminal_flags(expr);
            if !terminal_flags.is_empty() {
                break; // TODO: emit warning
            }
        }

        let result = {
            let ty = if let Some(expr) = final_expr {
                self.tir_data.ents[expr].ty
            } else {
                self.builtin_types.nothing
            };

            if let Some((ret, ret_ast)) = root_data && !terminal_flags.contains(TirFlags::TERMINATING) {
                let because = ret_ast
                    .is_reserved_value()
                    .not()
                    .then(|| self.ast_data.nodes[ret_ast].span);

                let ret_value = (ret != self.builtin_types.nothing).then_some(final_expr).flatten();

                if ret_value.is_some() {
                    self.infer_ty(ty, ret, |_| TyError::ReturnTypeMismatch {
                        because,
                        expected: ret,
                        got: ty,
                        loc: span,
                    })?;
                }

                let kind = TirKind::Return(ret_value.into(), default());
                let ent = TirEnt::new(kind, ret, span);
                let tir = self.tir_data.ents.push(ent);
                self.tir_stack.push(tir);
                terminal_flags |= TirFlags::TERMINATING;
            }

            let slice = self.tir_stack.top_frame();
            let items = self.tir_data.cons.push(slice);
            let kind = TirKind::Block(items, default());
            let flags = terminal_flags;
            let ent = TirEnt {
                kind,
                ty,
                flags,
                span,
            };
            self.tir_data.ents.push(ent)
        };

        self.tir_stack.pop_frame();
        self.scope.pop_frame();

        Ok(result)
    }

    fn expr(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let expected = expected.map(|ty| self.types[ty].flags
            .contains(TyFlags::GENERIC).not().then_some(ty)).flatten();
        let ast::AstEnt { kind, span, .. } = self.ast_data.nodes[ast];
        match kind {
            AstKind::Unary => self.unary(ast),
            AstKind::Binary => self.binary(ast),
            AstKind::Ident | AstKind::Path => self.symbol(ast),
            AstKind::If => self.r#if(ast, expected),
            AstKind::Return => self.r#return(ast),
            AstKind::Int => self.int(ast, expected),
            AstKind::Bool => self.bool(ast),
            AstKind::Char => self.char(ast),
            AstKind::String => self.string(ast),
            AstKind::Variable(mutable) => self.variable(mutable, ast),
            AstKind::Constructor => self.constructor(ast, expected),
            AstKind::DotExpr => self.dot_expr(ast),
            AstKind::Call => self.call(ast, expected),
            AstKind::Block => self.block(ast, expected),
            AstKind::Loop => self.r#loop(ast, expected),
            AstKind::Break => self.r#break(ast),
            AstKind::Continue => self.r#continue(ast),
            AstKind::Deref => self.deref(ast),
            AstKind::BitCast => self.bit_cast(ast, expected),
            AstKind::Match => self.r#match(ast, expected),
            AstKind::Ref(mutable) => self.r#ref(ast, mutable, expected),
            AstKind::Error => Err(()),
            _ => unimplemented!(
                "Unhandled expression ast {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }
    }

    fn string(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;

        let (ptr, len) = {
            let str = self.sources.display(span);
            let id = ID::new(str);

            let global = if let Some(&global) = self.global_map.get(id) {
                global
            } else {
                let mut translator = EscapeTranslator::new(&str[1..str.len() - 1]);
                let bytes = {
                    let bytes = (&mut translator).flat_map(|c| {
                        let mut data = [0u8; 4];
                        let len = c.encode_utf8(&mut data).len();
                        data.into_iter().take(len)
                    });
                    self.global_data.push_iter(bytes)
                };

                if let Some((error, pos)) = translator.report_error() {
                    self.diagnostics.push(TyError::StringError {
                        error,
                        pos,
                        loc: span,
                    });
                }

                let bytes = bytes.is_reserved_value().not().then_some(bytes).into();

                let global = GlobalEnt {
                    id,
                    ty: self.builtin_types.u8,
                    bytes,
                    name: span,

                    ..default()
                };

                let global = self.globals.push(global);
                self.global_map.insert(id, global);
                global
            };

            let global_access = {
                let kind = TirKind::GlobalAccess(global);
                let flags = TirFlags::IMMUTABLE;
                let ty = self.builtin_types.uint;
                let ent = TirEnt::with_flags(kind, ty, flags, span);
                self.tir_data.ents.push(ent)
            };

            (
                self.take_ptr(global_access, false),
                self.globals[global]
                    .bytes
                    .map(|bytes| self.global_data.get(bytes).len())
                    .unwrap_or(0),
            )
        };

        let len = self.int_lit(len as u128, self.builtin_types.uint);

        let result = {
            let args = self.tir_data.cons.push(&[ptr, len]);
            let kind = TirKind::Constructor(args);
            let ty = self.builtin_types.str;
            let ent = TirEnt::new(kind, ty, span);
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn r#continue(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[label] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let (loop_expr, is_lowest) = self.find_loop(label, true)?;
        let kind = TirKind::Continue(loop_expr, default());
        let ent = TirEnt::with_flags(
            kind,
            self.builtin_types.nothing,
            TirFlags::TERMINATING | (TirFlags::CONTINUE & is_lowest),
            span,
        );
        Ok(self.tir_data.ents.push(ent))
    }

    fn r#ref(&mut self, ast: Ast, mutable: bool, expected: Expected) -> errors::Result<Tir> {
        let expr = self.ast_data.children(ast)[0];
        let expr = self.expr(expr, expected.map(|e| self.types.deref(e)))?;
        Ok(self.take_ptr(expr, mutable))
    }

    fn r#match(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let &[expr, body] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let expr = self.expr(expr, None)?;
        let span = self.ast_data.nodes[ast].span;

        self.tir_pattern_graph.clear();

        // load match arms
        let mut temp = vec![];
        let mut terminal_flags = TirFlags::all().terminal_flags();
        for &arm in self.ast_data.children(body) {
            let &[pat, body] = self.ast_data.children(arm) else {
                unreachable!();
            };

            drop(self.pattern(pat, expr, &mut temp, 1, span));

            // println!("{}", temp.iter()
            //     .map(|r| self.tir_data.ents[r.meta].ty)
            //     .map(|ty| format!("{}", ty_display!(self, ty)))
            //     .collect::<Vec<_>>()
            //     .join(" "),
            // );

            let block = {
                let inner_block = {
                    self.scope.mark_frame();
                    self.tir_stack.mark_frame();

                    for node in &temp {
                        if let TirPatternMeta::Var(name) = node.value {
                            let variable = {
                                let kind = TirKind::Variable(node.meta);
                                let ty = self.builtin_types.nothing;
                                let ent = TirEnt::new(kind, ty, name);
                                self.tir_data.ents.push(ent)
                            };

                            let id = self.sources.id_of(name);
                            let item = ScopeItem::new(variable, name);
                            self.scope.push_item(id, item);

                            self.tir_stack.push(variable)
                        }
                    }

                    let expr = self.expr(body, expected)?;
                    terminal_flags &= self.terminal_flags(expr);
                    self.tir_stack.push(expr);

                    self.scope.pop_frame();

                    self.tir_data.ents.push({
                        let content = self.tir_stack.save_and_pop_frame(&mut self.tir_data.cons);
                        let kind = TirKind::Block(content, default());
                        TirEnt {
                            kind,
                            ..self.tir_data.ents[expr]
                        }
                    })
                };

                self.tir_data.ents.push({
                    let kind = TirKind::MultiEntryBlock(inner_block);
                    let ty = self.tir_data.ents[inner_block].ty;
                    let span = self.ast_data.nodes[body].span;
                    TirEnt::new(kind, ty, span)
                })
            };

            temp.push(PatternLevelData {
                meta: block.into(),
                ..default()
            });

            self.tir_pattern_graph.add_branch(temp.drain(..));
        }

        self.tir_pattern_graph.build_graph().unwrap();

        let branching = self.match_branching()?;
        Ok(self.tir_data.ents.push({
            let kind = TirKind::Match(expr, branching);
            let ty = self.tir_data.ents[branching].ty;
            let span = self.ast_data.nodes[ast].span;
            let flags = terminal_flags;
            TirEnt {
                kind,
                ty,
                flags,
                span,
            }
        }))
    }

    fn match_branching(&mut self) -> errors::Result<Tir> {
        self.match_branching_recursive(self.tir_pattern_graph.root())
    }

    fn match_branching_recursive(&mut self, root: PatternNode) -> errors::Result<Tir> {
        let root_ent = self.tir_pattern_graph.node(root);

        // if !root_ent.meta.is_reserved_value() {
        //     let ty = self.tir_data.ents[root_ent.meta].ty;
        //     println!("{}", ty_display!(self, ty));
        // }

        let Some(&last) = self.tir_pattern_graph.slice(root_ent.children).last() else {
            return Ok(root_ent.meta);
        };

        let mut base = self.match_branching_recursive(last)?;

        for i in (0..self.tir_pattern_graph.slice(root_ent.children).len() - 1).rev() {
            let branch = self.tir_pattern_graph.slice(root_ent.children)[i];
            let branch_ent = self.tir_pattern_graph.node(branch);
            let expr = branch_ent.meta;
            let TirEnt {
                ty, span, flags, ..
            } = self.tir_data.ents[expr];

            let comparison = if branch_ent.coverage.is_one() {
                let func = {
                    let id = {
                        let ty_id = self.types[ty].id;
                        let op_id = ID::new("==");
                        ID::binary(ty_id, op_id)
                    };
                    self.scope.get_concrete::<Func>(id).unwrap()
                };

                let int_lit = self.int_lit(branch_ent.coverage.start, ty);

                let args = self.tir_data.cons.push(&[expr, int_lit]);
                let kind = TirKind::Call(ty.into(), TyList::reserved_value(), func, args);
                let ty = self.builtin_types.bool;
                let ent = TirEnt::new(kind, ty, span);
                self.tir_data.ents.push(ent)
            } else {
                unimplemented!();
            };

            base = {
                let expr = self.match_branching_recursive(branch)?;
                let flags = self.tir_data.ents[base].flags & flags;
                let kind = TirKind::If(comparison, expr, base, default());
                let expr_ty = self.tir_data.ents[expr].ty;
                let base_ty = self.tir_data.ents[base].ty;
                let ty = if expr_ty != base_ty {
                    self.builtin_types.nothing
                } else {
                    expr_ty
                };
                let ent = TirEnt::with_flags(kind, ty, flags, span);
                self.tir_data.ents.push(ent)
            };
        }

        Ok(base)
    }

    fn pattern(
        &mut self,
        ast: Ast,
        expr: Tir,
        branch: &mut Vec<PatternLevelData<Tir, TirPatternMeta>>,
        depth: u32,
        base_expr_span: Span,
    ) -> errors::Result {
        let AstEnt { kind, span, .. } = self.ast_data.nodes[ast];
        match kind {
            AstKind::StructPattern | AstKind::TupleStructPattern => {
                let is_tuple = kind == AstKind::TupleStructPattern;
                let &[path, body] = self.ast_data.children(ast) else {
                    unreachable!();
                };

                let low_expr = self.path_match(expr, path, branch, depth)?;
                let ty = self.tir_data.ents[low_expr].ty;

                let (ty, pointer, mutable) = self.types.may_deref(ty);

                let TyKind::Struct(fields) = self.types[ty].kind else {
                    unreachable!();
                };

                let mut last_i = 0;
                for (i, &field) in self.ast_data.children(body).iter().enumerate() {
                    let ((field_id, field_ty), field, i) = if is_tuple {
                        let span = self.ast_data.nodes[field].span;
                        let Ok(res) = self.get_field(ty, i, span) else {
                            continue;
                        };
                        (res, field, i)
                    } else {
                        let &[name, mut pattern] = self.ast_data.children(field) else {
                            unreachable!("{}", self.sources.display(span));
                        };

                        // shorthand case
                        if pattern.is_reserved_value() {
                            pattern = name;
                        }

                        let span = self.ast_data.nodes[name].span;
                        let ty = self.types.ptr_leaf_of(ty);
                        let id = ID::owned(self.types[ty].id, self.sources.id_of(span));

                        let matcher = matcher!(Func = "function");
                        let handler = scope_error_handler(
                            self.diagnostics,
                            || TyError::FieldNotFound { ty, loc: span },
                            span,
                            "field",
                            matcher,
                        );
                        let Ok(field_id) = self.scope.get_concrete::<TyComp>(id).map_err(handler) else {
                            continue;
                        };

                        let field_ty = self.subtype(ty, self.ty_comps[field_id].ty);

                        let i = self.ty_comps[field_id].index;

                        ((field_id, field_ty), pattern, i as usize)
                    };

                    let field_access = {
                        let kind = TirKind::FieldAccess(low_expr, field_id);
                        let ent = TirEnt::new(kind, field_ty, span);
                        let field = self.tir_data.ents.push(ent);
                        if pointer {
                            self.take_ptr(field, mutable)
                        } else {
                            field
                        }
                    };

                    // push skipped fields
                    for filed in &self.ty_comps.get(fields)[last_i..i] {
                        let range = self.types.range_of(filed.ty, self.ty_comps);
                        let data = PatternLevelData {
                            range,
                            coverage: range,
                            depth,

                            ..default()
                        };
                        branch.push(data);
                    }
                    last_i = i + 1;

                    drop(self.pattern(field, field_access, branch, depth + 1, base_expr_span));
                }

                for filed in &self.ty_comps.get(fields)[last_i..] {
                    let range = self.types.range_of(filed.ty, self.ty_comps);
                    let data = PatternLevelData {
                        range,
                        coverage: range,
                        depth,

                        ..default()
                    };
                    branch.push(data);
                }
            }

            AstKind::Path => drop(self.path_match(expr, ast, branch, depth)?),

            AstKind::Ident => {
                let id = self.sources.id_of(span);
                let range = self
                    .types
                    .range_of(self.tir_data.ents[expr].ty, self.ty_comps);
                let value = if id == ID::new("_") {
                    TirPatternMeta::Default
                } else {
                    TirPatternMeta::Var(span)
                };
                let data = PatternLevelData {
                    meta: expr,
                    range,
                    coverage: range,
                    depth,
                    value,
                };
                branch.push(data);
            }
            AstKind::Int | AstKind::Bool | AstKind::Char => {
                let tir = self.expr(ast, None)?;
                let ty = self.tir_data.ents[expr].ty;
                self.infer_tir_ty(tir, ty, |_, got, loc| TyError::PatternTypeMismatch {
                    loc,
                    got,
                    expected: ty,
                    because: base_expr_span,
                })?;

                let data = {
                    let coverage = match self.tir_data.ents[tir].kind {
                        TirKind::IntLit(value) => PatternRange {
                            start: value,
                            end: value,
                        },
                        TirKind::BoolLit(value) => PatternRange::new(value..value),
                        TirKind::CharLit(value) => PatternRange::new(value..value),
                        _ => unreachable!(),
                    };
                    let range = self.types.range_of(ty, self.ty_comps);
                    PatternLevelData {
                        coverage,
                        range,
                        depth,
                        meta: expr,
                        value: TirPatternMeta::Cmp,
                    }
                };

                branch.push(data);
            }

            kind => unimplemented!("{kind:?}"),
        }

        Ok(())
    }

    fn path_match(
        &mut self,
        expr: Tir,
        ast: Ast,
        branch: &mut Vec<PatternLevelData<Tir, TirPatternMeta>>,
        depth: u32,
    ) -> errors::Result<Tir> {
        let TirEnt { ty: expr_ty, .. } = self.tir_data.ents[expr];
        let id = ident_hasher!(self).ident_id(ast, None)?;
        let span = self.ast_data.nodes[ast].span;

        if let TyEnt {
            kind: TyKind::Enum(discriminant_ty, ..),
            id: enum_id,
            ..
        } = self.types[expr_ty]
        {
            let id = ID::owned(enum_id, id);
            let matcher = matcher!(Func = "function");
            let handler = scope_error_handler(
                self.diagnostics,
                || TyError::EnumVariantNotFound {
                    ty: expr_ty,
                    loc: span,
                },
                span,
                "enum-variant",
                matcher,
            );
            let variant = self.scope.get_concrete::<TyComp>(id).map_err(handler)?;
            let TyCompEnt {
                ty: variant, index, ..
            } = self.ty_comps[variant];

            {
                let lit_value = index - 1;

                let flag_field = {
                    let (field, _) = self.get_field(expr_ty, 0, span)?;
                    let kind = TirKind::FieldAccess(expr, field);
                    let ent = TirEnt::new(kind, discriminant_ty, span);
                    self.tir_data.ents.push(ent)
                };

                let data = PatternLevelData {
                    meta: flag_field,
                    coverage: PatternRange::new(lit_value..lit_value),
                    depth,
                    range: self.types.range_of(expr_ty, self.ty_comps),
                    value: TirPatternMeta::Cmp,
                };

                branch.push(data);
            }

            let value = {
                let (field, _) = self.get_field(expr_ty, 1, span)?;
                let kind = TirKind::FieldAccess(expr, field);
                let ent = TirEnt::new(kind, variant, span);
                self.tir_data.ents.push(ent)
            };

            Ok(value)
        } else {
            let data = PatternLevelData { depth, ..default() };

            branch.push(data);

            Ok(expr)
        }
    }

    fn bit_cast(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[expr, ty] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let expr = self.expr(expr, None);
        let Some(ty) = self.parse_type_optional(ty)?.or(expected) else {
            todo!();
        };

        let expr = expr?; // recovery
        if self.is_terminating(expr) {
            return Ok(expr);
        }

        let kind = TirKind::BitCast(expr);
        let ent = TirEnt::new(kind, ty, span);
        Ok(self.tir_data.ents.push(ent))
    }

    fn deref(&mut self, ast: Ast) -> errors::Result<Tir> {
        let expr = self.ast_data.children(ast)[0];
        let target = self.expr(expr, None)?;
        let ty = self.tir_data.ents[target].ty;

        if !matches!(self.types[ty].kind, TyKind::Ptr(..)) {
            self.diagnostics.push(TyError::NonPointerDereference {
                loc: self.ast_data.nodes[ast].span,
                ty,
            });
            return Err(());
        }

        Ok(self.deref_ptr(target))
    }

    fn char(&mut self, ast: Ast) -> errors::Result<Tir> {
        let literal_value = char_value(self.sources, self.ast_data.nodes[ast].span).unwrap();
        let span = self.ast_data.nodes[ast].span;
        let ent = TirEnt::new(
            TirKind::CharLit(literal_value),
            self.builtin_types.char,
            span,
        );
        Ok(self.tir_data.ents.push(ent))
    }

    fn unary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[op, expr] = self.ast_data.children(ast) else {
            unreachable!()
        };

        let expr = self.expr(expr, None)?;
        if self.is_terminating(expr) {
            return Ok(expr);
        }
        let operand_ty = self.tir_data.ents[expr].ty;

        let func = {
            let span = self.ast_data.nodes[op].span;
            let id = {
                let ty = self.types.ptr_leaf_id_of(operand_ty);

                let op = self.sources.id_of(span);

                ID::unary(ty, op)
            };

            let matcher = matcher!();
            let handler = scope_error_handler(
                self.diagnostics,
                not_found_handler(span),
                span,
                "operator",
                matcher,
            );
            self.scope.get_concrete::<Func>(id).map_err(handler)?
        };

        let result = {
            let ty = self.funcs[func.meta()].sig.ret;
            let span = self.ast_data.nodes[ast].span;
            let args = self.tir_data.cons.push(&[expr]);
            let kind = TirKind::Call(operand_ty.into(), TyList::reserved_value(), func, args);
            let ent = TirEnt::new(kind, ty, span);
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn r#break(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[label, value] = self.ast_data.children(ast) else {
            unreachable!();
        };

        

        let (loop_expr, _) = self.find_loop(label, false)?;

        let TirEnt { ref mut flags, ty, kind: TirKind::LoopInProgress(ret, ref mut infinite), .. } = self.tir_data.ents[loop_expr] else {
            unreachable!("{:?}", self.tir_data.ents[loop_expr].kind);
        };
        flags.remove(TirFlags::TERMINATING);
        *infinite = false;

        let ty = (ty != self.builtin_types.nothing)
            .then_some(ty)
            .or_else(|| ret.map(|ret| self.tir_data.ents[ret].ty));

        let value = if value.is_reserved_value() {
            None
        } else {
            Some(self.expr(value, ty)?)
        };

        let TirKind::LoopInProgress(ret, ..) = &mut self.tir_data.ents[loop_expr].kind else {
            unreachable!("{:?}", self.tir_data.ents[loop_expr].kind);
        };

        if let Some(ret) = ret.expand() {
            let TirEnt {
                span: ret_span, ty, ..
            } = self.tir_data.ents[ret];
            if let Some(value) = value {
                self.infer_tir_ty(value, ty, |_, got, loc| TyError::BreakValueTypeMismatch {
                    because: ret_span,
                    expected: ty,
                    got,
                    loc,
                })?;
            } else {
                self.diagnostics.push(TyError::MissingBreakValue {
                    because: ret_span,
                    expected: ty,
                    loc: span,
                });
                return Err(());
            }
        } else {
            *ret = value.into();
        }

        let result = {
            let kind = TirKind::Break(loop_expr, value.into(), default());
            let ent = TirEnt::with_flags(
                kind,
                self.builtin_types.nothing,
                TirFlags::TERMINATING,
                span,
            );
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn r#loop(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[label, body_ast] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let loop_slot = {
            let kind = TirKind::LoopInProgress(None.into(), true);
            let ent = TirEnt::new(kind, expected.unwrap_or(self.builtin_types.nothing), span);
            self.tir_data.ents.push(ent)
        };

        // break and continue will propagate side effects so we save the lookup

        let id = if label.is_reserved_value() {
            ID::reserved_value()
        } else {
            ast::id_of(label, self.ast_data, self.sources)
        };

        self.scope_context.loops.push((loop_slot, id));
        let block = self.block(body_ast, None)?;
        self.scope_context.loops.pop().unwrap();

        {
            let TirEnt { flags, kind: TirKind::LoopInProgress(ret, infinite), .. } = self.tir_data.ents[loop_slot] else {
                unreachable!();
            };
            let ty = ret
                .map(|ret| self.tir_data.ents[ret].ty)
                .unwrap_or(self.builtin_types.nothing);
            let flags = (TirFlags::TERMINATING & infinite) | (flags & !TirFlags::TERMINATING);
            let kind = TirKind::Loop(block);
            self.tir_data.ents[loop_slot] = TirEnt {
                kind,
                ty,
                flags,
                span,
            };
        }

        Ok(loop_slot)
    }

    fn direct_call(
        &mut self,
        func: Func,
        ast: Ast,
        caller: Option<Ty>,
        mut obj: Option<Tir>,
        instantiation: Option<Ast>,
        expected: Expected,
        span: Span,
    ) -> errors::Result<Tir> {
        let Sig { args, ret: ty, .. } = self.funcs[func.meta()].sig;
        let generic = self.funcs[func].flags.contains(FuncFlags::GENERIC);
        let mut args = self
            .vec_pool
            .alloc_iter(self.ty_comps.get(args).iter().map(|arg| arg.ty));
        let origin = self.funcs[func.meta()].name;
        let (terminal_flags, tir_args) = self.arguments(ast, &mut obj, &args, Ok(origin))?;

        let (params, ty) = if generic {
            self.instantiate_call(caller, func, instantiation, &tir_args, expected, span)?
        } else {
            tir_args
                .iter()
                .zip(args.drain(..))
                .map(|(&arg, ty)| {
                    self.infer_tir_ty(arg, ty, |s, got, _| {
                        let span = s.tir_data.ents[arg].span;
                        todo!(
                            "{} != {} {}",
                            ty_display!(s, got),
                            ty_display!(s, ty),
                            span.log(s.sources)
                        );
                    })
                })
                .fold(Ok(()), |acc, err| acc.and(err))?;
            (TyList::reserved_value(), ty)
        };

        let args = self.tir_data.cons.push(&tir_args);
        let kind = TirKind::Call(caller.into(), params, func, args);
        let flags = terminal_flags | (TirFlags::GENERIC & generic);
        let ent = TirEnt {
            kind,
            ty,
            flags,
            span,
        };
        Ok(self.tir_data.ents.push(ent))
    }

    fn instantiate_call(
        &mut self,
        caller: Option<Ty>,
        func: Func,
        instantiation: Option<Ast>,
        tir_args: &[Tir],
        expected: Expected,
        _span: Span,
    ) -> errors::Result<(TyList, Ty)> {
        let FuncMeta {
            params,
            sig: Sig { args, ret, .. },
            name,
            ..
        } = self.funcs[func.meta()];
        let param_slice = self.ty_lists.get(params);

        let mut param_slots = vec![None; param_slice.len()];

        if let Some(instantiation) = instantiation {
            if param_slice.len() > self.ast_data.children(instantiation).len() - 1 {
                self.diagnostics.push(TyError::ExplicitParamOverflow {
                    because: name,
                    expected: param_slice.len(),
                    got: self.ast_data.children(instantiation).len() - 1,
                    loc: self.ast_data.nodes[instantiation].span,
                });

                return Err(());
            } else {
                for (i, &param) in self.ast_data.children(instantiation)[1..]
                    .iter()
                    .enumerate()
                {
                    let Ok(ty) = self.parse_type_optional(param) else {
                        continue;
                    };
                    param_slots[i] = ty;
                }
            }
        }

        for (&arg, &arg_comp) in tir_args.iter().zip(self.ty_comps.get(args)) {
            let TirEnt { ty, span, .. } = self.tir_data.ents[arg];
            if let Err(err) = bound_checker!(self).infer_parameters(
                ty,
                arg_comp.ty,
                &mut param_slots,
                self.ty_lists.get(params),
                span,
                true,
            ) {
                if let Some(err) = err {
                    self.diagnostics.push(err);
                } else {
                    return Err(());
                }
            }
        }

        if let Some(ty) = expected {
            drop(bound_checker!(self).infer_parameters(
                ty, 
                ret,
                &mut param_slots,
                self.ty_lists.get(params),
                Default::default(),
                true,
            ));
        }

        if let (Some(ty), Some(param @ &mut None)) = (caller, param_slots.last_mut()) {
            *param = Some(ty);
        }

        let param_slice = self.vec_pool.alloc(self.ty_lists.get(params));
        for (_i, &p) in param_slots.iter().enumerate() {
            let Some(p) = p else {
                todo!("{}", _span.log(self.sources));
            };

            self.scope_context.use_type(p, self.types);
        }

        let param_slots = self
            .vec_pool
            .alloc_iter(param_slots.drain(..).map(Option::unwrap));
        param_slots
            .iter()
            .for_each(|&p| self.scope_context.use_type(p, self.types));
        let ret = self.instantiate(ret, param_slots.as_slice(), param_slice.as_slice());
        Ok((self.ty_lists.push(&param_slots), ret))
    }

    fn indirect_call(&mut self, value: Tir, ast: Ast, span: Span) -> errors::Result<Tir> {
        let TirEnt { ty, .. } = self.tir_data.ents[value];

        let TyKind::FuncPtr(Sig { args, ret: ty, .. }) = self.types[ty].kind else {
            self.diagnostics.push(TyError::CallNonFunction {
                ty,
                loc: span,
            });
            return Err(());
        };

        let mut arg_tys = self
            .vec_pool
            .alloc_iter(self.ty_comps.get(args).iter().map(|arg| arg.ty));

        let (terminal_flags, tir_args) = self.arguments(ast, &mut None, &arg_tys, Err(ty))?;

        tir_args
            .iter()
            .zip(arg_tys.drain(..))
            .map(|(&arg, ty)| self.infer_tir_ty(arg, ty, |_, _, _| todo!()))
            .fold(Ok(()), |acc, err| acc.and(err))?;

        let args = self.tir_data.cons.push(&tir_args);
        let kind = TirKind::IndirectCall(value, args);
        let flags = terminal_flags;
        let ent = TirEnt {
            kind,
            ty,
            flags,
            span,
        };
        Ok(self.tir_data.ents.push(ent))
    }

    fn call(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let caller = self.ast_data.children(ast)[0];
        let AstEnt { kind, span, .. } = self.ast_data.nodes[caller];

        if kind == AstKind::DotExpr {
            let &[lhs, rhs] = self.ast_data.children(caller) else {
                unreachable!();
            };

            let obj = self.expr(lhs, None)?;
            let TirEnt { ty, span, .. } = self.tir_data.ents[obj];
            let ty = self.types.ptr_leaf_of(ty);
            let caller = self.types.caller_of(ty);

            let (rhs, instantiation) = if self.ast_data.nodes[rhs].kind == AstKind::Instantiation {
                (self.ast_data.children(rhs)[0], Some(rhs))
            } else {
                (rhs, None)
            };

            let id = ident_hasher!(self).ident_id(rhs, Some((caller, span)))?;

            let matcher = matcher!();
            self.scope
                .get(id)
                .and_then(|field_or_method| {
                    if let Some(field) = field_or_method.pointer.may_read::<TyComp>() {
                        let field_ty = self.subtype(ty, self.ty_comps[field].ty);
                        let field = {
                            let kind = TirKind::FieldAccess(obj, field);
                            let ent = TirEnt::new(kind, field_ty, span);
                            self.tir_data.ents.push(ent)
                        };
                        self.indirect_call(field, ast, span)
                            .map_err(|_| ScopeFindError::Other)
                    } else if let Some(method) = field_or_method.pointer.may_read::<Func>() {
                        self.direct_call(method, ast, Some(ty), Some(obj), instantiation, expected, span)
                            .map_err(|_| ScopeFindError::Other)
                    } else {
                        Err(ScopeFindError::InvalidType(field_or_method.pointer.id))
                    }
                })
                .map_err(scope_error_handler(
                    self.diagnostics,
                    not_found_handler(span),
                    span,
                    "field method",
                    matcher,
                ))
        } else {
            let (caller, instantiation) =
                if self.ast_data.nodes[caller].kind == AstKind::Instantiation {
                    (self.ast_data.children(caller)[0], Some(caller))
                } else {
                    (caller, None)
                };

            let (id, caller) = ident_hasher!(self).ident_id_low(caller, None)?;
            let func = self.symbol_low(id, span, false)?;
            if let TirKind::FuncPtr(func) = func.kind {
                self.direct_call(
                    func,
                    ast,
                    caller.map(|(ty, _)| ty),
                    None,
                    instantiation,
                    expected,
                    span,
                )
            } else {
                let func = self.tir_data.ents.push(func);
                self.indirect_call(func, ast, span)
            }
        }
    }

    fn arguments(
        &mut self,
        ast: Ast,
        obj: &mut Option<Tir>,
        arg_tys: &[Ty],
        origin: std::result::Result<Span, Ty>,
    ) -> errors::Result<(TirFlags, PoolVec<Tir>)> {
        if let (Some(obj), Some(&expected)) = (obj.as_mut(), arg_tys.first()) {
            *obj = self.ptr_correct(*obj, expected);
        }

        let mut vec = self
            .vec_pool
            .with_capacity(arg_tys.len() + obj.is_some() as usize);
        if let &mut Some(obj) = obj {
            vec.push(obj);
        }
        let mut terminal_flags = TirFlags::empty();
        for (&arg, &ty) in self.ast_data.children(ast)[1..].iter().zip(arg_tys) {
            let Ok(arg) = self.expr(arg, Some(ty)) else {
                continue;
            };
            terminal_flags |= self.terminal_flags(arg);
            vec.push(arg);
            if !terminal_flags.is_empty() {
                break;
            }
        }

        if arg_tys.len() != vec.len() {
            self.diagnostics.push(TyError::CallArgCountMismatch {
                because: origin,
                expected: arg_tys.len(),
                got: vec.len(),
                loc: self.ast_data.nodes[ast].span,
            });
            return Err(());
        }

        Ok((terminal_flags, vec))
    }

    fn ptr_correct(&mut self, mut target: Tir, expected: Ty) -> Tir {
        let ty = self.tir_data.ents[target].ty;

        let target_depth = self.types.depth_of(ty);
        let expected_depth = self.types.depth_of(expected);

        for _ in expected_depth..target_depth {
            target = self.deref_ptr(target);
        }

        let mutable = self.types[expected].flags.contains(TyFlags::MUTABLE);

        for _ in target_depth..expected_depth {
            target = self.take_ptr(target, mutable);
        }

        target
    }

    fn take_ptr(&mut self, target: Tir, mutable: bool) -> Tir {
        if self.is_terminating(target) {
            return target;
        }

        let TirEnt {
            ty,
            span,
            flags,
            kind,
            ..
        } = &mut self.tir_data.ents[target];
        let (span, ty) = (*span, *ty);

        if flags.contains(TirFlags::IMMUTABLE) && mutable {
            todo!();
        }

        if let &mut TirKind::Access(target, ..) = kind {
            self.tir_data.ents[target].flags.insert(TirFlags::SPILLED);
        } else {
            flags.insert(TirFlags::SPILLED);
        }

        let kind = TirKind::TakePtr(target);
        let ptr_ty = ty_factory!(self).pointer_of(ty, mutable);
        self.scope_context.use_type(ptr_ty, self.types);
        let ptr = TirEnt::new(kind, ptr_ty, span);
        self.tir_data.ents.push(ptr)
    }

    fn deref_ptr(&mut self, target: Tir) -> Tir {
        if self.is_terminating(target) {
            return target;
        }
        let span = self.tir_data.ents[target].span;
        let kind = TirKind::DerefPtr(target);
        let ty = self.tir_data.ents[target].ty;
        let deref_ty = self.types.deref(ty);
        self.scope_context.use_type(deref_ty, self.types);
        let assignable = self.types[ty].flags.contains(TyFlags::MUTABLE);
        let flags = TirFlags::IMMUTABLE & !assignable;
        let deref = TirEnt::with_flags(kind, deref_ty, flags, span);
        self.tir_data.ents.push(deref)
    }

    fn dot_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[header, field] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let mut header = self.expr(header, None)?;
        if self.is_terminating(header) {
            return Ok(header);
        }

        let ty = self.tir_data.ents[header].ty;
        let base_ty = self.types.ptr_leaf_of(ty);

        let span = self.ast_data.nodes[field].span;
        let (field_id, field_ty) = {
            let id = self.sources.id_of(span);
            let caller_ty = self.types.caller_of(base_ty);
            let field_id = ID::owned(self.types[caller_ty].id, id);
            let matcher = matcher!(Func = "method");
            let handler = scope_error_handler(
                self.diagnostics,
                || TyError::FieldNotFound { ty, loc: span },
                span,
                "field",
                matcher,
            );
            let field_id = self
                .scope
                .get_concrete::<TyComp>(field_id)
                .map_err(handler)?;
            let field_ty = self.subtype(base_ty, self.ty_comps[field_id].ty);
            (field_id, field_ty)
        };

        while base_ty != self.tir_data.ents[header].ty {
            header = self.deref_ptr(header);
        }

        let result = {
            let header_flags = self.tir_data.ents[header].flags;
            let flags = header_flags & TirFlags::IMMUTABLE;
            let kind = TirKind::FieldAccess(header, field_id);
            let ent = TirEnt::with_flags(kind, field_ty, flags, span);
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn bool(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let ty = self.builtin_types.bool;
        let value = {
            let str = self.sources.display(span);
            str == "true"
        };
        let kind = TirKind::BoolLit(value);
        let ent = TirEnt::new(kind, ty, span);
        let result = self.tir_data.ents.push(ent);
        Ok(result)
    }

    fn constructor(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let &[name, body] = self.ast_data.children(ast) else {
            unreachable!("{:?}", self.ast_data.children(ast));
        };

        let name_span = self.ast_data.nodes[name].span;
        if let Some(ty) = expected {
            if name.is_reserved_value() {
                return self.constructor_low(ty, body);
            } else if let &[ph, name] = self.ast_data.children(name) && ph.is_reserved_value() {
                let id = ident_hasher!(self).ident_id(name, Some((ty, name_span)))?;
                return self.construct_enum(ty, id, body, name_span);
            }
        }

        self.construct_non_inferred(name, body)
    }

    fn construct_non_inferred(&mut self, name: Ast, body: Ast) -> errors::Result<Tir> {
        let (id, owner) = ident_hasher!(self).ident_id_low(name, None)?;
        if let Some((enum_ty, span)) = owner {
            self.construct_enum(enum_ty, id, body, span)
        } else {
            let span = self.ast_data.nodes[name].span;
            let matcher = matcher!(
                Func = "function",
                Global = "global-value",
                Tir = "local-value"
            );
            let handler = scope_error_handler(
                self.diagnostics,
                not_found_handler(span),
                span,
                "constructor",
                matcher,
            );
            let ty = self.scope.get_concrete::<Ty>(id).map_err(handler)?;
            self.constructor_low(ty, body)
        }
    }

    fn construct_enum(&mut self, enum_ty: Ty, id: ID, body: Ast, span: Span) -> errors::Result<Tir> {
        let matcher = matcher!(Func = "method");
        let handler = scope_error_handler(
            self.diagnostics,
            || TyError::EnumVariantNotFound {
                ty: enum_ty,
                loc: span,
            },
            span,
            "enum-variant",
            matcher,
        );
        let variant = self.scope.get_concrete::<TyComp>(id).map_err(handler)?;
        let TyCompEnt { ty, index, .. } = self.ty_comps[variant];
        let TyKind::Enum(discriminant, _) = self.types[enum_ty].kind else {
            unreachable!();
        };
        let value = self.constructor_low(ty, body)?;
        let flag = self.int_lit(index as u128 - 1, discriminant);

        let args = self.tir_data.cons.push(&[flag, value]);
        let kind = TirKind::Constructor(args);
        let flags = TirFlags::TERMINATING & self.is_terminating(value);
        let ent = TirEnt::with_flags(kind, enum_ty, flags, span);

        Ok(self.tir_data.ents.push(ent))
    }

    fn constructor_low(&mut self, ty: Ty, body: Ast) -> errors::Result<Tir> {
        let AstEnt { span, kind, .. } = self.ast_data.nodes[body];
        let TyEnt { flags, .. } = self.types[ty];

        let (mut param_slots, ty) = if let TyKind::Instance(base, params) = self.types[ty].kind {
            (
                self.vec_pool
                    .alloc_iter(self.ty_lists.get(params).iter().copied().map(Some)),
                base,
            )
        } else {
            (
                self.vec_pool
                    .of_size(Option::<Ty>::None, flags.param_count()),
                ty,
            )
        };

        let TyKind::Struct(fields) = self.types[ty].kind else {
            self.diagnostics.push(TyError::ExpectedStruct {
                got: ty,
                loc: span,
            });
            return Err(());
        };

        let mut initial_values = vec![Tir::reserved_value(); self.ty_comps.get(fields).len()];

        // TODO: maybe avoid allocation
        let mut terminal_flags = TirFlags::empty();
        for (i, &field) in self.ast_data.children(body).iter().enumerate() {
            let (expr, (field, field_ty)) = if kind == AstKind::ConstructorBody {
                let &[name, expr] = self.ast_data.children(field) else {
                    unreachable!();
                };

                let span = self.ast_data.nodes[name].span;
                let id = self.sources.id_of(span);
                let field_id = ID::owned(self.types[ty].id, id);
                let matcher = matcher!(Func = "method");
                let handler = scope_error_handler(
                    self.diagnostics,
                    || TyError::FieldNotFound { ty, loc: span },
                    span,
                    "field",
                    matcher,
                );
                let field_id = self
                    .scope
                    .get_concrete::<TyComp>(field_id)
                    .map_err(handler)?;
                // we don't want to register the subtype in used types since struct type might
                // not be inferred yet and we kow that the type that initialized this field exists.
                let field_ty = ty_factory!(self).subtype(ty, self.ty_comps[field_id].ty);
                (expr, (field_id, field_ty))
            } else {
                let span = self.ast_data.nodes[field].span;
                let Ok(res) = self.get_field(ty, i, span) else {
                    continue;
                };
                (field, res)
            };

            let TyCompEnt {
                name: hint, index, ..
            } = self.ty_comps[field];

            let Ok(value) = (match self.ast_data.nodes[expr].kind {
                AstKind::ConstructorBody | AstKind::TupleConstructorBody => self.constructor_low(field_ty, expr),
                _ => self.expr(expr, Some(field_ty)),
            }) else {
                continue;
            };

            terminal_flags |= self.terminal_flags(value);
            if !terminal_flags.is_empty() {
                break;
            }

            if self.types[field_ty].flags.contains(TyFlags::GENERIC) {
                let TirEnt {
                    ty: parametrized_field_ty,
                    span,
                    ..
                } = self.tir_data.ents[value];
                let subs = collect_ty_params(ty, self.types, self.vec_pool, self.builtin_types);
                if let Err(err) = bound_checker!(self).infer_parameters(
                    parametrized_field_ty,
                    field_ty,
                    &mut param_slots,
                    subs.as_slice(),
                    span,
                    true,
                ) {
                    if let Some(err) = err {
                        self.diagnostics.push(err);
                    } else {
                        return Err(());
                    }
                }
            } else {
                // we ignore this to report more errors
                drop(self.infer_tir_ty(value, field_ty, |_, got, loc| {
                    TyError::ConstructorFieldTypeMismatch {
                        because: hint,
                        expected: field_ty,
                        got,
                        loc,
                    }
                }));
            }

            initial_values[index as usize] = value;
        }

        if initial_values.iter().any(Tir::is_reserved_value) {
            let missing = initial_values
                .iter()
                .enumerate()
                .filter_map(|(index, &value)| (value == Tir::reserved_value()).then_some(index))
                .map(|i| self.ty_comps.get(fields)[i].name)
                .collect::<Vec<_>>();

            self.diagnostics.push(TyError::ConstructorMissingFields {
                on: ty,
                missing,
                loc: span,
            });

            return Err(());
        }

        let span = self.ast_data.nodes[body].span;

        let ty = if param_slots.len() > 0 {
            param_slots
                .iter()
                .enumerate()
                .filter_map(|(i, ty)| ty.is_none().then_some(i))
                // OK if no cycles performed
                .fold(Ok(()), |_, param| {
                    self.diagnostics.push(TyError::UnknownGenericTypeParam {
                        ty: self.types[ty].name,
                        param,
                        loc: span,
                    });
                    Err(())
                })?;

            let param_slots = self
                .vec_pool
                .alloc_iter(param_slots.drain(..).map(Option::unwrap));
            param_slots
                .iter()
                .for_each(|&p| self.scope_context.use_type(p, self.types));
            let subs = collect_ty_params(ty, self.types, self.vec_pool, self.builtin_types);
            let ty = self.instantiate(ty, param_slots.as_slice(), subs.as_slice());
            ty
        } else {
            ty
        };

        let result = {
            let fields = self.tir_data.cons.push(&initial_values);
            let kind = TirKind::Constructor(fields);
            let ent = TirEnt::new(kind, ty, span);
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn variable(&mut self, mutable: bool, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[name, ty, value] = self.ast_data.children(ast) else {
            unreachable!();
        };

        let id = {
            let span = self.ast_data.nodes[name].span;
            let str = self.sources.display(span);
            ID::new(str)
        };

        let ty = if ty.is_reserved_value() {
            None
        } else {
            Some(self.parse_type(ty)?)
        };

        let value = if value.is_reserved_value() {
            let kind = TirKind::Uninit;
            let ent = TirEnt::new(kind, ty.unwrap(), span);
            self.tir_data.ents.push(ent)
        } else {
            let value = self.expr(value, ty)?;
            if self.is_terminating(value) {
                return Ok(value);
            }
            value
        };
        
        self.tir_data.ents[value]
            .flags
            .insert(TirFlags::IMMUTABLE & !mutable);

        let result = {
            let kind = TirKind::Variable(value);
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.tir_data.ents.push(ent)
        };

        {
            let item = ScopeItem::new(result, span);
            self.scope.push_item(id, item);
        }

        Ok(result)
    }

    fn int(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let ty = {
            let str = self.sources.display(span);
            if str.ends_with("u") {
                self.builtin_types.uint
            } else {
                self.builtin_types
                    .integers()
                    .into_iter()
                    .find(|&ty| str.ends_with(self.sources.display(self.types[ty].name)))
                    .unwrap_or_else(|| if let Some(ty) = expected {
                        ty
                    } else {
                        self.builtin_types.int
                    })
            }
        };
        let signed = self.types[ty].flags.contains(TyFlags::SIGNED);
        let literal_value = int_value(self.sources, span, signed);
        let kind = TirKind::IntLit(literal_value);
        let ent = TirEnt::new(kind, ty, span);
        Ok(self.tir_data.ents.push(ent))
    }

    fn r#return(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[value] = self.ast_data.children(ast) else {
            unreachable!()
        };

        let ret = self.funcs[self.func.meta()].sig.ret;

        let value = if value.is_reserved_value() {
            self.infer_ty(self.builtin_types.nothing, ret, |s| {
                TyError::UnexpectedReturnValue {
                    because: s.funcs[s.func.meta()].name,
                    loc: span,
                }
            })?;
            None
        } else {
            let value = self.expr(value, Some(ret))?;
            if self.is_terminating(value) {
                return Ok(value);
            }
            self.infer_tir_ty(value, ret, |s, got, loc| {
                let &[.., ret_ast, _] = s.ast_data.children(s.scope_context.func_ast[s.func]) else {
                    unreachable!();
                };
                let ret_span =
                    (!ret_ast.is_reserved_value()).then(|| s.ast_data.nodes[ret_ast].span);
                TyError::ReturnTypeMismatch {
                    because: ret_span,
                    expected: ret,
                    got,
                    loc,
                }
            })?;
            Some(value)
        };

        for &(loop_header, _) in self.scope_context.loops.iter() {
            self.tir_data.ents[loop_header]
                .flags
                .insert(TirFlags::TERMINATING);
        }

        let result = {
            let kind = TirKind::Return(value.into(), default());
            let ent = TirEnt::with_flags(
                kind,
                self.builtin_types.nothing,
                TirFlags::TERMINATING,
                span,
            );
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn r#if(&mut self, ast: Ast, expected: Expected) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[cond, then, otherwise] = self.ast_data.children(ast) else {
            unreachable!()
        };

        let cond = self.expr(cond, Some(self.builtin_types.bool))?;
        drop(
            self.infer_tir_ty(cond, self.builtin_types.bool, |_, got, loc| {
                TyError::IfConditionTypeMismatch { got, loc }
            }),
        );

        let then = self.block(then, expected);
        let otherwise = if otherwise.is_reserved_value() {
            let kind = TirKind::Block(default(), default());
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.tir_data.ents.push(ent)
        } else {
            self.block(otherwise, expected)?
        };
        let then = then?;

        let ty = {
            let then = self.tir_data.ents[then].ty;
            let otherwise = self.tir_data.ents[otherwise].ty;
            if then == otherwise {
                then
            } else {
                self.builtin_types.nothing
            }
        };

        let flags = {
            let ents = &self.tir_data.ents;
            let then_flags = ents[then].flags;
            let otherwise_flags = ents[otherwise].flags;
            (then_flags & otherwise_flags) & TirFlags::TERMINATING
        };

        let result = {
            let kind = TirKind::If(cond, then, otherwise, default());
            let ent = TirEnt::with_flags(kind, ty, flags, span);
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn symbol(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let id = ident_hasher!(self).ident_id(ast, None)?;
        let ent = self.symbol_low(id, span, true)?;
        Ok(self.tir_data.ents.push(ent))
    }

    fn symbol_low(&mut self, id: ID, span: Span, func_ptr_ty: bool) -> errors::Result<TirEnt> {
        let matcher = matcher!(Ty = "type");
        self.scope
            .get(id)
            .and_then(|value| {
                if let Some(var) = value.pointer.may_read::<Tir>() {
                    let (var, local) = match self.tir_data.ents[var].kind {
                        TirKind::Variable(local) => (Some(var), local),
                        TirKind::Argument(..) => (None, var),
                        kind => unimplemented!("{:?}", kind),
                    };

                    let mut copy = self.tir_data.ents[local];
                    copy.kind = TirKind::Access(local, var.into());
                    copy.span = span;
                    Ok(copy)
                } else if let Some(global) = value.pointer.may_read::<Global>() {
                    let kind = TirKind::GlobalAccess(global);
                    let GlobalEnt { ty, mutable, .. } = self.globals[global];
                    let flags = TirFlags::IMMUTABLE & !mutable;
                    Ok(TirEnt::with_flags(kind, ty, flags, span))
                } else if let Some(func) = value.pointer.may_read::<Func>() {
                    let kind = TirKind::FuncPtr(func);
                    let ty = if func_ptr_ty {
                        let sig = self.funcs[func.meta()].sig;
                        let generic = self.funcs[func].flags.contains(FuncFlags::GENERIC);
                        ty_factory!(self).func_pointer_of(sig, generic) // TODO: call use_type when it becomes relevant
                    } else {
                        self.builtin_types.nothing
                    };
                    self.scope_context.use_type(ty, self.types);
                    Ok(TirEnt::new(kind, ty, span))
                } else {
                    Err(ScopeFindError::InvalidType(value.pointer.id))
                }
            })
            .map_err(scope_error_handler(
                self.diagnostics,
                not_found_handler(span),
                span,
                "local-value global-value function-pointer",
                matcher,
            ))
    }

    fn binary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast_data.nodes[ast].span;
        let &[left, op, right] = self.ast_data.children(ast) else {
            unreachable!()
        };

        // we walk the ast even if something fails for better error diagnostics
        let (left, right) = {
            let left = self.expr(left, None);
            let right = self.expr(right, None);
            (left?, right?)
        };

        let mut terminal_flags = self.terminal_flags(left);
        if terminal_flags.is_empty() {
            terminal_flags = self.terminal_flags(right);
        } else {
            return Ok(left);
        }

        let left_ty = self.tir_data.ents[left].ty;

        let op_span = self.ast_data.nodes[op].span;
        let id = {
            let op_id = {
                let str = self.sources.display(op_span);
                if str == "=" {
                    return self.assign(left, right, span);
                }

                ID::new(str)
            };

            let left_id = self.types[left_ty].id;

            ID::binary(left_id, op_id)
        };

        let matcher = matcher!(Ty = "type", Global = "global-value");
        let handler = scope_error_handler(
            self.diagnostics,
            not_found_handler(op_span),
            op_span,
            "operator",
            matcher,
        );
        let func = self.scope.get_concrete::<Func>(id).map_err(handler)?;

        /* sanity check */
        {
            let func_ent = &self.funcs[func.meta()];
            let args = self.ty_comps.get(func_ent.sig.args);

            let arg_count = args.len();
            if arg_count != 2 {
                self.diagnostics.push(TyError::OperatorArgCountMismatch {
                    because: func_ent.name,
                    expected: 2,
                    got: arg_count,
                    loc: op_span,
                });
                return Err(());
            }

            let expected = args[1].ty;
            self.infer_tir_ty(right, expected, |_, got, loc| TyError::BinaryTypeMismatch {
                expected,
                got,
                loc,
            })?;
        }

        let tir = {
            let ty = self.funcs[func.meta()].sig.ret;
            let args = self.tir_data.cons.push(&[left, right]);
            let kind = TirKind::Call(left_ty.into(), TyList::reserved_value(), func, args);
            let flags = TirFlags::TERMINATING & terminal_flags;
            let ent = TirEnt {
                kind,
                ty,
                flags,
                span,
            };
            self.tir_data.ents.push(ent)
        };

        Ok(tir)
    }

    fn assign(&mut self, left: Tir, right: Tir, span: Span) -> errors::Result<Tir> {
        if self.is_terminating(right) {
            return Ok(right);
        }

        let TirEnt {
            ty,
            flags,
            span: left_span,
            kind,
        } = self.tir_data.ents[left];

        if flags.contains(TirFlags::IMMUTABLE) {
            let because = if let TirKind::Access(here, ..) = kind {
                Some(self.tir_data.ents[here].span)
            } else {
                None
            };
            self.diagnostics.push(TyError::AssignToNonAssignable {
                because,
                loc: left_span,
            });
            return Err(());
        }

        self.infer_tir_ty(right, ty, |_, got, loc| TyError::AssignTypeMismatch {
            because: left_span,
            expected: ty,
            got,
            loc,
        })?;

        let result = {
            let kind = TirKind::Assign(left, right, default());
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.tir_data.ents.push(ent)
        };

        Ok(result)
    }

    fn get_field(&mut self, on: Ty, index: usize, span: Span) -> errors::Result<(TyComp, Ty)> {
        let base = if let TyKind::Instance(base, ..) = self.types[on].kind {
            base
        } else {
            on
        };

        let (TyKind::Struct(fields) | TyKind::Enum(.., fields)) = self.types[base].kind else {
            unreachable!("{:?}", self.types[base].kind);
        };

        let Some(field) = self.ty_comps.get(fields).get(index) else {
            self.diagnostics.push(TyError::UnregisteredFieldIndex {
                index,
                max: self.ty_comps.get(fields).len(),
                loc: span,
                on,
            });
            return Err(());
        };

        let ty = if let TyKind::Instance(base, params) = self.types[on].kind {
            let params = self.vec_pool.alloc(self.ty_lists.get(params));
            let subs = collect_ty_params(base, self.types, self.vec_pool, self.builtin_types);
            self.instantiate(field.ty, &params, subs.as_slice())
        } else {
            field.ty
        };

        Ok((
            TyComp::new(self.ty_comps.start_index_of(fields).unwrap() + index),
            ty,
        ))
    }

    fn subtype(&mut self, parent: Ty, child: Ty) -> Ty {
        let ty = ty_factory!(self).subtype(parent, child);
        self.scope_context.use_type(ty, self.types);
        ty
    }

    fn instantiate(&mut self, ty: Ty, params: &[Ty], subs: &[Ty]) -> Ty {
        let ty = ty_factory!(self).instantiate(ty, &params, subs);
        self.scope_context.use_type(ty, self.types);
        ty
    }

    fn parse_type(&mut self, ty: Ast) -> errors::Result<Ty> {
        let ty = ty_parser!(self).parse_type(ty)?;
        self.scope_context.use_type(ty, self.types);
        Ok(ty)
    }

    fn parse_type_optional(&mut self, ty: Ast) -> errors::Result<Option<Ty>> {
        let ty = ty_parser!(self).parse_type_optional(ty)?;
        if let Some(ty) = ty {
            self.scope_context.use_type(ty, self.types);
        }
        Ok(ty)
    }

    fn find_loop(&mut self, label: Ast, is_continue: bool) -> errors::Result<(Tir, bool)> {
        let id = if label.is_reserved_value() {
            ID::reserved_value()
        } else {
            ast::id_of(label, self.ast_data, self.sources)
        };

        self.scope_context
            .loops
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, &(tir, loop_id))| {
                (id == loop_id).then(|| {
                    if is_continue {
                        if let Some(&(tir, _)) = self.scope_context.loops.get(i + 1) {
                            self.tir_data.ents[tir]
                                .flags
                                .insert(TirFlags::TERMINATING | TirFlags::CONTINUE);
                        }
                    } else {
                        for &(tir, _) in &self.scope_context.loops[i + 1..] {
                            if let TirKind::LoopInProgress(.., false) = self.tir_data.ents[tir].kind
                            {
                                continue;
                            }

                            self.tir_data.ents[tir].flags.insert(TirFlags::TERMINATING);
                        }
                    }
                    (tir, i == self.scope_context.loops.len() - 1)
                })
            })
            .ok_or(())
            .map_err(|()| todo!("{}", self.ast_data.nodes[label].span.log(self.sources)))
    }

    fn is_terminating(&self, tir: Tir) -> bool {
        self.tir_data.is_terminating(tir)
    }

    pub fn terminal_flags(&self, expr: Tir) -> TirFlags {
        self.tir_data.terminal_flags(expr)
    }

    fn infer_tir_ty(
        &mut self,
        right: Tir,
        ty: Ty,
        err_fact: impl Fn(&mut Self, Ty, Span) -> TyError,
    ) -> errors::Result {
        let TirEnt { ty: got, span, .. } = self.tir_data.ents[right];
        self.infer_ty(ty, got, |s| err_fact(s, got, span))
    }

    fn infer_ty(
        &mut self,
        ty: Ty,
        got: Ty,
        err_fact: impl Fn(&mut Self) -> TyError,
    ) -> errors::Result {
        if ty != got {
            let error = err_fact(self);
            self.diagnostics.push(error);
            Err(())
        } else {
            Ok(())
        }
    }

    fn int_lit(&mut self, value: u128, ty: Ty) -> Tir {
        let kind = TirKind::IntLit(value);
        let ent = TirEnt::new(kind, ty, self.builtin_types.discriminant);
        self.tir_data.ents.push(ent)
    }
}
