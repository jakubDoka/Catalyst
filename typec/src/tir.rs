use std::ops::Not;

use incr::*;
use module_types::*;
use matching::*;

use crate::{scope::ScopeContext, TyError, *};

pub struct TirBuilder<'a> {
    pub func: Func,
    pub funcs: &'a Funcs,
    pub func_lists: &'a TFuncLists,
    pub ty_lists: &'a mut TyLists,
    pub instances: &'a mut Instances,
    pub ty_comps: &'a mut TyComps,
    pub ty_comp_lookup: &'a mut TyCompLookup,
    pub builtin_types: &'a BuiltinTypes,
    pub bound_impls: &'a mut BoundImpls,
    pub ctx: &'a mut ScopeContext,
    pub body: &'a mut TirData,
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub temp: &'a mut FramedStack<Tir>,
    pub sources: &'a Sources,
    pub ast: &'a AstData,
    pub modules: &'a mut Modules,
    pub diagnostics: &'a mut errors::Diagnostics,
    pub incr: &'a mut Incr,
    pub func_meta: &'a mut FuncMeta,
    pub pattern_graph: &'a mut PatternGraph<Tir, PatternMeta>,
}

#[macro_export]
macro_rules! tir_builder {
    ($self:expr, $func:expr) => {
        TirBuilder::new(
            $func,
            &mut $self.funcs,
            &mut $self.func_lists,
            &mut $self.ty_lists,
            &mut $self.instances,
            &mut $self.ty_comps,
            &mut $self.ty_comp_lookup,
            &$self.builtin_types,
            &mut $self.bound_impls,
            &mut $self.scope_context,
            &mut $self.tir_temp_body,
            &mut $self.scope,
            &mut $self.types,
            &mut $self.tir_temp,
            &$self.sources,
            &$self.ast,
            &mut $self.modules,
            &mut $self.diagnostics,
            &mut $self.incr,
            &mut $self.func_meta,
            &mut $self.pattern_graph,
        )
    };
}

impl<'a> TirBuilder<'a> {
    pub fn new(
        func: Func,
        funcs: &'a Funcs,
        func_lists: &'a TFuncLists,
        ty_lists: &'a mut TyLists,
        instances: &'a mut Instances,
        ty_comps: &'a mut TyComps,
        ty_comp_lookup: &'a mut TyCompLookup,
        builtin_types: &'a BuiltinTypes,
        bound_impls: &'a mut BoundImpls,
        ctx: &'a mut ScopeContext,
        body: &'a mut TirData,
        scope: &'a mut Scope,
        types: &'a mut Types,
        temp: &'a mut FramedStack<Tir>,
        sources: &'a Sources,
        ast: &'a AstData,
        modules: &'a mut Modules,
        diagnostics: &'a mut errors::Diagnostics,
        incr: &'a mut Incr,
        func_meta: &'a mut FuncMeta,
        pattern_graph: &'a mut PatternGraph<Tir, PatternMeta>,
    ) -> Self {
        TirBuilder {
            func,
            funcs,
            func_lists,
            ty_lists,
            instances,
            ty_comps,
            ty_comp_lookup,
            builtin_types,
            bound_impls,
            ctx,
            body,
            scope,
            types,
            temp,
            sources,
            ast,
            modules,
            diagnostics,
            incr,
            func_meta,
            pattern_graph,
        }
    }

    pub fn build(&mut self) -> errors::Result {
        let FuncMetaData {
            kind,
            sig: Sig {
                ret, params, args, ..
            },
            ..
        } = self.func_meta[self.func];
        let flags = self.funcs.ents[self.func].flags;

        //println!("{}", self.func_meta[self.func].name.log(self.sources));

        let ast = self.ctx.func_ast[self.func];
        let header = self.ast.children(ast);
        let &[generics, .., ret_ast, _] = header else {
            unreachable!();
        };
        let &body_ast = header.last().unwrap();

        if body_ast.is_reserved_value() {
            if flags.contains(FuncFlags::EXTERNAL) {
                self.func_meta[self.func].kind = FuncKind::External;
            }
            return Ok(());
        }

        self.scope.mark_frame();
        if let FuncKind::Owned(ty) = kind {
            let span = self.ast.nodes[ast].span;
            self.scope.push_item("Self", ScopeItem::new(ty, span));
        }

        self.build_generics(params, generics);

        let args = {
            let ast_args = &header[ast::FUNCTION_ARG_START..header.len() - ast::FUNCTION_ARG_END];
            let args = self.ty_lists.get(args);
            let mut tir_args = Vec::with_capacity(args.len());
            for (i, (&ast, &ty)) in ast_args.iter().zip(args).enumerate() {
                // println!("=== {} {} {:?}", ty_display!(self, ty), self.ast.nodes[ast].span.log(self.sources), self.types[ty].flags);
                self.ctx.use_type(ty, self.types);

                let &[name, ..] = self.ast.children(ast) else {
                    unreachable!();
                };
                let span = self.ast.nodes[name].span;

                let arg = {
                    let kind = TirKind::Argument(i as u32);
                    let ent = TirEnt::new(kind, ty, span);
                    self.body.ents.push(ent)
                };

                {
                    let id = {
                        let str = self.sources.display(span);
                        ID::new(str)
                    };
                    let item = ScopeItem::new(arg, span);
                    self.scope.push_item(id, item);
                }

                tir_args.push(arg);
            }
            self.body.cons.push(&tir_args)
        };

        let root = self.build_block(body_ast)?;
        self.func_meta[self.func].body = root;
        self.func_meta[self.func].args = args;

        self.body.used_types = self.ty_lists.push(&self.ctx.used_types);
        self.ctx.used_types.clear();
        self.ctx.used_types_set.clear();

        if !self.body.ents[root].flags.contains(TirFlags::TERMINATING)
            && ret != self.builtin_types.nothing
        {
            let because = ret_ast
                .is_reserved_value()
                .not()
                .then(|| self.ast.nodes[ret_ast].span);
            self.infer_tir_ty(root, ret, |_, got, loc| TyError::ReturnTypeMismatch {
                because,
                expected: ret,
                got,
                loc,
            })?;
        }

        self.scope.pop_frame();

        Ok(())
    }

    fn build_generics(&mut self, params: TyList, generics: Ast) {
        if generics.is_reserved_value() {
            return;
        }

        let ast = self.ast.children(generics);
        let bounds = self.ty_lists.get(params);
        for (&item, &bound_combo) in ast.iter().zip(bounds) {
            let name = self.ast.children(item)[0];
            let span = self.ast.nodes[name].span;
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
                    let name = self.func_meta[func].name;
                    let id = {
                        let id = self.sources.id_of(name);
                        let bound = self.types[bound_combo].id;
                        ID::owned_func(bound, id)
                    };
                    self.scope.push_item(id, ScopeItem::new(func, name));
                }
            }
        }
    }

    fn build_block(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;

        self.temp.mark_frame();
        self.scope.mark_frame();

        let mut final_expr = None;
        let mut terminating = false;
        for &stmt in self.ast.children(ast) {
            let Ok(expr) = self.build_expr(stmt) else {
                continue; // Recover here
            };
            self.temp.push(expr);
            final_expr = Some(expr);
            terminating |= self.body.ents[expr].flags.contains(TirFlags::TERMINATING);
            if terminating {
                break; // TODO: emit warning
            }
        }

        let result = {
            let ty = if let Some(expr) = final_expr {
                self.body.ents[expr].ty
            } else {
                self.builtin_types.nothing
            };

            let slice = self.temp.top_frame();
            let items = self.body.cons.push(slice);

            let kind = TirKind::Block(items);
            let flags = TirFlags::TERMINATING & terminating;
            let ent = TirEnt::with_flags(kind, ty, flags, span);
            self.body.ents.push(ent)
        };

        self.temp.pop_frame();
        self.scope.pop_frame();

        Ok(result)
    }

    fn build_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let ast::AstEnt { kind, span, .. } = self.ast.nodes[ast];
        let expr = match kind {
            AstKind::Unary => self.build_unary(ast)?,
            AstKind::Binary => self.build_binary(ast)?,
            AstKind::Ident => self.build_ident(ast)?,
            AstKind::If => self.build_if(ast)?,
            AstKind::Return => self.build_return(ast)?,
            AstKind::Int => self.build_int(ast)?,
            AstKind::Bool => self.build_bool(ast)?,
            AstKind::Char => self.build_char(ast)?,
            AstKind::Variable(mutable) => self.build_variable(mutable, ast)?,
            AstKind::Constructor => self.build_constructor(ast)?,
            AstKind::DotExpr => self.build_dot_expr(ast)?,
            AstKind::Call => self.build_call(ast)?,
            AstKind::Block => self.build_block(ast)?,
            AstKind::Loop => self.build_loop(ast)?,
            AstKind::Break => self.build_break(ast)?,
            AstKind::Deref => self.build_deref(ast)?,
            AstKind::BitCast => self.build_bit_cast(ast)?,
            AstKind::Match => self.build_match(ast)?,
            AstKind::Error => return Err(()),
            _ => todo!(
                "Unhandled expression ast {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        };
        Ok(expr)
    }

    fn build_match(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[expr, body] = self.ast.children(ast) else {
            unreachable!();
        };
        
        let expr = self.build_expr(expr)?;

        self.pattern_graph.clear();

        // load match arms
        let mut temp = vec![];
        for &arm in self.ast.children(body) {
            let &[pat, body] = self.ast.children(arm) else {
                unreachable!();
            };

            drop(self.build_pattern(pat, expr, &mut temp, 1));

            let block = {
                let inner_block = {
                    self.scope.mark_frame();
                    self.temp.mark_frame();
    
                    for node in &temp {
                        if let (PatternMeta::Var(name), Some(value)) = (node.value, node.meta.expand()) {
                            let id = self.sources.id_of(name);
                            let item = ScopeItem::new(value, name);
                            self.scope.push_item(id, item);
    
                            self.temp.push({
                                let kind = TirKind::Variable(value);
                                let ty = self.builtin_types.nothing;
                                let ent = TirEnt::new(kind, ty, name);
                                self.body.ents.push(ent)
                            })
                        }
                    }
    
                    let expr = self.build_expr(body)?;
                    self.temp.push(expr);
                    
                    self.scope.pop_frame();

                    self.body.ents.push({
                        let content = self.temp.save_and_pop_frame(&mut self.body.cons);
                        let kind = TirKind::Block(content);
                        TirEnt {
                            kind,
                            ..self.body.ents[expr]
                        }
                    })
                };

                self.body.ents.push({
                    let kind = TirKind::MatchBlock(inner_block);
                    let ty = self.builtin_types.nothing;
                    let span = self.ast.nodes[body].span;
                    TirEnt::new(kind, ty, span)
                })
            };

            temp.push(PatternLevelData {
                meta: block.into(),
                ..Default::default()
            });

            self.pattern_graph.add_branch(temp.drain(..));
        }

        self.pattern_graph.build_graph();

        self.build_match_branching()
    }

    fn build_match_branching(&mut self) -> errors::Result<Tir> {
        todo!()
    }

    fn build_pattern(&mut self, ast: Ast, expr: Tir, branch: &mut Vec<PatternLevelData<Tir, PatternMeta>>, depth: u32) -> errors::Result {
        let AstEnt { kind, span, .. } = self.ast.nodes[ast]; 
        match kind {
            AstKind::StructPattern | AstKind::TupleStructPattern => {
                let is_tuple = kind == AstKind::TupleStructPattern;
                let &[path, body] = self.ast.children(ast) else {
                    unreachable!();
                };
                
                let low_expr = self.build_path_match(expr, path, branch, depth)?;
                let ty = self.body.ents[low_expr].ty;
    
                
                let TyKind::Struct(fields) = self.types[ty].kind else {
                    unreachable!();
                };
                  
                let mut last_i = 0;
                for (i, &field) in self.ast.children(body).iter().enumerate() {
                    let ((field_id, field_ty), field, i) = if is_tuple {
                        let span = self.ast.nodes[field].span;
                        let Ok(res) = self.get_field(ty, i, span) else {
                            continue;
                        };
                        (res, field, i)
                    } else {
                        let &[name, mut pattern] = self.ast.children(field) else {
                            unreachable!("{}", self.sources.display(span));
                        };
        
                        // shorthand case
                        if pattern.is_reserved_value() {
                            pattern = name;
                        }
        
                        let span = self.ast.nodes[name].span;
                        let id = self.sources.id_of(span);
    
                        let Ok(res) = self.find_field(ty, id, span) else {
                            continue;
                        };

                        let i = self.ty_comps[res.0].index;

                        (res, pattern, i as usize)
                    };

                    let field_access = {
                        let kind = TirKind::FieldAccess(low_expr, field_id);
                        let ent = TirEnt::new(kind, field_ty, span);
                        self.body.ents.push(ent)
                    };

                    // push skipped fields
                    for filed in &self.ty_comps.get(fields)[last_i..i] {
                        let range = self.ty_range(filed.ty);
                        let data = PatternLevelData {
                            range,
                            coverage: range,
                            depth,
                            
                            ..Default::default()
                        };
                        branch.push(data);
                    }
                    last_i = i;

                    drop(self.build_pattern(field, field_access, branch, depth + 1));
                }

                for filed in &self.ty_comps.get(fields)[last_i..] {
                    let range = self.ty_range(filed.ty);
                    let data = PatternLevelData {
                        range,
                        coverage: range,
                        depth,
                        
                        ..Default::default()
                    };
                    branch.push(data);
                }
            },
            
            AstKind::Path => drop(self.build_path_match(expr, ast, branch, depth)?),
            
            AstKind::Ident => {
                let id = self.sources.id_of(span);
                let range = self.ty_range(self.body.ents[expr].ty); 
                let (meta, value) = if id == ID::new("_") {
                    (None.into(), PatternMeta::Default)
                } else {
                    (Some(expr).into(), PatternMeta::Var(span))
                };
                let data = PatternLevelData {
                    meta,
                    range,
                    coverage: range,
                    depth,
                    value,
                };
                branch.push(data);
            },
            
            AstKind::Int |
            AstKind::Bool |
            AstKind::Char => {
                let tir = self.build_expr(ast)?;
                let ty = self.body.ents[expr].ty;
                self.infer_tir_ty(tir, ty, |_, _, _| todo!())?;
                
                // get comparator for given types
                // let func = {
                //     let id = {
                //         let ty_id = self.types[ty].id;
                //         let op_id = ID::new("==");
                //         ID::binary(ty_id, op_id)
                //     };
                //     self.scope.get::<Func>(self.diagnostics, id, span)?
                // };

                // let comparison = {
                //     let args = self.body.cons.push(&[expr, tir]);
                //     let kind = TirKind::Call(TyList::default(), func, args);
                //     let ty = self.builtin_types.bool;
                //     let ent = TirEnt::new(kind, ty, span);
                //     self.body.ents.push(ent)
                // };

                let data = {
                    let coverage = match self.body.ents[tir].kind {
                        TirKind::IntLit(value) => PatternRange::new(value..value),
                        TirKind::BoolLit(value) => PatternRange::new(value..value),
                        TirKind::CharLit(value) => PatternRange::new(value..value),
                        _ => unreachable!(),
                    };
                    let range = self.ty_range(ty);
                    PatternLevelData {
                        coverage,
                        range,
                        depth,
                        meta: Some(tir).into(),
                        value: PatternMeta::Cmp(expr)
                    }
                };

                branch.push(data);
            },

            kind => todo!("{kind:?}"),
        }

        Ok(())
    }

    fn build_path_match(&mut self, expr: Tir, ast: Ast, branch: &mut Vec<PatternLevelData<Tir, PatternMeta>>, depth: u32) -> errors::Result<Tir> {
        let TirEnt { ty: expr_ty, .. } = self.body.ents[expr];
        let id = ident_hasher!(self).ident_id(ast, None)?;
        let span = self.ast.nodes[ast].span;

        if let TyEnt { kind: TyKind::Enum(discriminant_ty, ..), id: enum_id, .. } = self.types[expr_ty] {
            let id = ID::field(enum_id, id);
            let Some(&variant) = self.ty_comp_lookup.get(id) else {
                todo!("{}", span.log(self.sources));                        
            };
            let TyCompEnt { ty: variant, index, .. } = self.ty_comps[variant];

            {
                let lit_value = index as i64 - 1;
                let flag = self.int_lit(lit_value, discriminant_ty);
    
                let flag_field = {
                    let (field, _) = self.get_field(expr_ty, 0, span)?;
                    let kind = TirKind::FieldAccess(expr, field);
                    let ent = TirEnt::new(kind, variant, span);
                    self.body.ents.push(ent)
                };

                let data = PatternLevelData {
                    meta: Some(flag).into(),
                    coverage: PatternRange::new(lit_value..lit_value),
                    depth: 0,
                    range: self.ty_range(discriminant_ty),
                    value: PatternMeta::Cmp(flag_field),
                };

                branch.push(data);
            }

            let value = {
                let (field, _) = self.get_field(expr_ty, 1, span)?;
                let kind = TirKind::FieldAccess(expr, field);
                let ent = TirEnt::new(kind, variant, span);
                self.body.ents.push(ent)
            };
            
            Ok(value)
        } else {
            let data = PatternLevelData {
                depth,
                ..Default::default()   
            };

            branch.push(data);

            Ok(expr)
        }
    }

    fn ty_range(&self, ty: Ty) -> PatternRange {
        match self.types[ty].kind {
            TyKind::Int(value) => match value {
                8 => PatternRange::new(i8::MIN..i8::MAX),
                16 => PatternRange::new(i16::MIN..i16::MAX),
                32 => PatternRange::new(i32::MIN..i32::MAX),
                64 => PatternRange::new(i64::MIN..i64::MAX),
                _ => PatternRange::new(isize::MIN..isize::MAX),
            },
            TyKind::Bool => PatternRange::new(0..1),
            _ => unreachable!(),
        }
    }

    /*
    fn build_match(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[expr, body] = self.ast.children(ast) else {
            unreachable!();
        };
        
        let expr = self.build_expr(expr)?;

        let mut ret_ty = None;
        let mut terminating = true;
        let mut bindings = Vec::new();

        let arms = {
            self.temp.mark_frame();
            for &arm in self.ast.children(body) {
                let &[pattern, body] = self.ast.children(arm) else {
                    unreachable!();
                };

                
                self.scope.mark_frame();
                let Ok(pattern) = self.build_pattern(expr, pattern, &mut bindings) else {
                    continue;
                };
                
                let block = {
                    self.temp.mark_frame();
    
                    for binding in bindings.drain(..) {
                        let binding = {
                            let kind = TirKind::Variable(binding);
                            let ty = self.builtin_types.nothing;
                            let span = self.body.ents[expr].span;
                            let ent = TirEnt::new(kind, ty, span);
                            self.body.ents.push(ent)
                        };
                        self.temp.push(binding);
                    }
    
                    let Ok(expr) = self.build_expr(body) else {
                        continue;
                    };
                    self.temp.push(expr);

                    self.scope.pop_frame();

                    let statements = self.temp.save_and_pop_frame(&mut self.body.cons);
                    let kind = TirKind::Block(statements);
                    let ty = self.body.ents[expr].ty;
                    let span = self.body.ents[expr].span;
                    let ent = TirEnt::new(kind, ty, span);
                    self.body.ents.push(ent)
                };

                let TirEnt { ty, flags, .. } = self.body.ents[block];

                let tir = {
                    let span = self.ast.nodes[arm].span;
                    let kind = TirKind::MatchArm(pattern, expr);
                    let ent = TirEnt::new(kind, ty, span);
                    self.body.ents.push(ent)
                };
                self.temp.push(tir);
                
                if !flags.contains(TirFlags::TERMINATING) {
                    terminating = false;
                    if let Some(ref mut ret_ty) = ret_ty {
                        if ty != *ret_ty {
                            *ret_ty = self.builtin_types.nothing;
                        }
                    } else {
                        ret_ty = Some(ty);
                    }
                }
            }
            self.temp.save_and_pop_frame(&mut self.body.cons)
        };

        let ty = ret_ty.unwrap_or(self.builtin_types.nothing);
        let span = self.ast.nodes[ast].span;
        let kind = TirKind::Match(expr, arms);
        let ent = TirEnt::with_flags(kind, ty, TirFlags::TERMINATING & terminating, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_pattern(&mut self, expr: Tir, ast: Ast, bindings: &mut Vec<Tir>) -> errors::Result<Tir> {
        let AstEnt { kind, span, .. } = self.ast.nodes[ast];

        Ok(match kind {
            AstKind::Ident => {
                let span = self.ast.nodes[ast].span;
                let id = self.sources.id_of(span);

                if id != ID::new("_") {
                    let item = ScopeItem::new(expr, span);
                    self.scope.push_item(id, item);
                    bindings.push(expr);
                }

                Tir::reserved_value()
            },
            AstKind::TupleStructPattern | AstKind::StructPattern => {
                let &[path, body] = self.ast.children(ast) else {
                    unreachable!();
                };
                
                let (low_expr, enum_flag) = self.build_path_match(expr, path)?;
                let ty = self.body.ents[low_expr].ty;
    
                self.temp.mark_frame();
                self.temp.pre_push(self.ast.children(body).len());
                for (i, &field) in self.ast.children(body).iter().enumerate() {
                    let ((field_id, field_ty), field, i) = if kind == AstKind::TupleStructPattern {
                        let span = self.ast.nodes[field].span;
                        let Ok(res) = self.get_field(ty, i, span) else {
                            continue;
                        };
                        (res, field, i)
                    } else {
                        let &[name, mut pattern] = self.ast.children(field) else {
                            unreachable!("{}", self.sources.display(span));
                        };
        
                        // shorthand case
                        if pattern.is_reserved_value() {
                            pattern = name;
                        }
        
                        let span = self.ast.nodes[name].span;
                        let id = self.sources.id_of(span);
    
                        let Ok(res) = self.find_field(ty, id, span) else {
                            continue;
                        };

                        let i = self.ty_comps[res.0].index;

                        (res, pattern, i as usize)
                    };

                    let field_access = {
                        let kind = TirKind::FieldAccess(low_expr, field_id);
                        let ent = TirEnt::new(kind, field_ty, span);
                        self.body.ents.push(ent)
                    };

                    let Ok(field) = self.build_pattern(field_access, field, bindings) else {
                        continue; // recover here
                    };

                    self.temp.set(i, field);
                }

                let constructor = {
                    let fields = self.temp.save_and_pop_frame(&mut self.body.cons);
                    let kind = TirKind::Constructor(fields);
                    let ty = self.body.ents[low_expr].ty;
                    let ent = TirEnt::new(kind, ty, span);
                    self.body.ents.push(ent)
                };

                if let Some(flag) = enum_flag {
                    let fields = self.body.cons.push(&[flag, constructor]);
                    let kind = TirKind::Constructor(fields);
                    let ty = self.body.ents[expr].ty;
                    let ent = TirEnt::new(kind, ty, span);
                    self.body.ents.push(ent)
                } else {
                    constructor
                }
            },
            AstKind::Char | AstKind::Int | AstKind::Bool => {
                self.build_expr(ast)?
            },
            _ => todo!(
                "Unhandled pattern ast {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        })
    }

    fn build_path_match(&mut self, expr: Tir, ast: Ast) -> errors::Result<(Tir, Option<Tir>)> {
        let TirEnt { ty: expr_ty, .. } = self.body.ents[expr];
        let id = ident_hasher!(self).ident_id(ast, None)?;
        let span = self.ast.nodes[ast].span;

        if let TyEnt { kind: TyKind::Enum(discriminant_ty, ..), id: enum_id, .. } = self.types[expr_ty] {
            let id = ID::field(enum_id, id);
            let Some(&variant) = self.ty_comp_lookup.get(id) else {
                todo!("{}", span.log(self.sources));                        
            };
            let TyCompEnt { ty: variant, index, .. } = self.ty_comps[variant];

            let flag = self.int_lit(index as i64 - 1, discriminant_ty);

            let value = {
                let (field, _) = self.get_field(expr_ty, 1, span)?;
                let kind = TirKind::FieldAccess(expr, field);
                let ent = TirEnt::new(kind, variant, span);
                self.body.ents.push(ent)
            };
            
            Ok((value, Some(flag)))
        } else {
            Ok((expr, None))
        }
    }
    */

    fn build_bit_cast(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[expr, ty] = self.ast.children(ast) else {
            unreachable!();
        };

        let expr = self.build_expr(expr);
        let ty = ty_parser!(self).parse_type(ty);

        let (expr, ty) = (expr?, ty?); // recovery

        self.ctx.use_type(ty, self.types);

        let kind = TirKind::BitCast(expr);
        let ent = TirEnt::new(kind, ty, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_deref(&mut self, ast: Ast) -> errors::Result<Tir> {
        let expr = self.ast.children(ast)[0];
        let target = self.build_expr(expr)?;
        let ty = self.body.ents[target].ty;

        if !matches!(self.types[ty].kind, TyKind::Ptr(..)) {
            self.diagnostics.push(TyError::NonPointerDereference {
                loc: self.ast.nodes[ast].span,
                ty,
            });
            return Err(());
        }

        Ok(self.deref_ptr(target))
    }

    fn build_char(&mut self, ast: Ast) -> errors::Result<Tir> {
        let literal_value = char_value(self.sources, self.ast.nodes[ast].span).unwrap();
        let span = self.ast.nodes[ast].span;
        let ent = TirEnt::new(TirKind::CharLit(literal_value), self.builtin_types.char, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_unary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[op, expr] = self.ast.children(ast) else {
            unreachable!()
        };

        let expr = self.build_expr(expr)?;
        let operand_ty = self.body.ents[expr].ty;

        let func = {
            let span = self.ast.nodes[op].span;
            let id = {
                let ty = self.types.base_id_of(operand_ty);

                let op = self.sources.id_of(span);

                ID::unary(ty, op)
            };

            self.scope.get::<Func>(self.diagnostics, id, span)?
        };

        let result = {
            let ty = self.func_meta[func].sig.ret;
            let span = self.ast.nodes[ast].span;
            let args = self.body.cons.push(&[expr]);
            let params = self.ty_lists.push(&[operand_ty]);
            let kind = TirKind::Call(params, func, args);
            let ent = TirEnt::with_flags(kind, ty, TirFlags::WITH_CALLER, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_break(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!();
        };

        let value = if value.is_reserved_value() {
            None
        } else {
            Some(self.build_expr(value)?)
        };

        let loop_expr = self.scope.get::<Tir>(self.diagnostics, "<loop>", span)?;

        {
            let TirKind::LoopInProgress(ret, infinite) = &mut self.body.ents[loop_expr].kind else {
                unreachable!();
            };

            *infinite = false;

            if let Some(ret) = ret.expand() {
                let TirEnt {
                    span: ret_span, ty, ..
                } = self.body.ents[ret];
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
        }

        let result = {
            let kind = TirKind::Break(loop_expr, value.into());
            let ent = TirEnt::with_flags(
                kind,
                self.builtin_types.nothing,
                TirFlags::TERMINATING,
                span,
            );
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_loop(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[body_ast] = self.ast.children(ast) else {
            unreachable!();
        };

        let loop_slot = {
            let kind = TirKind::LoopInProgress(None.into(), true);
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.body.ents.push(ent)
        };

        // break and continue will propagate side effects so we save the lookup

        self.scope
            .push_item("<loop>", ScopeItem::new(loop_slot, span));

        let block = self.build_block(body_ast)?;

        self.scope.pop_item();

        {
            let TirKind::LoopInProgress(ret, infinite) = self.body.ents[loop_slot].kind else {
                unreachable!();
            };
            let ty = ret
                .map(|ret| self.body.ents[ret].ty)
                .unwrap_or(self.builtin_types.nothing);
            let flags = TirFlags::TERMINATING & infinite;
            let kind = TirKind::Loop(block);
            let span = span;
            self.body.ents[loop_slot] = TirEnt::with_flags(kind, ty, flags, span);
        }

        Ok(loop_slot)
    }

    fn build_call(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[caller, ..] = self.ast.children(ast) else {
            unreachable!();
        };

        let (id, fn_span, mut obj, caller, instantiation) =
            if self.ast.nodes[caller].kind == AstKind::DotExpr {
                let &[expr, name] = self.ast.children(caller) else {
                    unreachable!();
                };

                let expr = self.build_expr(expr)?;
                let ty = {
                    let ty = self.body.ents[expr].ty;
                    self.types.caller_of(ty)
                };

                let (ident, instantiation) = {
                    if AstKind::Instantiation == self.ast.nodes[name].kind {
                        (self.ast.children(name)[0], Some(name))
                    } else {
                        (name, None)
                    }
                };

                let span = self.body.ents[expr].span;
                let name_id = ident_hasher!(self).ident_id(ident, Some((ty, span)))?;

                let span = self.ast.nodes[name].span;
                (name_id, span, Some(expr), Some(ty), instantiation)
            } else {
                let (ident, instantiation) = {
                    if AstKind::Instantiation == self.ast.nodes[caller].kind {
                        (self.ast.children(caller)[0], Some(caller))
                    } else {
                        (caller, None)
                    }
                };

                let (name_id, owner) = ident_hasher!(self).ident_id_low(ident, None)?;

                let span = self.ast.nodes[caller].span;
                (name_id, span, None, owner.map(|(ty, _)| ty), instantiation)
            };

        // TODO: Handle function pointer as field
        let func = self.scope.get::<Func>(self.diagnostics, id, fn_span)?;

        let sig = self.func_meta[func].sig;
        let flags = self.funcs.ents[func].flags;

        let arg_tys = self.ty_lists.get(sig.args).to_vec(); // TODO: avoid allocation

        // handle auto ref or deref
        if let (Some(obj), Some(&expected)) = (obj.as_mut(), arg_tys.first()) {
            *obj = self.ptr_correct(*obj, expected);
        }

        // we first collect results, its in a way of recovery
        // TODO: This can be optimized when needed
        let args = {
            let mut vec = Vec::with_capacity(arg_tys.len() + obj.is_some() as usize);
            if let Some(obj) = obj {
                vec.push(obj);
            }
            self.ast.children(ast)[1..]
                .iter()
                .fold(Ok(()), |acc, &arg| {
                    self.build_expr(arg).map(|arg| vec.push(arg)).and(acc)
                })?;
            vec
        };

        let caller_offset = caller.is_some() as usize;
        let generic = flags.contains(FuncFlags::GENERIC);

        let because = self.func_meta[func].name;
        // check arg count
        {
            let args_len = args.len();
            if arg_tys.len() != args_len {
                let loc = self.ast.nodes[ast].span;
                self.diagnostics.push(TyError::FunctionParamMismatch {
                    because,
                    expected: arg_tys.len(),
                    got: args_len,
                    loc,
                });
                return Err(());
            }
        }

        let (ret, func, params) = if !generic {
            // here we type check all arguments and then check for errors
            args.iter()
                .zip(arg_tys)
                .map(|(&arg, ty)| {
                    self.infer_tir_ty(arg, ty, |_, got, loc| TyError::CallArgTypeMismatch {
                        because,
                        expected: ty,
                        got,
                        loc,
                    })
                })
                .fold(Ok(()), |acc, err| acc.and(err))?;

            let params = if let Some(caller) = caller {
                self.ty_lists.push(&[caller])
            } else {
                TyList::reserved_value()
            };

            (sig.ret, func, params)
        } else {
            let params = self.ty_lists.get(sig.params);

            prepare_params(params, self.types);

            let mut param_vec = vec![Ty::default(); params.len() + caller_offset];
            if let Some(caller) = caller {
                param_vec[0] = caller;
            }
            let param_slots = &mut param_vec[caller_offset..];

            if let Some(instantiation) = instantiation {
                if params.len() > self.ast.children(instantiation).len() - 1 {
                    todo!("this is an error");
                }

                for (i, &param) in self.ast.children(instantiation)[1..].iter().enumerate() {
                    let Ok(ty) = ty_parser!(self).parse_type_optional(param) else {
                        continue;
                    };
                    param_slots[i] = ty;
                }
            }

            args.iter()
                .zip(arg_tys)
                .map(|(&arg, arg_ty)| {
                    let TirEnt { ty, span, .. } = self.body.ents[arg];
                    infer_parameters(
                        ty,
                        arg_ty,
                        param_slots,
                        span,
                        self.types,
                        self.ty_lists,
                        self.bound_impls,
                        self.diagnostics,
                    )
                })
                // fold prevents allocation
                .fold(Ok(()), |acc, err| acc.and(err))?;

            if let (None, Some(ty), Some(param)) = (obj, caller, param_slots.last_mut()) && param.is_reserved_value() {
                *param = ty;
            }

            param_slots
                .iter()
                .enumerate()
                .filter_map(|(i, ty)| ty.is_reserved_value().then_some(i))
                // OK if no cycles performed
                .fold(Ok(()), |_, param| {
                    let span = self.ast.nodes[ast].span;
                    self.diagnostics.push(TyError::UnknownGenericParam {
                        func: self.func_meta[func].name,
                        param,
                        loc: span,
                    });
                    Err(())
                })?;

            let ret = ty_parser!(self).instantiate(sig.ret, param_slots);

            self.ctx.use_type(ret, self.types);

            (ret, func, self.ty_lists.push(&param_vec))
        };

        let result = {
            let span = self.ast.nodes[ast].span;
            let args = self.body.cons.push(&args);
            let kind = TirKind::Call(params, func, args);
            let flags = TirFlags::WITH_CALLER & caller.is_some() | TirFlags::GENERIC & generic;
            let ent = TirEnt::with_flags(kind, ret, flags, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn ptr_correct(&mut self, mut target: Tir, expected: Ty) -> Tir {
        let ty = self.body.ents[target].ty;

        let target_depth = self.types.depth_of(ty);
        let expected_depth = self.types.depth_of(expected);

        for _ in expected_depth..target_depth {
            target = self.deref_ptr(target);
        }

        for _ in target_depth..expected_depth {
            target = self.take_ptr(target);
        }

        target
    }

    fn take_ptr(&mut self, target: Tir) -> Tir {
        let TirEnt {
            ty,
            span,
            flags,
            kind,
            ..
        } = &mut self.body.ents[target];
        let (span, ty) = (*span, *ty);
        if let &mut TirKind::Access(target) = kind {
            self.body.ents[target].flags.insert(TirFlags::SPILLED);
        } else {
            flags.insert(TirFlags::SPILLED);
        }

        let kind = TirKind::TakePtr(target);
        let ptr_ty = pointer_of(ty, self.types, self.instances);
        self.ctx.use_type(ptr_ty, self.types);
        let deref = TirEnt::new(kind, ptr_ty, span);
        self.body.ents.push(deref)
    }

    fn deref_ptr(&mut self, target: Tir) -> Tir {
        let span = self.body.ents[target].span;
        let kind = TirKind::DerefPointer(target);
        let deref_ty = self.types.deref(self.body.ents[target].ty);
        self.ctx.use_type(deref_ty, self.types);
        let deref = TirEnt::new(kind, deref_ty, span);
        self.body.ents.push(deref)
    }

    fn build_dot_expr(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[header, field] = self.ast.children(ast) else {
            unreachable!();
        };

        let mut header = self.build_expr(header)?;

        let ty = self.body.ents[header].ty;
        let base_ty = self.types.base_of(ty);

        let span = self.ast.nodes[field].span;
        let (field_id, field_ty) = {
            let id = self.sources.id_of(span);
            self.find_field(base_ty, id, span)?
        };

        while base_ty != self.body.ents[header].ty {
            header = self.deref_ptr(header);
        }

        let result = {
            self.ctx.use_type(field_ty, self.types);
            let kind = TirKind::FieldAccess(header, field_id);
            let ent = TirEnt::new(kind, field_ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_bool(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let ty = self.builtin_types.bool;
        let value = {
            let str = self.sources.display(span);
            str == "true"
        };
        let kind = TirKind::BoolLit(value);
        let ent = TirEnt::new(kind, ty, span);
        let result = self.body.ents.push(ent);
        Ok(result)
    }

    fn build_constructor(&mut self, ast: Ast) -> errors::Result<Tir> {
        let &[name, body] = self.ast.children(ast) else {
            unreachable!("{:?}", self.ast.children(ast));
        };

        let span = self.ast.nodes[name].span;
        let (id, owner) = ident_hasher!(self).ident_id_low(name, None)?;
        if let Some((enum_ty, span)) = owner {
            let Some(&variant) = self.ty_comp_lookup.get(id) else {
                todo!();
            };
            let TyCompEnt { ty, index, .. } = self.ty_comps[variant];
            let TyKind::Enum(discriminant, _) = self.types[enum_ty].kind else {
                unreachable!();
            };
            let value = self.build_constructor_low(ty, body)?;
            let flag = self.int_lit(index as i64 - 1, discriminant);

            let args = self.body.cons.push(&[flag, value]);
            let kind = TirKind::Constructor(args);
            let ent = TirEnt::new(kind, enum_ty, span);

            Ok(self.body.ents.push(ent))
        } else {
            let ty = self.scope.get::<Ty>(self.diagnostics, id, span)?;
            self.build_constructor_low(ty, body)
        }
    }

    fn build_constructor_low(&mut self, ty: Ty, body: Ast) -> errors::Result<Tir> {
        let AstEnt { span, kind, .. } = self.ast.nodes[body];
        let TyEnt { flags, .. } = self.types[ty];

        let TyKind::Struct(fields) = self.types[ty].kind else {
            self.diagnostics.push(TyError::ExpectedStruct {
                got: ty,
                loc: span,
            });
            return Err(());
        };
        
        let mut initial_values = vec![Tir::reserved_value(); self.ty_comps.get(fields).len()];

        // TODO: maybe avoid allocation
        let mut param_slots = vec![Ty::reserved_value(); flags.param_count()];
        for (i, &field) in self.ast.children(body).iter().enumerate() {
            let (expr, (field, field_ty)) = if kind == AstKind::ConstructorBody {
                let &[name, expr] = self.ast.children(field) else {
                    unreachable!();
                };
    
                let span = self.ast.nodes[name].span;
                let id = self.sources.id_of(span);
    
                let Ok(res) = self.find_field(ty, id, span) else {
                    continue;
                };
                (expr, res)
            } else {
                let span = self.ast.nodes[field].span;
                let Ok(res) = self.get_field(ty, i, span) else {
                    continue;
                };
                (field, res)
            };

            let TyCompEnt {
                span: hint, index, ..
            } = self.ty_comps[field];

            let Ok(value) = (match self.ast.nodes[expr].kind {
                AstKind::ConstructorBody | AstKind::TupleConstructorBody => self.build_constructor_low(field_ty, expr),
                _ => self.build_expr(expr),
            }) else {
                continue;
            };

            if self.types[field_ty].flags.contains(TyFlags::GENERIC) {
                let TirEnt { ty, span, .. } = self.body.ents[value];
                drop(infer_parameters(
                    ty,
                    field_ty,
                    &mut param_slots,
                    span,
                    self.types,
                    self.ty_lists,
                    self.bound_impls,
                    self.diagnostics,
                ));
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
                .map(|i| self.ty_comps.get(fields)[i].span)
                .collect::<Vec<_>>();

            self.diagnostics.push(TyError::ConstructorMissingFields {
                on: ty,
                missing,
                loc: span,
            });

            return Err(());
        }

        let span = self.ast.nodes[body].span;

        let ty = if param_slots.len() > 0 {
            param_slots
                .iter()
                .enumerate()
                .filter_map(|(i, ty)| ty.is_reserved_value().then_some(i))
                // OK if no cycles performed
                .fold(Ok(()), |_, param| {
                    self.diagnostics.push(TyError::UnknownGenericTypeParam {
                        ty: self.types[ty].name,
                        param,
                        loc: span,
                    });
                    Err(())
                })?;

            self.ty_lists.mark_frame();
            for param in param_slots {
                self.ty_lists.push_one(param);
            }

            ty_parser!(self).parse_instance_type_low(ty, span)
        } else {
            ty
        };

        self.ctx.use_type(ty, self.types);

        let result = {
            let fields = self.body.cons.push(&initial_values);
            let kind = TirKind::Constructor(fields);
            let ent = TirEnt::new(kind, ty, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_variable(&mut self, mutable: bool, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[name, value] = self.ast.children(ast) else {
            unreachable!();
        };

        let id = {
            let span = self.ast.nodes[name].span;
            let str = self.sources.display(span);
            ID::new(str)
        };

        let value = self.build_expr(value)?;
        self.body.ents[value]
            .flags
            .insert(TirFlags::ASSIGNABLE & mutable);

        {
            let item = ScopeItem::new(value, span);
            self.scope.push_item(id, item);
        }

        let result = {
            let kind = TirKind::Variable(value);
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_int(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let literal_value = int_value(self.sources, span);
        let kind = TirKind::IntLit(literal_value);
        let ty = {
            let str = self.sources.display(span);
            if str.ends_with("i8") {
                self.builtin_types.i8
            } else if str.ends_with("i16") {
                self.builtin_types.i16
            } else if str.ends_with("i32") {
                self.builtin_types.i32
            } else if str.ends_with("i64") {
                self.builtin_types.i64
            } else {
                self.builtin_types.int
            }
        };

        let ent = TirEnt::new(kind, ty, span);
        Ok(self.body.ents.push(ent))
    }

    fn build_return(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[value] = self.ast.children(ast) else {
            unreachable!()
        };

        let ret = self.func_meta[self.func].sig.ret;

        let value = if value.is_reserved_value() {
            self.infer_ty(self.builtin_types.nothing, ret, |s| {
                TyError::UnexpectedReturnValue {
                    because: s.func_meta[s.func].name,
                    loc: span,
                }
            })?;
            None
        } else {
            let value = self.build_expr(value)?;
            self.infer_tir_ty(value, ret, |s, got, loc| {
                let &[.., ret_ast, _] = s.ast.children(s.ctx.func_ast[s.func]) else {
                    unreachable!();
                };
                let ret_span = (!ret_ast.is_reserved_value()).then(|| s.ast.nodes[ret_ast].span);
                TyError::ReturnTypeMismatch {
                    because: ret_span,
                    expected: ret,
                    got,
                    loc,
                }
            })?;
            Some(value)
        };

        let result = {
            let kind = TirKind::Return(value.into());
            let ent = TirEnt::with_flags(
                kind,
                self.builtin_types.nothing,
                TirFlags::TERMINATING,
                span,
            );
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_if(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[cond, then, otherwise] = self.ast.children(ast) else {
            unreachable!()
        };

        let cond = self.build_expr(cond)?;
        drop(
            self.infer_tir_ty(cond, self.builtin_types.bool, |_, got, loc| {
                TyError::IfConditionTypeMismatch { got, loc }
            }),
        );

        let then = self.build_block(then);
        let otherwise = if otherwise.is_reserved_value() {
            None
        } else {
            Some(self.build_block(otherwise)?)
        };
        let then = then?;

        let ty = if let Some(otherwise) = otherwise {
            let then = self.body.ents[then].ty;
            let otherwise = self.body.ents[otherwise].ty;
            if then == otherwise {
                then
            } else {
                self.builtin_types.nothing
            }
        } else {
            self.builtin_types.nothing
        };

        let flags = {
            let ents = &self.body.ents;

            let then_flags = ents[then].flags;
            let otherwise_flags = otherwise
                .map(|otherwise| ents[otherwise].flags)
                .unwrap_or(TirFlags::empty());

            (then_flags & otherwise_flags) & TirFlags::TERMINATING
        };

        let result = {
            let kind = TirKind::If(cond, then, otherwise.into());
            let ent = TirEnt::with_flags(kind, ty, flags, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn build_ident(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let id = self.sources.id_of(span);

        let value = self.scope.get::<Tir>(self.diagnostics, id, span)?;

        // this allows better error messages
        let result = {
            let mut copy = self.body.ents[value];
            copy.kind = TirKind::Access(value);
            copy.span = span;
            self.body.ents.push(copy)
        };

        Ok(result)
    }

    fn build_binary(&mut self, ast: Ast) -> errors::Result<Tir> {
        let span = self.ast.nodes[ast].span;
        let &[left, op, right] = self.ast.children(ast) else {
            unreachable!()
        };

        // we walk the ast even if something fails for better error diagnostics
        let (left, right) = {
            let left = self.build_expr(left);
            let right = self.build_expr(right);
            (left?, right?)
        };

        let left_ty = self.body.ents[left].ty;

        let op_span = self.ast.nodes[op].span;
        let id = {
            let op_id = {
                let str = self.sources.display(op_span);
                if str == "=" {
                    return self.build_assign(left, right, span);
                }

                ID::new(str)
            };

            let left_id = self.types.base_id_of(left_ty);

            ID::binary(left_id, op_id)
        };

        let Ok(func) = self.scope.get::<Func>(self.diagnostics, id, op_span) else {
            self.diagnostics.push(TyError::BinaryOperatorNotFound {
                left_ty: self.body.ents[left].ty,
                right_ty: self.body.ents[right].ty,
                loc: op_span,
            });
            return Err(());
        };

        /* sanity check */
        {
            let func_ent = &self.func_meta[func];
            let args = self.ty_lists.get(func_ent.sig.args);

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

            let expected = args[1];
            self.infer_tir_ty(right, expected, |_, got, loc| TyError::BinaryTypeMismatch {
                expected,
                got,
                loc,
            })?;
        }

        let tir = {
            let ty = self.func_meta[func].sig.ret;
            let args = self.body.cons.push(&[left, right]);
            let caller = self.ty_lists.push(&[left_ty]);
            let kind = TirKind::Call(caller, func, args);
            let ent = TirEnt::with_flags(kind, ty, TirFlags::WITH_CALLER, span);
            self.body.ents.push(ent)
        };

        Ok(tir)
    }

    fn build_assign(&mut self, left: Tir, right: Tir, span: Span) -> errors::Result<Tir> {
        let TirEnt {
            ty,
            flags,
            span: left_span,
            kind,
        } = self.body.ents[left];

        if !flags.contains(TirFlags::ASSIGNABLE) {
            let because = if let TirKind::Access(here) = kind {
                Some(self.body.ents[here].span)
            } else {
                None
            };
            self.diagnostics.push(TyError::AssignToNonAssignable {
                because,
                loc: left_span,
            });
        }

        self.infer_tir_ty(right, ty, |_, got, loc| TyError::AssignTypeMismatch {
            because: left_span,
            expected: ty,
            got,
            loc,
        })?;

        let result = {
            let kind = TirKind::Assign(left, right);
            let ent = TirEnt::new(kind, self.builtin_types.nothing, span);
            self.body.ents.push(ent)
        };

        Ok(result)
    }

    fn get_field(&mut self, on: Ty, index: usize, _span: Span) -> errors::Result<(TyComp, Ty)> {
        let base = if let TyKind::Instance(base, ..) = self.types[on].kind {
            base
        } else {
            on
        };

        let (TyKind::Struct(fields) | TyKind::Enum(.., fields)) = self.types[base].kind else {
            todo!("{:?}", self.types[base].kind);
        };

        let Some(field) = self.ty_comps.get(fields).get(index) else {
            todo!();
        };

        let ty = if let TyKind::Instance(.., params) = self.types[on].kind {
            let params = self.ty_lists.get(params).to_vec();
            ty_parser!(self).instantiate(field.ty, &params)
        } else {
            field.ty
        };

        Ok((TyComp::new(self.ty_comps.start_index_of(fields).unwrap() + index), ty))
    }

    fn find_field(&mut self, on: Ty, id: ID, loc: Span) -> errors::Result<(TyComp, Ty)> {
        let id = {
            let on = if let TyKind::Instance(base, ..) = self.types[on].kind {
                base
            } else {
                on
            };
            let ty_id = self.types[on].id;
            ID::field(ty_id, id)
        };

        self.ty_comp_lookup
            .get(id)
            .map(|&f| {
                let ty = self.ty_comps[f].ty;
                if let TyKind::Instance(_, params) = self.types[on].kind {
                    let params = self.ty_lists.get(params).to_vec();
                    let ty = ty_parser!(self).instantiate(ty, &params);
                    (f, ty)
                } else {
                    (f, ty)
                }
            })
            .ok_or_else(|| {
                let candidates = if let TyKind::Struct(fields) = self.types[on].kind {
                    self.ty_comps
                        .get(fields)
                        .iter()
                        .map(|field| field.span)
                        .collect()
                } else {
                    Vec::new()
                };

                self.diagnostics.push(TyError::UnknownField {
                    candidates,
                    on,
                    loc,
                });
            })
    }

    fn infer_tir_ty(
        &mut self,
        right: Tir,
        ty: Ty,
        err_fact: impl Fn(&mut Self, Ty, Span) -> TyError,
    ) -> errors::Result {
        let TirEnt { ty: got, span, .. } = self.body.ents[right];
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

    fn int_lit(&mut self, value: i64, ty: Ty) -> Tir {
        let kind = TirKind::IntLit(value);
        let ent = TirEnt::new(kind, ty, self.builtin_types.discriminant);
        self.body.ents.push(ent)
    }
}

#[derive(Clone, Copy)]
pub enum PatternMeta {
    Cmp(Tir),
    Var(Span),
    Default,
}

impl Default for PatternMeta {
    fn default() -> Self {
        PatternMeta::Default
    }
}