use std::str::FromStr;

use cranelift_codegen::{ir, isa::CallConv};
use cranelift_entity::SecondaryMap;
use lexer::{Sources, SourcesExt};
use typec::{self, tir, Ty, Signature};

use crate::{error::{Error, self}, func, mir};

type Result<T = ()> = std::result::Result<T, Error>;

typec::gen_context!(Translator<'a> {
    ptr_ty: ir::Type,
    system_call_convention: CallConv,
    value_lookup: &'a mut SecondaryMap<tir::Value, mir::Value>,
    function: &'a mut func::Function,
    t_functions: &'a typec::Functions,
    t_types: &'a typec::Types,
    sources: &'a Sources,
});

impl<'a> Translator<'a> {
    pub fn translate_func(&mut self, func: typec::Func) -> Result<()> {
        self.function.clear();

        let Signature { call_conv, args, ret } = self.t_functions.ents[func].sig;
        
        self.function.signature.call_conv = {
            let str = self.sources.display(call_conv.strip_sides());
            if str == "default" {
                self.system_call_convention
            } else {
                CallConv::from_str(str)
                    .map_err(|_| Error::new(error::Kind::InvalidCallConv, call_conv))?
            }
        };
        
        let params = self.t_types.slice(args).iter().map(|&ty| {
            let repr = Self::repr_low(self.t_types, self.ptr_ty, ty);
            ir::AbiParam::new(repr)
        });
        self.function.signature.params.extend(params);

        if let Some(ret_ty) = ret.expand() {
            let repr = Self::repr_low(self.t_types, self.ptr_ty, ret_ty);
            self.function
                .signature
                .returns
                .push(ir::AbiParam::new(repr));
        }

        for (id, _) in self.t_functions.blocks_of(func) {
            self.translate_block(id)?;
        }

        Ok(())
    }

    pub fn translate_block(&mut self, id: tir::Block) -> Result {
        self.function
            .create_block(self.t_functions.block_params(id).map(|(_, param)| {
                mir::value::Ent::new(Self::repr_low(self.t_types, self.ptr_ty, param.ty))
            }));

        for (_, inst) in self.t_functions.insts_of(id) {
            self.translate_inst(inst)?;
        }

        Ok(())
    }

    pub fn translate_inst(&mut self, inst: &tir::inst::Ent) -> Result {
        match inst.kind {
            tir::Kind::IntLit => {
                let inst_value = inst.result.unwrap();

                let value = {
                    let ty = self.t_functions.values[inst_value].ty;
                    let repr = self.repr(ty);
                    let ent = mir::value::Ent::new(repr);
                    self.function.values.push(ent)
                };

                let inst = {
                    let literal = lexer::int_value(self.sources, inst.span);
                    let kind = mir::inst::Kind::IntLit(literal);
                    mir::inst::Ent::with_value(kind, value)
                };

                self.value_lookup[inst_value] = value;
                self.function.add_inst(inst);
            }
            tir::Kind::Return => {
                let inst = {
                    let value = inst.result.unwrap();
                    let value = self.value_lookup[value];
                    let kind = mir::inst::Kind::Return;
                    mir::inst::Ent::with_value(kind, value)
                };
                self.function.add_inst(inst);
            }
        }

        Ok(())
    }

    pub fn repr(&self, ty: Ty) -> ir::Type {
        Self::repr_low(self.t_types, self.ptr_ty, ty)
    }

    pub fn repr_low(types: &typec::Types, ptr_ty: ir::Type, ty: Ty) -> ir::Type {
        match types.get(ty).kind {
            typec::Kind::Int(base) => match base {
                64 => ir::types::I64,
                32 => ir::types::I32,
                16 => ir::types::I16,
                8 => ir::types::I8,
                _ => ptr_ty,
            },
        }
    }
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use lexer::{SourceEnt, Span, ID};
    use modules::{scope::{self, Scope}, logic::Modules, module};
    use parser::{ast, Parser};
    use typec::{Builder, Collector, Func, Functions, Types};

    use crate::Function;

    use super::*;

    #[test]
    fn test_translation() {
        let mut scope = Scope::new();
        let mut sources = Sources::new();
        let mut functions = Functions::new();
        let mut types = Types::new();
        let mut modules = Modules::new();
        let test_str = "
        fn main() -> int {
            ret 0
        }
        ";

        let int = types.add(typec::ty::Ent::new(typec::Kind::Int(-1), "int".into()));

        let source = sources.push(SourceEnt::new(PathBuf::from(""), test_str.to_string()));
        scope
            .insert(
                source,
                "int",
                scope::Item::new(int, Span::new(source, 0, 0)),
            )
            .unwrap();

        let module = module::Ent::new(ID::default());
        let module = modules.push(module);

        let mut ast_data = ast::Data::new();
        let mut ast_temp = ast::Temp::new();

        let inter_state =
            Parser::parse_imports(test_str, &mut ast_data, &mut ast_temp, source).unwrap();
        Parser::parse_code_chunk(test_str, &mut ast_data, &mut ast_temp, inter_state).unwrap();

        Collector {
            scope: &mut scope,
            functions: &mut functions,
            types: &mut types,
            modules: &mut modules,
            sources: &sources,
            ast: &ast_data,
        }
        .collect_items(module)
        .unwrap();

        let func = scope.get::<Func>("main", Span::default()).unwrap();

        Builder {
            scope: &mut scope,
            functions: &mut functions,
            types: &mut types,
            sources: &sources,
            ast: &ast_data,
        }
        .build_function_ir(func)
        .unwrap();

        let mut function = Function::new();

        Translator {
            ptr_ty: ir::types::I32,
            system_call_convention: CallConv::Fast,
            value_lookup: &mut SecondaryMap::new(),
            function: &mut function,
            t_functions: &functions,
            t_types: &types,
            sources: &sources,
        }
        .translate_func(func)
        .unwrap();

        println!("{:?}", function);
    }
}
