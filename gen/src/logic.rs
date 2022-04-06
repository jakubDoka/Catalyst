use cranelift_codegen::ir::{InstBuilder, Signature};
use cranelift_codegen::{ir, packed_option::PackedOption};
use cranelift_entity::SecondaryMap;
use cranelift_frontend::FunctionBuilder;
use instance::mir;
use typec::tir::LinkedList;

typec::gen_context!(Generator<'a> {
    builder: &'a mut FunctionBuilder<'a>,
    value_lookup: &'a mut SecondaryMap<mir::Value, PackedOption<ir::Value>>,
    source: &'a instance::Function,
});

impl<'a> Generator<'a> {
    pub fn generate(&mut self) {
        Self::transfer_signature(
            &self.source.signature,
            &mut self.builder.func.signature
        );

        for (id, _) in self.source.blocks() {
            self.generate_block(id);
        }

        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    fn transfer_signature(from: &Signature, to: &mut Signature) {
        to.clear(from.call_conv);
        to.params.extend(from.params.iter().cloned());
        to.returns.extend(from.returns.iter().cloned());
    }

    fn generate_block(&mut self, id: mir::Block) {
        let block = self.builder.create_block();
        self.builder.switch_to_block(block);

        for (value, ent) in self.source.block_params(id) {
            let ir_value = self.builder.append_block_param(block, ent.repr);
            self.value_lookup[value] = ir_value.into();
        }

        for (_, inst) in self
            .source
            .insts
            .linked_iter(self.source.blocks[id].first.expand())
        {
            self.generate_inst(inst);
        }
    }

    fn generate_inst(&mut self, inst: &mir::inst::Ent) {
        match inst.kind {
            mir::Kind::IntLit(literal) => {
                let value = inst.value.unwrap();
                let repr = self.source.values[value].repr;
                let ir_value = self.builder.ins().iconst(repr, literal as i64);
                self.value_lookup[value] = ir_value.into();
            }
            mir::Kind::Return => {
                if let Some(value) = inst.value.expand() {
                    let ir_value = self.value_lookup[value];
                    self.builder.ins().return_(&[ir_value.unwrap()]);
                } else {
                    self.builder.ins().return_(&[]);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use cranelift_frontend::FunctionBuilderContext;
    use instance::{logic::Translator, Function};
    use lexer::{SourceEnt, Sources, Span, ID};
    use modules::{scope::{self, Scope}, module, logic::Modules};
    use parser::{ast, Parser};
    use typec::{Builder, Collector, Func, Functions, Types};

    use super::*;

    #[test]
    fn test_translation() {
        let mut scope = Scope::new();
        let mut sources = Sources::new();
        let mut functions = Functions::new();
        let mut types = Types::new();
        let mut modules = Modules::new();
        let test_str = "
        fn \"windows_fastcall\" main() -> int {
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
            value_lookup: &mut SecondaryMap::new(),
            function: &mut function,
            t_functions: &functions,
            t_types: &types,
            sources: &sources,
        }
        .translate_func(func)
        .unwrap();

        let mut context = FunctionBuilderContext::new();
        let mut ir_function = ir::Function::new();

        let mut builder = FunctionBuilder::new(&mut ir_function, &mut context);

        Generator {
            builder: &mut builder,
            value_lookup: &mut SecondaryMap::new(),
            source: &function,
        }
        .generate();

        println!("{}", ir_function.display());
    }
}
