// #![feature(let_else)]
// #![feature(fs_try_exists)]
// #![feature(thread_id_value)]
// #![feature(default_free_fn)]
// #![feature(array_zip)]
// #![feature(unboxed_closures)]
// #![feature(fn_traits)]

// use std::{default::default, fmt::Write, fs, iter, path::Path, process::Command, vec};

// use cranelift_codegen::{ir::InstBuilder, settings, Context};
// use cranelift_frontend::FunctionBuilderContext;
// use target_lexicon::Triple;

// use diags::*;
// use gen::*;
// use lexing::*;
// use mir::*;
// use mir_t::*;
// use packaging::*;
// use packaging_t::*;
// use parsing::*;

// use storage::*;
// use testing::*;
// use typec::*;

// use typec_t::*;

// function_pointer! {
//     ConstUsize -> usize,
//     ConstU32 -> u32,
// }

// struct LaterInit {
//     object_context: ObjectContext,
//     context: Context,
//     func_ctx: FunctionBuilderContext,
//     jit_context: JitContext,
//     jit_isa: Isa,
// }

// #[derive(Default)]
// struct TestState {
//     interner: Interner,
//     scope: Scope,
//     typec: Typec,
//     workspace: Workspace,
//     resources: Resources,
//     package_graph: PackageGraph,
//     typec_ctx: TyCheckerCtx,
//     ast_transfer: AstTransfer<'static>,
//     mir_ctx: MirBuilderCtx,
//     gen: Gen,
//     object_resources: GenResources,
//     jit_resources: GenResources,
//     gen_layouts: GenLayouts,
//     compile_requests: CompileRequests,
//     functions: String,
//     later_init: Option<LaterInit>,
//     mir: Mir,
//     entry_points: Vec<VRef<CompiledFunc>>,
//     mir_type_swapper: MirTypeSwapper,
// }

// impl TestState {
//     fn get_init_later(&mut self) -> LaterInit {
//         self.later_init.take().unwrap_or_else(|| {
//             let object_isa = Isa::new(
//                 Triple::host(),
//                 settings::Flags::new(settings::builder()),
//                 false,
//                 &mut self.interner,
//             )
//             .unwrap();
//             let jit_isa = Isa::new(
//                 Triple::host(),
//                 settings::Flags::new(settings::builder()),
//                 true,
//                 &mut self.interner,
//             )
//             .unwrap();
//             self.gen_layouts.ptr_ty = object_isa.pointer_ty;
//             LaterInit {
//                 object_context: ObjectContext::new(object_isa).unwrap(),
//                 context: Context::new(),
//                 func_ctx: FunctionBuilderContext::new(),
//                 jit_context: JitContext::new([("ctl_lexer_next", ctl_lexer_next as _)]),
//                 jit_isa,
//             }
//         })
//     }

//     fn collect_entry_points(&mut self, triple: VRef<str>) -> Vec<CompileRequest> {
//         let entry_points = self
//             .mir_ctx
//             .just_compiled
//             .drain(..)
//             .filter(|&func| self.typec.funcs[func].flags.contains(FuncFlags::ENTRY))
//             .map(|func| {
//                 let id = Generator::func_instance_name(
//                     true,
//                     triple,
//                     func,
//                     iter::empty(),
//                     &self.typec,
//                     &mut self.interner,
//                 );
//                 let id = self
//                     .gen
//                     .compiled_funcs
//                     .insert_unique(id, CompiledFunc::new(func));
//                 CompileRequest {
//                     id,
//                     func,
//                     params: default(),
//                 }
//             })
//             .collect::<Vec<_>>();

//         self.entry_points
//             .extend(entry_points.iter().map(|req| req.id));

//         entry_points
//     }

//     fn compile_func(&mut self, current_func: VRef<CompiledFunc>, later_init: &mut LaterInit) {
//         if let Err(err) = later_init.context.compile(&*later_init.object_context.isa) {
//             println!("Failed to compile: {:?}", err);
//             println!("{}", later_init.context.func.display());
//             panic!();
//         }

//         self.gen
//             .save_compiled_code(current_func, &later_init.context)
//             .unwrap();

//         later_init.context.clear();
//     }

//     fn generate_entry_point(&mut self, later_init: &mut LaterInit) {
//         let default_cc = later_init.object_context.isa.default_call_conv();

//         later_init.context.clear();
//         later_init.context.func.signature.clear(default_cc);

//         let dummy = FuncMir::default();
//         let mut builder = GenBuilder::new(
//             &later_init.jit_isa,
//             &dummy,
//             &mut later_init.context.func,
//             &mut later_init.func_ctx,
//         );

//         let entry_point = builder.create_block();
//         builder.append_block_params_for_function_params(entry_point);
//         builder.switch_to_block(entry_point);

//         for func in self.entry_points.drain(..) {
//             let (func_ref, _) = generator!(self, self.jit_resources).import_compiled_func(
//                 func,
//                 iter::empty(),
//                 &mut builder,
//             );
//             builder.ins().call(func_ref, &[]);
//             builder.ins().return_(&[]);
//         }

//         let func_id = self.typec.funcs.push(Func {
//             visibility: FuncVisibility::Exported,
//             name: self.interner.intern(gen::ENTRY_POINT_NAME),
//             ..default()
//         });
//         let entry_point = self.gen.compiled_funcs.insert_unique(
//             self.interner.intern(gen::ENTRY_POINT_NAME),
//             CompiledFunc::new(func_id),
//         );

//         self.compile_func(entry_point, later_init);

//         later_init
//             .object_context
//             .load_functions(
//                 iter::once(entry_point),
//                 &self.gen,
//                 &self.typec,
//                 &self.interner,
//             )
//             .unwrap();
//     }

//     fn compute_func_constants(
//         &mut self,
//         target_func: VRef<CompiledFunc>,
//         later_init: &mut LaterInit,
//     ) {
//         let target_func = self.gen.compiled_funcs[target_func].func;

//         if self
//             .mir
//             .bodies
//             .get(target_func)
//             .map_or(false, |body| body.constants.is_empty())
//         {
//             return;
//         }

//         let body = self
//             .mir
//             .bodies
//             .remove(target_func)
//             .expect("Should be generated.");

//         let mut compile_queue = vec![];

//         let root_func = self
//             .gen
//             .compiled_funcs
//             .get_or_insert(self.interner.intern("anon_const"), |_| {
//                 CompiledFunc::new(Func::ANON_TEMP)
//             });

//         let mut signature = Signature {
//             cc: self.interner.intern("windows_fastcall").into(),
//             ..default()
//         };

//         for (key, const_mir) in body.constants.iter() {
//             let root_block = const_mir.block;
//             signature.ret = body.dependant_types[const_mir.ty].ty;

//             let mut builder = GenBuilder::new(
//                 &later_init.jit_isa,
//                 &body,
//                 &mut later_init.context.func,
//                 &mut later_init.func_ctx,
//             );

//             generator!(self, self.jit_resources).generate(signature, &[], root_block, &mut builder);

//             write!(
//                 self.functions,
//                 "const {}\n\n",
//                 later_init.context.func.display()
//             )
//             .unwrap();

//             self.compile_func(root_func, later_init);

//             let next_iter = self
//                 .compile_requests
//                 .queue
//                 .drain(..)
//                 .map(|request| request.id);

//             compile_queue.extend(next_iter);

//             let mut compiled_funcs = vec![];

//             while let Some(current_func) = compile_queue.pop() {
//                 compiled_funcs.push(current_func);

//                 let CompiledFunc { func, .. } = self.gen.compiled_funcs[current_func];
//                 let Func {
//                     signature,
//                     visibility,
//                     ..
//                 } = self.typec.funcs[func];

//                 if visibility == FuncVisibility::Imported {
//                     continue;
//                 }

//                 let fall_back = (func == target_func).then_some(&body);
//                 let body = self
//                     .mir
//                     .bodies
//                     .get(func)
//                     .or(fall_back)
//                     .expect("should be generated");

//                 let root_block = body
//                     .blocks
//                     .keys()
//                     .next()
//                     .expect("function without blocks is invalid");

//                 let mut builder = GenBuilder::new(
//                     &later_init.jit_isa,
//                     body,
//                     &mut later_init.context.func,
//                     &mut later_init.func_ctx,
//                 );

//                 generator!(self, self.jit_resources).generate(
//                     signature,
//                     &[],
//                     root_block,
//                     &mut builder,
//                 );

//                 write!(self.functions, "{}\n\n", later_init.context.func.display()).unwrap();

//                 self.compile_func(current_func, later_init);

//                 let next_iter = self
//                     .compile_requests
//                     .queue
//                     .drain(..)
//                     .map(|request| request.id);

//                 compile_queue.extend(next_iter);
//             }

//             later_init
//                 .jit_context
//                 .load_functions(
//                     compiled_funcs,
//                     &self.gen,
//                     &self.typec,
//                     &self.interner,
//                     false,
//                 )
//                 .unwrap();
//             later_init
//                 .jit_context
//                 .load_functions(
//                     iter::once(root_func),
//                     &self.gen,
//                     &self.typec,
//                     &self.interner,
//                     true,
//                 )
//                 .unwrap();

//             later_init.jit_context.prepare_for_execution();

//             let constant = if body.dependant_types[const_mir.ty].ty == Ty::UINT {
//                 let func = unsafe { later_init.jit_context.get_function::<ConstUsize>(root_func) }
//                     .unwrap();
//                 GenFuncConstant::Int(func.call() as u64)
//             } else if body.dependant_types[const_mir.ty].ty == Ty::U32 {
//                 let func =
//                     unsafe { later_init.jit_context.get_function::<ConstU32>(root_func) }.unwrap();
//                 GenFuncConstant::Int(func.call() as u64)
//             } else {
//                 todo!()
//             };

//             self.object_resources.func_constants[key] = constant.into();
//         }

//         self.mir.bodies.insert(target_func, body);
//     }

//     fn jit_compile(&mut self, compile_queue: &mut Vec<CompileRequest>, later_init: &mut LaterInit) {
//         let mut compiled_funcs = vec![];

//         while let Some(current) = compile_queue.pop() {
//             compiled_funcs.push(current.id);

//             let CompiledFunc { func, .. } = self.gen.compiled_funcs[current.id];
//             let Func {
//                 signature,
//                 visibility,
//                 ..
//             } = self.typec.funcs[func];

//             if visibility == FuncVisibility::Imported {
//                 continue;
//             }

//             let body = self.mir.bodies.get_mut(func).expect("should be generated");

//             self.mir_type_swapper.swap(
//                 body,
//                 &self.compile_requests.ty_slices[current.params],
//                 &mut self.typec,
//                 &mut self.interner,
//             );

//             let root_block = body
//                 .blocks
//                 .keys()
//                 .next()
//                 .expect("function without blocks is invalid");

//             let mut builder = GenBuilder::new(
//                 &later_init.jit_isa,
//                 body,
//                 &mut later_init.context.func,
//                 &mut later_init.func_ctx,
//             );

//             generator!(self, self.jit_resources).generate(signature, &[], root_block, &mut builder);

//             self.mir_type_swapper.swap_back(body);

//             write!(
//                 self.functions,
//                 "jit {}\n\n",
//                 later_init.context.func.display()
//             )
//             .unwrap();

//             self.compile_func(current.id, later_init);

//             compile_queue.append(&mut self.compile_requests.queue);
//         }

//         later_init
//             .jit_context
//             .load_functions(
//                 compiled_funcs,
//                 &self.gen,
//                 &self.typec,
//                 &self.interner,
//                 false,
//             )
//             .unwrap();

//         later_init.jit_context.prepare_for_execution();
//     }

//     fn jit_compile_macros(&mut self, module: VRef<Module>, later_init: &mut LaterInit) {
//         let mut compile_queue = vec![];
//         let mut token_macros = vec![];
//         for &token_macro in &self.typec.module_items[module].macros {
//             let Impl {
//                 methods,
//                 key: ImplKey { ty, .. },
//                 ..
//             } = self.typec[token_macro];
//             let method_order = [
//                 Interner::NEW,
//                 Interner::START,
//                 Interner::NEXT,
//                 Interner::CLEAR,
//                 Interner::DROP,
//             ];
//             let funcs = method_order
//                 .zip(self.typec[methods].try_into().unwrap())
//                 .map(|(name, method)| {
//                     assert!(self.typec[method].name == name);
//                     let id = Generator::func_instance_name(
//                         true,
//                         later_init.jit_isa.triple,
//                         method,
//                         iter::empty(),
//                         &self.typec,
//                         &mut self.interner,
//                     );
//                     let id = self
//                         .gen
//                         .compiled_funcs
//                         .insert_unique(id, CompiledFunc::new(method));
//                     compile_queue.push(CompileRequest {
//                         id,
//                         func: method,
//                         params: default(),
//                     });
//                     id
//                 });
//             token_macros.push((ty, funcs));
//         }

//         self.jit_compile(&mut compile_queue, later_init);
//     }
// }

// impl Scheduler for TestState {
//     fn loader(&mut self) -> packaging::PackageLoader {
//         let packs = package_loader!(self);
//         // packs.token_macro_ctx = Some(&mut self.token_macro_ctx);
//         packs
//     }

//     fn init(&mut self, _: &Path) {
//         self.typec.init(&mut self.interner);
//     }

//     fn before_parsing(&mut self, module: storage::VRef<Module>) {
//         typec::build_scope(
//             module,
//             &mut self.scope,
//             &self.resources,
//             &self.typec,
//             &mut self.interner,
//         );
//     }

//     fn parse_segment(&mut self, module: storage::VRef<Module>, items: GroupedItemsAst) {
//         let mut later_init = self.get_init_later();
//         let arena = Arena::new();
//         unsafe {
//             later_init.jit_context.clear_temp();
//         }

//         {
//             let mut type_checked_funcs = bumpvec![];

//             let mut buff = String::new();
//             ty_checker!(self, module)
//                 .execute(
//                     &arena,
//                     items,
//                     &mut self.typec_ctx,
//                     self.ast_transfer.activate(),
//                     &mut type_checked_funcs,
//                 )
//                 .display_funcs(&type_checked_funcs, &mut buff)
//                 .unwrap();
//             let funcs = type_checked_funcs
//                 .iter()
//                 .map(|&(f, _)| f)
//                 .collect::<Vec<_>>();
//             mir_checker!(self, module)
//                 .funcs(&mut self.mir_ctx, &mut type_checked_funcs)
//                 .display_funcs(&funcs, &mut buff)
//                 .unwrap();
//             // println!("{}", buff);
//             let source = self.resources.modules[module].source;
//             generator!(self, self.object_resources).check_casts(
//                 source,
//                 &mut self.workspace,
//             )
//         }

//         if self.workspace.has_errors() {
//             return;
//         }

//         self.jit_compile_macros(module, &mut later_init);

//         let mut compiled_funcs = vec![];
//         let mut compile_queue = self.collect_entry_points(later_init.object_context.isa.triple);

//         while let Some(request) = compile_queue.pop() {
//             compiled_funcs.push(request);

//             let Func {
//                 signature,
//                 visibility,
//                 ..
//             } = self.typec.funcs[request.func];

//             if visibility == FuncVisibility::Imported {
//                 continue;
//             }

//             self.compute_func_constants(request.id, &mut later_init);

//             let body = self
//                 .mir
//                 .bodies
//                 .get_mut(request.func)
//                 .expect("should be generated");

//             self.mir_type_swapper.swap(
//                 body,
//                 &self.compile_requests.ty_slices[request.params],
//                 &mut self.typec,
//                 &mut self.interner,
//             );

//             let root_block = body
//                 .blocks
//                 .keys()
//                 .next()
//                 .expect("function without blocks is invalid");

//             let params = self.compile_requests.ty_slices[request.params].to_vec();

//             let mut builder = GenBuilder::new(
//                 &later_init.object_context.isa,
//                 body,
//                 &mut later_init.context.func,
//                 &mut later_init.func_ctx,
//             );

//             generator!(self, self.object_resources).generate(
//                 signature,
//                 &params,
//                 root_block,
//                 &mut builder,
//             );

//             self.mir_type_swapper.swap_back(body);

//             write!(self.functions, "{}\n\n", later_init.context.func.display()).unwrap();

//             self.compile_func(request.id, &mut later_init);

//             let next_iter = self.compile_requests.queue.drain(..);

//             compile_queue.extend(next_iter);
//         }

//         later_init
//             .object_context
//             .load_functions(
//                 compiled_funcs.iter().map(|r| r.id),
//                 &self.gen,
//                 &self.typec,
//                 &self.interner,
//             )
//             .unwrap();

//         self.later_init = Some(later_init);
//     }

//     fn finally(&mut self) {
//         // drop(mem::take(&mut self.token_macro_ctx));

//         self.workspace.push(snippet! {
//             info: ("mir repr of functions:\n{}", self.functions);
//         });

//         if self.entry_points.is_empty() {
//             self.workspace.push(snippet! {
//                 err: "no entry points found";
//                 help: "add '#[entry]' to a function that should take this role";
//             });
//         }

//         if self.workspace.has_errors() {
//             return;
//         }

//         let Some(mut later_init) = self.later_init.take() else {
//             return;
//         };

//         self.generate_entry_point(&mut later_init);

//         let thread_id = std::thread::current().id().as_u64().to_string();
//         let exe_path = format!("o-{}.exe", thread_id);
//         let obj_path = format!("o-{}.obj", thread_id);

//         let emitted = later_init.object_context.emit().unwrap();
//         fs::write(&obj_path, emitted).unwrap();

//         let host = Triple::host().to_string();
//         let target = later_init.object_context.isa.triple().to_string();

//         let compiler = cc::Build::new()
//             .opt_level(0)
//             .target(&target)
//             .host(&host)
//             .cargo_metadata(false)
//             .get_compiler();

//         let args = if compiler.is_like_msvc() {
//             vec![
//                 "ucrt.lib".into(),
//                 format!("-link /ENTRY:{} /SUBSYSTEM:CONSOLE", gen::ENTRY_POINT_NAME,),
//             ]
//         } else if compiler.is_like_clang() {
//             todo!()
//         } else if compiler.is_like_gnu() {
//             todo!()
//         } else {
//             unimplemented!("unknown compiler");
//         };

//         compiler
//             .to_command()
//             .arg(&obj_path)
//             .args(args)
//             .status()
//             .unwrap();

//         let path = Path::new(&exe_path).canonicalize().unwrap();

//         let output = Command::new(path)
//             .current_dir(std::env::current_dir().unwrap())
//             .output()
//             .unwrap();

//         fs::remove_file(obj_path).unwrap();
//         fs::remove_file(exe_path).unwrap();

//         self.workspace.push(snippet! {
//             info: ("status: {:x?}", output.status.code().unwrap());
//             info: ("stdout:\n{}", String::from_utf8_lossy(&output.stdout));
//             info: ("stderr:\n{}", String::from_utf8_lossy(&output.stderr));
//         });
//     }
// }

// fn main() {
//     gen_test! {
//         TestState,
//         false,
//         simple "functions" {
//             #[entry];
//             fn main -> uint => pass(0);
//             fn pass(a: uint) -> uint { return a };
//             fn pass_with_implicit_return(a: uint) -> uint { a };
//         }

//         simple "recursion" {
//             #[entry];
//             fn main -> uint => 0;

//             fn infinity(a: uint) => infinity(a);
//         }

//         simple "operators" {
//             #[entry];
//             fn main -> uint => 1 + 2 * 2 - 4 / 2 - 3;
//         }

//         simple "compile-time" {
//             fn sub(a: uint, b: uint) -> uint => a - b;

//             #[entry];
//             fn main -> uint => const sub(1, 1);
//         }

//         // simple "external" {
//         //     fn "default" putchar(c: char) -> u32 extern;

//         //     #[entry];
//         //     fn main -> uint {
//         //         const putchar('a'); // compile time print
//         //         putchar('\n');
//         //         0
//         //     };
//         // }

//         simple "generic" {
//             fn [T] pass(value: T) -> T => value;

//             #[entry];
//             fn main -> u32 => pass(0uint);
//         }

//         simple "struct-constructor" {
//             struct OnStack {
//                 a: uint;
//                 b: uint
//             };

//             struct InRegister {
//                 a: u32;
//                 b: u32
//             };

//             struct [T, E] Generic {
//                 a: T;
//                 b: E
//             };

//             #[entry];
//             fn main -> uint {
//                 Generic::{
//                     a: OnStack::{ a: 1; b: 2 };
//                     b: InRegister::{ a: 3; b: 1 }
//                 };
//                 0
//             };
//         }
//         simple "auto-ref-deref" {
//             impl uint {
//                 fn reference(s: ^^^^^^^^^^^^Self) -> ^^^^^^^^^^^^Self => s;
//                 fn dereference(s: Self) -> Self => s;
//             };

//             #[entry];
//             fn main -> uint => 0.reference().dereference();
//         }

//         simple "additional-param-garbage" {
//             fn [T] pass(value: T) -> T => value;

//             struct B;

//             #[entry];
//             fn main -> uint => pass::[uint, B](0uint, 'h');
//         }

//         simple "spec-test" {
//             spec Flood {
//                 fn new -> uint;
//             };

//             struct Fool;

//             impl Flood for Fool {
//                 fn new -> uint => 0;
//             };

//             fn [T: Flood] make_flood() -> uint => T::new();

//             #[entry];
//             fn main -> uint => make_flood::[Fool]();
//         }

//         simple "generic-spec" {
//             spec [T] GenericSpec {
//                 fn take(t: T) -> Self;
//             };

//             impl GenericSpec[uint] for uint {
//                 fn take(t: uint) -> Self => t;
//             };

//             fn [B, T: GenericSpec[B]] take(t: B) -> T => T::take(t);

//             #[entry];
//             fn main() -> uint => take(0);
//         }

//         simple "struct access" {
//             struct Foo {
//                 a: uint;
//                 b: uint;
//             };

//             #[entry];
//             fn main -> uint => Foo::{ a: 1; b: 0 }.b;
//         }

//         simple "register struct init and use" {
//             struct RegStruct {
//                 field: uint
//             };

//             struct RegStruct2 {
//                 field: u32;
//                 field2: u32
//             };

//             #[entry];
//             fn main -> uint {
//                 RegStruct2::{ field: 0; field2: 1 };
//                 RegStruct::{ field: 0 }.field
//             }
//         }

//         simple "match" {
//             struct Matched {
//                 a: uint;
//                 b: uint
//             };

//             #[entry];
//             fn main() -> uint => match Matched::{ a: 0; b: 1 } {
//                 ::{ a: 1; b: 0 } => 1;
//                 ::{ a: 0; b: 1 } => 0;
//                 ::{ a; b: 0 } => a;
//                 ::{ a; b } => a + b;
//             };
//         }

//         simple "match-with-struct-return" {
//             struct Returned {
//                 a: uint;
//                 b: uint
//             };

//             #[entry];
//             fn main() -> uint => match 0 {
//                 0 => Returned::{ a: 0; b: 1 };
//                 a => Returned::{ a; b: 0 };
//             }.a;
//         }

//         simple "recursive-fib" {
//             #[entry];
//             fn main -> uint => fib(10) - 55;

//             fn fib(x: uint) -> uint => match x {
//                 0 => 0;
//                 1 => 1;
//                 a => fib(a - 1) + fib(a - 2);
//             };
//         }

//         simple "enum" {
//             enum [T] Option {
//                 Some: T;
//                 None;
//             };

//             #[entry];
//             fn main() -> uint => match Option::Some~0 {
//                 ::Some~4 => 5;
//                 ::Some~1 => 2;
//                 ::None => 3;
//                 ::Some~a => a;
//             }
//         }

//         simple "enum-stress" {
//             enum [T] Option {
//                 None;
//                 Some: T;
//             };

//             #[entry];
//             fn main() -> uint => match Option::Some~Option::Some~Option::Some~0 {
//                 ::Some~::None => 5;
//                 ::Some~::Some~::None => 2;
//                 ::None => return 3;
//                 ::Some~a => match a {
//                     ::Some~::Some~a => a;
//                     ::Some~::None => 6;
//                     ::None => 1;
//                 };
//             }
//         }

//         simple "if-statement" {
//             #[entry];
//             fn main() -> uint =>
//                 if 0 == 0 => 0;
//                 elif 0 == 69 => 89;
//                 else => 1;
//         }

//         simple "let-binding" {
//             struct A {
//                 a: uint;
//                 b: uint;
//             };

//             #[entry];
//             fn main() -> uint {
//                 let ::{ mut a, b } = A::{ a: 0; b: 3 };
//                 a = a + b;
//                 a - 3
//             };
//         }

//         simple "cast" {
//             #[entry];
//             fn main() -> uint => cast(0);
//         }

//         simple "cast-mismatch" {
//             #[entry];
//             fn main() -> u32 => cast(0);

//             fn [F, T] my_cast(value: F) -> T => cast(value);
//         }

//         // simple "swap-macro" {
//         //     use {
//         //         "water/option";
//         //         "water/macros/tokens";
//         //     };
//         //     // use {
//         //     //     w "water"
//         //     // };

//         //     // TODO: Solution for macro name collisions
//         //     // type WSwap = w::Swap[uint];
//         //     // break;

//         //     struct LastToken {
//         //         last: MacroToken;
//         //     };

//         //     struct TwoTokens {
//         //         second: MacroToken;
//         //         first: MacroToken;
//         //     };

//         //     enum Swap {
//         //         Two: TwoTokens;
//         //         Last: LastToken;
//         //         Empty;
//         //     };

//         //     fn "default" malloc(size: uint) -> ^() extern;
//         //     fn "default" free(ptr: ^()) extern;

//         //     impl TokenMacro for Swap {
//         //         fn new() -> ^Self => cast(malloc(sizeof::[Self]()));

//         //         fn start(s: ^Self, lexer: MacroLexer) -> bool {
//         //             *s = ::Two~::{
//         //                 first: lexer.next();
//         //                 second: lexer.next();
//         //             };
//         //             true
//         //         };

//         //         fn next(s: ^Self, lexer: MacroLexer) -> Option[MacroToken] =>
//         //             ::Some~match *s {
//         //                 ::Two~::{ first, second } {
//         //                     *s = ::Last~::{ last: first };
//         //                     second
//         //                 };
//         //                 ::Last~::{ last } {
//         //                     *s = ::Empty;
//         //                     last
//         //                 };
//         //                 ::Empty => return ::None;
//         //             };

//         //         fn clear(s: ^Self) {};

//         //         fn drop(s: ^Self) => free(cast(s));
//         //     };

//         //     break;

//         //     #[entry];
//         //     swap! swap! fn -> main uint => 0;
//         //     //fn swap! -> main uint => 0;
//         //     //fn main -> uint => 0;
//         // }
//     }
// }

fn main() {}
