use lexing::*;
use std::path::*;
use storage::*;

pub struct PackagingContext {
    pub modules: SparseMap<Ident, Module>,
    pub conns: CacheBumpMap<DepList, Dep>,
    pub module_order: Vec<Ident>,
}

pub struct Module {
    pub path: PathBuf,
    pub deps: Maybe<DepList>,
    pub content: String,
    pub ordering: usize,
    pub line_mapping: LineMapping,
}

pub struct Dep {
    pub name: Span,
    pub ptr: Ident,
}

gen_v_ptr!(DepList);
