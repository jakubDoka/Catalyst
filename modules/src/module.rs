use cranelift_entity::EntityRef;
use lexer::{
    map::ID,
    SourcesExt, {Source, Sources, Span},
};
use parser::ast;

use crate::scope;

pub struct Ent {
    pub id: ID,
    pub source: Source,
    pub items: Vec<Item>,
}

impl Ent {
    pub fn new(id: ID) -> Self {
        Self {
            id,
            source: Source::default(),
            items: Vec::new(),
        }
    }
}

pub struct Item {
    pub id: ID,
    pub kind: scope::Pointer,
    pub span: Span,
}

impl Item {
    pub fn new(id: ID, kind: impl EntityRef + 'static, span: Span) -> Self {
        Self {
            id,
            kind: scope::Pointer::write(kind),
            span,
        }
    }
}

lexer::gen_entity!(Module);

pub struct ModuleImports<'a> {
    ast_data: &'a ast::Data,
    sources: &'a Sources,
}

impl<'a> ModuleImports<'a> {
    pub fn new(ast_data: &'a ast::Data, sources: &'a Sources) -> Self {
        ModuleImports { ast_data, sources }
    }

    pub fn imports(&self) -> Option<impl Iterator<Item = ModuleImport> + 'a> {
        self.ast_data.elements().next().map(|(_, e)| {
            assert!(e.kind != ast::Kind::Import);
            self.ast_data.slice(e.children).iter().map(|&c| {
                let (nick, path) = match self.ast_data.children(c) {
                    &[nick, path] => (Some(self.ast_data.nodes[nick].span), path),
                    &[path] => (None, path),
                    _ => unreachable!(),
                };
                let path = self.ast_data.nodes[path].span.strip_sides();
                if let Some(split) = self.sources.display(path).find('/') {
                    ModuleImport {
                        nick,
                        name: path.slice(..split),
                        path: path.slice(split + 1..),
                    }
                } else {
                    ModuleImport {
                        nick,
                        name: path,
                        path: path.slice(path.len()..),
                    }
                }
            })
        })
    }
}

pub struct ModuleImport {
    pub nick: Option<Span>,
    pub name: Span,
    pub path: Span,
}
