use crate::{TypeParser, Func};
use cranelift_entity::{
    packed_option::ReservedValue,
    EntityList, ListPool, PrimaryMap, SecondaryMap,
};
use lexer::*;
use modules::*;
use parser::*;

pub struct Builder<'a> {
    pub scope: &'a mut Scope,
    pub types: &'a mut Types,
    pub sources: &'a Sources,
    pub ast: &'a ast::Data,
    pub type_ast: &'a SecondaryMap<Ty, Ast>,
    pub graph: &'a mut GenericGraph,
    pub ty: Ty,
    pub diagnostics: &'a mut errors::Diagnostics,
}

impl<'a> Builder<'a> {
    pub fn build(&mut self) -> errors::Result {
        let Ent { id, .. } = self.types.ents[self.ty];
        let ast = self.type_ast[self.ty];
        let ast::Ent { kind, span, .. } = self.ast.nodes[ast];

        match kind {
            ast::Kind::Struct => self.build_struct(id, ast)?,
            _ => todo!(
                "Unhandled type decl {:?}: {}",
                kind,
                self.sources.display(span)
            ),
        }

        Ok(())
    }

    pub fn build_struct(&mut self, id: ID, ast: Ast) -> errors::Result {
        let &[.., body] = self.ast.children(ast) else {
            unreachable!();
        };

        // fields are inserted into centralized hash map for faster lookup
        // and memory efficiency, though we still need field ordering when
        // calculating offsets
        let fields = {
            let mut fields = vec![]; // TODO: rather reuse allocation
            for &field in self.ast.children(body) {
                let children = self.ast.children(field);
                let &[.., field_ty_ast] = children else {
                    unreachable!();
                };
                let field_ty = self.parse_type(field_ty_ast)?;
                for &name in &children[..children.len() - 1] {
                    let span = self.ast.nodes[name].span;
                    let id = {
                        let str = self.sources.display(span);
                        Self::field_id(id, ID::new(str))
                    };
                    let field = Field {
                        span,
                        parent: self.ty,
                        ty: field_ty,
                        index: fields.len() as u32,
                    };
                    assert!(self.types.fields.insert(id, field).is_none());
                    fields.push(field_ty);
                    self.graph.add_edge(field_ty.as_u32());
                }
            }
            self.types.cons.list(&fields)
        };

        self.types.ents[self.ty].kind = Kind::Struct(fields);
        self.graph.close_node();

        Ok(())
    }

    pub fn field_id(ty: ID, name: ID) -> ID {
        ty + name
    }
}

impl TypeParser for Builder<'_> {
    fn state(&mut self) -> (&mut Scope, &mut Types, &Sources, &ast::Data, &mut errors::Diagnostics) {
        (self.scope, self.types, self.sources, self.ast, self.diagnostics)
    }
}

pub struct Types {
    pub ents: PrimaryMap<Ty, Ent>,
    pub funcs: ListPool<Func>,
    pub fields: Map<Field>,
    pub cons: ListPool<Ty>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            funcs: ListPool::new(),
            ents: PrimaryMap::new(),
            cons: ListPool::new(),
            fields: Map::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Field {
    pub parent: Ty,
    pub ty: Ty,
    pub index: u32,
    pub span: Span,
}

impl ReservedValue for Field {
    fn reserved_value() -> Self {
        Field {
            parent: Ty::reserved_value(),
            ty: Ty::reserved_value(),
            span: Span::reserved_value(),
            index: u32::MAX,
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.ty.is_reserved_value() && self.index == u32::MAX
    }
}

#[derive(Default)]
pub struct Ent {
    pub id: ID,
    pub name: Span,
    pub kind: Kind,
}

impl Ent {
    pub fn new(kind: Kind, id: ID) -> Self {
        Self {
            id,
            kind,
            name: Default::default(),
        }
    }
}

pub struct Display<'a> {
    types: &'a Types,
    sources: &'a Sources,
    ty: Ty,
}

impl<'a> Display<'a> {
    pub fn new(types: &'a Types, sources: &'a Sources, ty: Ty) -> Self {
        Self { types, ty, sources }
    }
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut str = String::new();
        self.ty.display(self.types, self.sources, &mut str)?;
        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Bound(EntityList<Func>),
    Struct(EntityList<Ty>),
    Int(i16),
    Bool,
    Nothing,
    Unresolved,
}

impl Default for Kind {
    fn default() -> Self {
        Kind::Unresolved
    }
}

lexer::gen_entity!(Ty);

impl Ty {
    pub fn display(self, types: &Types, sources: &Sources, to: &mut String) -> std::fmt::Result {
        use std::fmt::Write;
        match types.ents[self].kind {
            Kind::Struct(_) | Kind::Bound(_) => {
                let name = types.ents[self].name;
                write!(to, "{}", sources.display(name))?;
            }
            Kind::Int(base) => {
                if base > 0 {
                    write!(to, "i{}", base)?;
                } else {
                    write!(to, "int")?;
                }
            }
            Kind::Bool => write!(to, "bool")?,
            Kind::Nothing => write!(to, "nothing")?,
            Kind::Unresolved => write!(to, "unresolved")?,
        }

        Ok(())
    }
}
