use lexer::{Sources, Span};
use parser::ast::{self, Ast};

pub struct Manifest<'a> {
    ast_data: &'a ast::Data,
    sources: &'a Sources,
}

impl<'a> Manifest<'a> {
    pub fn new(ast_data: &'a ast::Data, sources: &'a Sources) -> Self {
        Manifest { ast_data, sources }
    }

    pub fn get_string_tag(&self, name: &str) -> Option<&'a str> {
        self.find_tag(name)
            .map(|tag| self.sources.display(self.ast_data.span(tag).strip_sides()))
    }

    pub fn dependencies(&self) -> Option<impl Iterator<Item = ManifestDepInfo> + 'a> {
        self.find_tag("dependencies").map(|tag| {
            self.ast_data.children_iter(tag).filter_map(|elem| {
                let &[name, path] = self.ast_data.slice(elem.children) else {
                    return None;
                };

                let name = self.ast_data.span(name);
                let path = self.ast_data.span(path).strip_sides();

                let split = self.sources.display(path).find('@').unwrap_or(path.len());
                let version = path.slice(split + 1..);
                let path = path.slice(..split);

                return Some(ManifestDepInfo {
                    name,
                    path,
                    version,
                });
            })
        })
    }

    fn find_tag(&self, name: &str) -> Option<Ast> {
        for (_, ent) in self.ast_data.elements() {
            let &[nm, content] = self.ast_data.slice(ent.children) else {
                continue;
            };

            if name != self.sources.display(self.ast_data.span(nm)) {
                continue;
            };

            return Some(content);
        }

        None
    }
}

pub struct ManifestDepInfo {
    pub name: Span,
    pub path: Span,
    pub version: Span,
}
