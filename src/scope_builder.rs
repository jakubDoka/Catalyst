use crate::*;

impl MainScopeBuilder<'_> {
    /// The module imports are read from temporary ast and minimal data is preserved.
    ///
    /// TODO: Could there be an option to optimize this?
    pub fn build_scope(&mut self, source: Source) {
        for item in self.modules[self.builtin_source.source].items.iter() {
            self.scope
                .insert(&mut self.diagnostics, source, item.id, item.to_scope_item())
                .unwrap();
        }

        self.scope.dependencies.clear();
        if let Some(imports) = ModuleImports::new(&self.ast_data, &self.sources).imports() {
            for import in imports {
                let nick = self.sources.display(import.nick);
                let Some(&dep) = self.module_map.get((nick, source)) else {
                    continue; // recovery, module might not exist due to previous recovery
                };
                self.scope
                    .insert(
                        &mut self.diagnostics,
                        source,
                        nick,
                        ScopeItem::new(dep, import.nick),
                    )
                    .unwrap();
                for item in self.modules[dep].items.iter() {
                    drop(self.scope.insert(
                        &mut self.diagnostics,
                        source,
                        item.id,
                        item.to_scope_item(),
                    ));
                }
                self.scope.dependencies.push((dep, import.nick));
            }
        }
    }
}