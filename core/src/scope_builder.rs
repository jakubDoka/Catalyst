use crate::*;

impl MainScopeBuilder<'_> {
    /// The module imports are read from temporary ast and minimal data is preserved.
    ///
    /// TODO: Could there be an option to optimize this?
    pub fn build_scope(&mut self, source: Source) {
        for &item in self.modules[self.builtin_source.source].items.iter() {
            self.scope.insert_builtin(item);
        }

        self.scope.dependencies.clear();
        if let Some(imports) = ModuleImports::new(&self.ast_data, &self.sources).imports() {
            for import in imports {
                let nick = self.sources.display(import.nick);
                let Some(&dep) = self.module_map.get(ID::scoped(nick.into(), source)) else {
                    continue; // recovery, module might not exist due to previous recovery
                };
                self.scope.insert(
                    &mut self.diagnostics,
                    source,
                    ModuleItem::new(nick.into(), dep, import.nick),
                );
                for &item in self.modules[dep].items.iter() {
                    self.scope.insert(&mut self.diagnostics, source, item);
                }
                self.scope.dependencies.push((dep, import.nick));
            }
        }
    }
}
