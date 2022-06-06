use crate::{state::Logger, *};

impl Logger<'_> {
    /// Function iterates trough all possible diagnostic options,
    /// logs them and if errors are encountered, exits.
    pub fn log(&self) {
        if self.diagnostics.is_empty() {
            return;
        }

        let mut errors = String::new();

        self.diagnostics
            .iter::<AstError>()
            .map(|errs| errs.for_each(|err| err.display(&self.sources, &mut errors).unwrap()));

        self.diagnostics.iter::<ModuleError>().map(|errs| {
            errs.for_each(|err| {
                modules::error::display(err, &self.sources, &self.units, &mut errors).unwrap()
            })
        });

        self.diagnostics.iter::<TyError>().map(|errs| {
            errs.for_each(|err| {
                typec::error::display(
                    err,
                    &self.sources,
                    &self.types,
                    &self.ty_lists,
                    &self.ty_comps,
                    &mut errors,
                )
                .unwrap()
            })
        });

        self.diagnostics.iter::<InstError>().map(|errs| {
            errs.for_each(|err| {
                instance::error::display(
                    &err,
                    &self.types,
                    &self.ty_lists,
                    &self.sources,
                    &mut errors,
                )
                .unwrap()
            })
        });

        println!("{errors}");
        exit!();
    }
}
