use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn r#struct(&mut self, vis: Option<VisAst<M>>) -> Option<StructAst<'arena, M>> {
        Some(StructAst {
            vis,
            r#struct: self.advance().source_meta(),
            generics: self.generics()?,
            name: self.name("struct")?,
            body: self.opt_object("struct body", Self::struct_field)?,
        })
    }

    fn struct_field(&mut self) -> Option<StructFieldAst<'arena, M>> {
        Some(StructFieldAst {
            vis: self.vis(),
            used: self.try_advance(Tk::Use),
            mutable: self.try_advance(Tk::Mut),
            name: self.name("struct field")?,
            colon: self.expect(Tk::Colon, |ctx| MissingColon {
                something: "struct field",
                found: ctx.state.current.kind,
                loc: ctx.loc(),
            })?,
            ty: self.ty()?,
        })
    }
}
