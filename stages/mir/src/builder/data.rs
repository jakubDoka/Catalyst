use super::*;

impl<'i, 'm> MirBuilder<'i, 'm> {
    pub(super) fn access(
        &mut self,
        var: VRef<VarHeaderTir>,
        dest: OptVRef<ValueMir>,
        span: Span,
    ) -> NodeRes {
        let var = self.reused.var(var);
        self.inst(InstMir::Access(var, dest), span);
        if dest.is_some() {
            self.move_out(var, span);
        }
        Some(dest.unwrap_or(var))
    }

    pub(super) fn ctor(
        &mut self,
        fields: &[TirNode],
        ty: Ty,
        dest: OptVRef<ValueMir>,
        span: Span,
    ) -> NodeRes {
        let mir_fields = fields
            .iter()
            .map(|&field| self.create_value(field.ty))
            .collect::<BumpVec<_>>();

        let final_dest = dest.unwrap_or_else(|| self.create_value(ty));
        let pushed_fields = self
            .func
            .module
            .value_args
            .extend(mir_fields.iter().copied());
        self.inst(
            InstMir::Ctor(pushed_fields, final_dest, dest.is_some()),
            span,
        );

        // this ensures values are dropped if construction is not completed
        let frame = self.start_scope_frame();
        for (&field, value) in fields.iter().zip(mir_fields) {
            if let Some(value) = self.node(field, Some(value)) {
                self.reused.moves.mark_var(value);
            } else {
                self.end_scope_frame(frame, span);
                return None;
            }
        }
        self.discard_scope_frame(frame);

        Some(final_dest)
    }

    pub(super) fn deref(&mut self, node: TirNode, dest: VRef<ValueMir>, span: Span) -> NodeRes {
        let node = self.node(node, None)?;
        self.inst(InstMir::Deref(node, dest), span);
        self.connect_deref_owner(node, dest);
        Some(dest)
    }

    pub(super) fn r#ref(&mut self, node: TirNode, dest: VRef<ValueMir>, span: Span) -> NodeRes {
        let node = self.node(node, None)?;
        self.handle_referencing(node, span);
        self.inst(InstMir::Ref(node, dest), span);
        Some(dest)
    }

    pub(super) fn number<T: FromStr>(
        &mut self,
        computed: Option<T>,
        dest: VRef<ValueMir>,
        span: Span,
        to_inst: impl FnOnce(T, VRef<ValueMir>) -> InstMir,
    ) -> NodeRes
    where
        T::Err: std::fmt::Debug,
    {
        let lit = computed.unwrap_or_else(|| {
            self.ext
                .resources
                .span_str(self.meta.source, span)
                .parse()
                .expect("Lexer should have validated this.")
        });
        self.inst(to_inst(lit, dest), span);
        Some(dest)
    }

    pub(super) fn char(&mut self, dest: VRef<ValueMir>, span: Span) -> NodeRes {
        let lit = parse_char(
            self.ext
                .resources
                .span_str(self.meta.source, span.shrink(1))
                .chars()
                .by_ref(),
        )
        .expect("Lexer should have validated this.");
        self.inst(InstMir::Int(lit as i64, dest), span);
        Some(dest)
    }

    pub(super) fn bool(&mut self, value: bool, dest: VRef<ValueMir>, span: Span) -> NodeRes {
        self.inst(InstMir::Int(value as i64, dest), span);
        Some(dest)
    }

    pub(super) fn r#let(&mut self, LetTir { pat, value }: LetTir) -> NodeRes {
        let value = self.node(value, None)?;
        self.bind_pattern_vars(pat, value);
        Some(self.func.unit)
    }

    pub(super) fn assign(&mut self, AssignTir { lhs, rhs }: AssignTir, span: Span) -> NodeRes {
        let dest = self.node(lhs, None)?;
        self.move_in(dest, span);
        self.node(rhs, Some(dest))?;
        Some(self.func.unit)
    }
}
