use diags::*;
use packaging_t::Source;

use super::*;

impl<'ctx, 'arena, M: TokenMeta> Parser<'ctx, 'arena, M> {
    pub fn grouped_items(&mut self) -> Option<GroupedItemsAst<'arena, M>> {
        let (items, last, ..) = self.items()?;

        macro_rules! gen_groups {
            (
                $(
                    $name:ident: $enum_name:ident,
                )*
            ) => {
                let mut attrs = bumpvec![];
                $(
                    let mut $name = bumpvec![];
                )*

                for &item in items.iter() {
                    match item {
                        $(
                            ItemAst::$enum_name(&s) => $name.push((s, self.arena.alloc_iter(attrs.drain(..)))),
                        )*
                        ItemAst::Attribute(&a) => attrs.push(a),
                    }
                }

                if let Some(span) = attrs.into_iter().map(|a| a.span()).reduce(Span::joined) {
                    self.workspace.push(TrailingAttribute {
                        loc: SourceLoc {
                            span,
                            origin: self.source,
                        },
                    })?;
                }

                $(
                    let $name = self.arena.alloc_slice($name.as_slice());
                )*

                Some(GroupedItemsAst {
                    $(
                        $name,
                    )*
                    last,
                })
            };
        }

        gen_groups! {
            structs: Struct,
            funcs: Func,
            specs: Spec,
            impls: Impl,
            enums: Enum,
        }
    }

    fn items(&mut self) -> Option<(BumpVec<ItemAst<'arena, M>>, bool)> {
        let mut items = bumpvec![];
        let last = loop {
            self.skip(Tk::NewLine);

            if self.at(Tk::Eof) {
                break true;
            }

            if self.at(Tk::Break) {
                break false;
            }

            let Some(item) = self.item() else {
                self.recover_list(Tk::NewLine, Tk::Eof)?;
                continue;
            };

            items.push(item);
        };

        Some((items, last))
    }

    fn item(&mut self) -> Option<ItemAst<'arena, M>> {
        let vis = self.vis();

        branch! { self => {
            Struct => self.r#struct(vis)
                .map(|s| self.arena.alloc(s))
                .map(ItemAst::Struct),
            Func => self.func_def(vis)
                .map(|s| self.arena.alloc(s))
                .map(ItemAst::Func),
            Spec => self.spec(vis)
                .map(|s| self.arena.alloc(s))
                .map(ItemAst::Spec),
            Enum => self.r#enum(vis)
                .map(|s| self.arena.alloc(s))
                .map(ItemAst::Enum),
            Impl => self.r#impl(vis)
                .map(|s| self.arena.alloc(s))
                .map(ItemAst::Impl),
            Hash => self.top_level_attr()
                .map(|s| self.arena.alloc(s))
                .map(ItemAst::Attribute),
            @"module item",
        }}
    }

    fn r#enum(&mut self, vis: Option<VisAst<M>>) -> Option<EnumAst<'arena, M>> {
        Some(EnumAst {
            vis,
            r#enum: self.advance(),
            generics: self.generics()?,
            name: self.name("enum")?,
            body: self.opt_object("enum body", Self::enum_variant)?,
        })
    }

    fn enum_variant(&mut self) -> Option<EnumVariantAst<'arena, M>> {
        Some(EnumVariantAst {
            name: self.name("enum variant")?,
            ty: self
                .try_advance(Tk::Colon)
                .map(|colon| self.ty().map(|ty| (colon, ty)))
                .transpose()?,
        })
    }

    fn r#impl(&mut self, vis: Option<VisAst<M>>) -> Option<ImplAst<'arena, M>> {
        Some(ImplAst {
            vis,
            r#impl: self.advance(),
            generics: self.generics()?,
            target: self.impl_target()?,
            body: self.opt_object("impl body", Self::impl_item)?,
        })
    }

    fn impl_target(&mut self) -> Option<ImplTargetAst<'arena, M>> {
        self.skip(Tk::NewLine);
        let ty = self.ty()?;

        if let Some(for_) = self.try_advance(Tk::For) {
            Some(ImplTargetAst::Spec(
                match ty {
                    TyAst::Path(path) => SpecExprAst { path },
                    _ => self.workspace.push(InvalidSpecImplSyntax {
                        span: ty.span(),
                        source: self.source,
                    })?,
                },
                for_,
                self.ty()?,
            ))
        } else {
            Some(ImplTargetAst::Direct(ty))
        }
    }

    fn impl_item(&mut self) -> Option<ImplItemAst<'arena, M>> {
        let vis = self.vis();
        branch! { self => {
            Func => self.func_def(vis)
                .map(|s| self.arena.alloc(s))
                .map(ImplItemAst::Func),
            @"impl item",
        }}
    }

    fn spec(&mut self, vis: Option<VisAst<M>>) -> Option<SpecAst<'arena, M>> {
        Some(SpecAst {
            vis,
            spec: self.advance(),
            generics: self.generics()?,
            name: self.name("spec")?,
            inherits: self.param_specs()?,
            body: self.opt_object("spec body", Self::func_sig)?,
        })
    }

    fn top_level_attr(&mut self) -> Option<TopLevelAttrAst<M>> {
        Some(TopLevelAttrAst {
            hash: self.advance(),
            value: self.wrapped(
                Self::top_level_attr_kind,
                "top level attribute",
                Tk::LeftBracket,
                Tk::RightBracket,
            )?,
        })
    }

    fn top_level_attr_kind(&mut self) -> Option<TopLevelAttrKindAst<M>> {
        use TopLevelAttrKindAst::*;
        branch! {str self => {
            "entry" => Some(Entry(self.advance())),
            "water_drop" => Some(WaterDrop(
                self.advance(),
            )),
            "compile_time" => Some(CompileTime(
                self.advance(),
            )),
            "no_moves" => Some(NoMoves(self.advance())),
            "macro" => Some(Macro(self.advance(), self.name("macro")?)),
            "inline" => {
                self.at(Tk::LeftParen).then(||
                    self.wrapped(Self::inline_mode, "inline mode", Tk::LeftParen, Tk::RightParen)
                )
                .transpose()
                .map(Inline)
            },
            @"top level attribute",
        }}
    }

    fn inline_mode(&mut self) -> Option<InlineModeAst<M>> {
        Some(branch! {str self => {
            "always" => InlineModeAst::Always(self.advance()),
            "never" => InlineModeAst::Never(self.advance()),
            @"inline mode",
        }})
    }
}

ctl_errors! {
    #[err => "invalid top level attribute expected one of: {expected}"]
    error InvalidTopLevelAttribute: fatal {
        #[err loc]
        expected ref: String,
        loc: SourceLoc,
    }

    #[err => "invalid syntax for impl of spec"]
    #[info => "spec must be in form of a type path ( [\\] <ident> {{\\ ( <ident> | <generics> ) }} )"]
    error InvalidSpecImplSyntax: fatal {
        #[err source, span, "here"]
        span: Span,
        source: VRef<Source>,
    }

    #[err => "invalid inline mode expected one of: {expected}"]
    error InvalidInlineMode: fatal {
        #[err loc]
        expected ref: String,
        loc: SourceLoc,
    }

    #[err => "trailing attribute(s)"]
    #[info => "attribute must be placed before the item"]
    error TrailingAttribute: fatal {
        #[err loc]
        loc: SourceLoc,
    }
}
