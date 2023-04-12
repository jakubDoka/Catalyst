use std::ops::Not;

use proc_macro::TokenStream;
use syn::DeriveInput;

pub fn derive(input: DeriveInput) -> TokenStream {
    let DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data,
    } = input;
    match data {
        syn::Data::Struct(s) => derive_struct(s, ident, generics, vis, attrs),
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    }
}

fn derive_struct(
    s: syn::DataStruct,
    ident: syn::Ident,
    generics: syn::Generics,
    vis: syn::Visibility,
    attrs: Vec<syn::Attribute>,
) -> TokenStream {
    match s.fields {
        syn::Fields::Named(named) => derive_named_struct(ident, named, generics, vis, attrs),
        syn::Fields::Unnamed(unnamed) => derive_tuple_struct(ident, unnamed, generics, vis, attrs),
        syn::Fields::Unit => derive_unit_struct(ident),
    }
}

fn derive_named_struct(
    ident: syn::Ident,
    named: syn::FieldsNamed,
    mut generics: syn::Generics,
    _vis: syn::Visibility,
    _attrs: Vec<syn::Attribute>,
) -> TokenStream {
    let field_names = named.named.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let used_fields = field_names
        .iter()
        .zip(named.named.iter())
        .filter_map(|(i, f)| is_ignored(f).not().then_some(i))
        .collect::<Vec<_>>();
    let ignored_fields = field_names
        .iter()
        .zip(named.named.iter())
        .filter_map(|(i, f)| is_ignored(f).then_some(i))
        .collect::<Vec<_>>();

    let used_fields = used_fields.as_slice();
    let ignored_fields = ignored_fields.as_slice();

    generics
        .type_params_mut()
        .for_each(|p| p.bounds.push(syn::parse_quote!(incremental::Increment)));
    let params = generics.type_params();
    let param_names = generics.type_params().map(|p| &p.ident);
    let where_clause = &generics.where_clause;

    quote::quote! {
        impl<#(#params),*> incremental::Increment for #ident<#(#param_names),*> #where_clause {
            const DEFAULT: bool = true;

            fn serialize(&self, serializer: &mut incremental::Serializer) {
                let Self { #(#used_fields),* , .. } = self;
                #(
                    incremental::Increment::serialize(#used_fields, serializer);
                )*
            }

            unsafe fn deserialize(deserializer: &mut incremental::Deserializer) -> Self {
                Self {
                    #(#used_fields: incremental::Increment::deserialize(deserializer),)*
                    #(#ignored_fields: Default::default(),)*
                }
            }

            fn size(&self) -> usize {
                let Self { #(#used_fields),* , .. } = self;
                0
                #(
                    + incremental::Increment::size(#used_fields)
                )*
            }
        }
    }
    .into()
}

fn derive_tuple_struct(
    ident: syn::Ident,
    unnamed: syn::FieldsUnnamed,
    mut generics: syn::Generics,
    _vis: syn::Visibility,
    _attrs: Vec<syn::Attribute>,
) -> TokenStream {
    let field_names = (0..unnamed.unnamed.len())
        .map(|i| syn::Ident::new(&format!("_{}", i), ident.span()))
        .collect::<Vec<_>>();
    let used_fields = field_names
        .iter()
        .zip(unnamed.unnamed.iter())
        .filter_map(|(i, f)| is_ignored(f).not().then_some(i))
        .collect::<Vec<_>>();
    let ignored_fields = field_names
        .iter()
        .zip(unnamed.unnamed.iter())
        .filter_map(|(i, f)| is_ignored(f).then_some(i))
        .collect::<Vec<_>>();

    let field_names = field_names.as_slice();
    let used_fields = used_fields.as_slice();
    let ignored_fields = ignored_fields.as_slice();

    generics
        .type_params_mut()
        .for_each(|p| p.bounds.push(syn::parse_quote!(incremental::Increment)));
    let params = generics.type_params();
    let param_names = generics.type_params().map(|p| &p.ident);
    let where_clause = &generics.where_clause;

    quote::quote! {
        impl<#(#params),*> incremental::Increment for #ident<#(#param_names),*> #where_clause {
            const DEFAULT: bool = true;

            fn serialize(&self, serializer: &mut incremental::Serializer) {
                let Self(#(#field_names),*) = self;
                #(
                    incremental::Increment::serialize(#used_fields, serializer);
                )*
            }

            unsafe fn deserialize(deserializer: &mut incremental::Deserializer) -> Self {
                #(let #used_fields = incremental::Increment::deserialize(deserializer);)*
                #(let #ignored_fields = default();)*
                Self(#(#field_names),*)
            }

            fn size(&self) -> usize {
                let Self(#(#field_names),*) = self;
                0
                #(
                    + incremental::Increment::size(#used_fields)
                )*
            }
        }
    }
    .into()
}

fn derive_unit_struct(ident: syn::Ident) -> TokenStream {
    quote::quote! {
        impl incremental::Increment for #ident {
            const DEFAULT: bool = true;

            fn serialize(&self, serializer: &mut incremental::Serializer) {}

            unsafe fn deserialize(deserializer: &mut incremental::Deserializer) -> Self {
                Self
            }

            fn size(&self) -> usize {
                0
            }
        }
    }
    .into()
}

fn is_ignored(field: &syn::Field) -> bool {
    struct Attr {
        _parens: syn::token::Paren,
        path: syn::Ident,
    }

    impl syn::parse::Parse for Attr {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let content;
            Ok(Self {
                _parens: syn::parenthesized!(content in input),
                path: content.parse()?,
            })
        }
    }

    field
        .attrs
        .iter()
        .filter(|a| a.path().get_ident().map_or(false, |i| i == "increment"))
        .filter_map(|attr| match &attr.meta {
            syn::Meta::List(list) => Some(list),
            _ => None,
        })
        .any(|list| {
            list.parse_args::<Attr>()
                .map_or(false, |a| a.path == "ignored")
        })
}
