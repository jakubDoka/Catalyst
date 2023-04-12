use proc_macro::TokenStream;
use syn::DeriveInput;

mod increment;

#[proc_macro_derive(Increment, attributes(increment))]
pub fn derive_increment(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).expect("Failed to parse tokens");

    let i = increment::derive(ast);

    //panic!("{}", i);

    i
}
