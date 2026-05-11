use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Error, Fields};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_ident = &input.ident;

    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => {
            return Error::new_spanned(&input, "Expected a named struct")
                .to_compile_error()
                .into()
        }
    };

    let field_names = fields.iter().map(|f| &f.ident);
    let field_name_strs = fields.iter().map(|f| f.ident.as_ref().unwrap().to_string());

    let debug_impl = quote! {
        impl std::fmt::Debug for #struct_ident {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_ident))
                    #(.field(#field_name_strs, &self.#field_names))*
                    .finish()
            }
        }
    };

    debug_impl.into()
}
