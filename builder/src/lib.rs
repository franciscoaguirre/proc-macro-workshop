use proc_macro::TokenStream;
use syn::{
    parse_macro_input, DeriveInput, Data, DataStruct, Fields,
    Ident, parse_quote,
};
use quote::quote;
use proc_macro2::Span;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // We first parse the input as the input of a derive macro
    let input = parse_macro_input!(input as DeriveInput);

    // We then gather what we need from the struct
    let struct_ident = &input.ident;
    let mut fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(fields),
        ..
    }) = input.data {
        fields.named
    } else {
        panic!("Expected a named struct");
    };

    // We build the new struct
    let builder_ident = Ident::new(
        &format!("{}Builder", struct_ident),
        Span::call_site()
    );
    let mut field_idents = Vec::new();
    for field in fields.iter_mut() {
        let old_type = &field.ty;
        field.ty = parse_quote! { Option<#old_type> };
        field_idents.push(field.ident.clone().expect("Struct is named"));
    };

    let expanded = quote! {
        pub struct #builder_ident {
            #fields
        }

        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_idents: None),*
                }
            }
        }
    };

    expanded.into()
}
