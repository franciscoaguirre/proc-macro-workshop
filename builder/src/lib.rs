use proc_macro::TokenStream;
use syn::{
    parse_macro_input, DeriveInput, Data, DataStruct, Fields,
    Ident,
};
use quote::quote;
use proc_macro2::Span;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // We first parse the input as the input of a derive macro
    let input = parse_macro_input!(input as DeriveInput);

    // We then gather what we need from the struct
    let struct_ident = &input.ident;
    let fields = if let Data::Struct(DataStruct {
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
    let mut field_types = Vec::new();
    for field in fields.iter() {
        field_idents.push(field.ident.clone().expect("Struct is named"));
        field_types.push(field.ty.clone());
    };

    let expanded = quote! {
        use std::error::Error;

        pub struct #builder_ident {
            #(#field_idents: Option<#field_types>),*
        }

        impl #builder_ident {
            #(
                fn #field_idents(&mut self, #field_idents: #field_types) -> &mut Self {
                    self.#field_idents = Some(#field_idents);
                    self
                }
            )*

            pub fn build(&mut self) -> Result<#struct_ident, Box<dyn Error>> {
                if #(self.#field_idents.is_none())||* {
                    return Err("All fields should be set".into());
                }

                Ok(#struct_ident {
                    #(#field_idents: self.#field_idents.clone().expect("Field existence was checked")),*
                })
            }
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
