use proc_macro::TokenStream;
use syn::{
    parse_macro_input, DeriveInput, Data, DataStruct, Fields,
    Ident, Type, parse_quote, PathArguments, GenericArgument,
    Error,
};
use quote::{quote, format_ident};

struct ParsedField {
    pub ident: Ident,
    pub ty: Type,
    pub is_optional: bool,
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // We first parse the input as the input of a derive macro
    let input = parse_macro_input!(input as DeriveInput);
    let struct_ident = &input.ident;

    let fields = match &input.data {
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => &fields.named,
        _ => return Error::new_spanned(&input, "Expected a named struct").to_compile_error().into(),
    };

    // We build the new struct
    let parsed_fields: Vec<_> = fields.iter()
        .map(|field| {
            let (is_optional, ty) = match_against_option(&field.ty);
            ParsedField {
                ident: field.ident.clone().unwrap(),
                ty,
                is_optional,
            }
        })
        .collect();
    let field_idents: Vec<&Ident> = parsed_fields.iter().map(|field| &field.ident).collect();
    let field_types: Vec<&Type> = parsed_fields.iter().map(|field| &field.ty).collect();

    let builder_ident = format_ident!("{}Builder", struct_ident);
    let builder_struct = quote! {
        pub struct #builder_ident {
            #(#field_idents: Option<#field_types>),*
        }
    };

    let setter_methods = quote! {
        #(
            fn #field_idents(&mut self, #field_idents: #field_types) -> &mut Self {
                self.#field_idents = Some(#field_idents);
                self
            }
        )*
    };

    let required_idents: Vec<&Ident> = parsed_fields
        .iter()
        .filter(|field| !field.is_optional)
        .map(|field| &field.ident)
        .collect();
    let optional_idents: Vec<&Ident> = parsed_fields
        .iter()
        .filter(|field| field.is_optional)
        .map(|field| &field.ident)
        .collect();
    let build_method = quote! {
        pub fn build(&mut self) -> Result<#struct_ident, Box<dyn std::error::Error>> {
            if #(self.#required_idents.is_none())||* {
                return Err("All non-optional fields should be set".into());
            }

            Ok(#struct_ident {
                #( #required_idents: self.#required_idents.clone().unwrap() ),*,
                #( #optional_idents: self.#optional_idents.clone() ),*
            })
        }
    };

    let builder_impl = quote! {
        impl #builder_ident {
            #setter_methods
            #build_method
        }
    };

    let builder_method = quote! {
        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_idents: None),*
                }
            }
        }
    };

    let expanded = quote! {
        #builder_struct
        #builder_impl
        #builder_method
    };

    expanded.into()
}

/// Returns `true` if the passed type is an `Option` and if it is, it returns
/// the inner type.
fn match_against_option(ty: &Type) -> (bool, Type) {
    let option_ident: Ident = parse_quote!(Option);
    match ty {
        Type::Path(type_path) => {
            for segment in type_path.path.segments.iter() {
                if segment.ident == option_ident {
                    let inner_type = match &segment.arguments {
                        PathArguments::AngleBracketed(arguments) => {
                            match arguments.args.first()
                                .expect("Option's have one generic argument") {
                                GenericArgument::Type(the_actual_type) => the_actual_type,
                                _ => panic!("Option's generic should be a type"),
                            }
                        },
                        _ => panic!("Option's have generics"),
                    };
                    return (true, inner_type.clone());
                };
            };

            (false, ty.clone())
        },
        _ => panic!("Expected a path"),
    }
}
