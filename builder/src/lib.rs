use proc_macro::TokenStream;
use syn::{
    parse_macro_input, DeriveInput, Data, DataStruct, Fields,
    Ident, Type, parse_quote, PathArguments, GenericArgument,
    Error, Attribute, Meta, MetaNameValue, Expr, Lit,
};
use quote::{quote, format_ident};

struct ParsedField {
    pub ident: Ident,
    pub ty: Type,
    pub is_optional: bool,
    /// Only if it's a vec
    /// You get it with #[builder(each = "<any ident>")]
    pub individual_item_setter: Option<Ident>,
    pub individual_item_type: Option<Type>,
    pub is_vector: bool,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // We first parse the input as the input of a derive macro
    let input = parse_macro_input!(input as DeriveInput);
    let struct_ident = &input.ident;

    let fields = match &input.data {
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => &fields.named,
        _ => return Error::new_spanned(&input, "Expected a named struct").to_compile_error().into(),
    };

    // We build the new struct
    let parsed_fields: Result<Vec<_>, Error> = fields.iter()
        .map(|field| {
            let inner_type = match_against_type(parse_quote!(Option), &field.ty);
            let is_optional = inner_type.is_some();
            // These types should be paired together
            let individual_item_setter = check_builder_attribute(&field.attrs)?;
            let individual_item_type = match_against_type(parse_quote!(Vec), &field.ty);
            let is_vector = individual_item_type.is_some();
            Ok(ParsedField {
                ident: field.ident.clone().unwrap(),
                ty: if let Some(inner_type) = inner_type { inner_type } else { field.ty.clone() },
                is_optional,
                is_vector,
                individual_item_setter,
                individual_item_type,
            })
        })
        .collect();
    let parsed_fields = match parsed_fields {
        Ok(inner) => inner,
        Err(error) => return error.to_compile_error().into(),
    };
    let field_idents: Vec<&Ident> = parsed_fields.iter().map(|field| &field.ident).collect();

    let builder_ident = format_ident!("{struct_ident}Builder");

    let individual_item_setter_methods = parsed_fields.iter()
        .filter(|field| field.individual_item_setter.is_some())
        .map(|field| {
            let ParsedField {
                individual_item_setter,
                individual_item_type,
                ident,
                ..
            } = field;
            // Need to get the type inside `Vec<>`
            quote! {
                pub fn #individual_item_setter(&mut self, item: #individual_item_type) -> &mut Self {
                    self.#ident.push(item);
                    self
                }
            }
        });
    let whole_item_setter_methods = parsed_fields.iter()
        .filter(|field| {
            if let Some(setter) = &field.individual_item_setter {
                return !field_idents.contains(&setter);
            }

            true
        })
        .map(|field| {
            let ParsedField { ident, ty, is_vector, .. } = field;
            let value = if *is_vector { quote! { #ident } } else { quote! { Some(#ident) } };
            quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = #value;
                    self
                }
            }
        });

    let setter_methods = quote! {
        #( #individual_item_setter_methods )*
        #( #whole_item_setter_methods )*
    };

    let required_idents: Vec<&Ident> = parsed_fields
        .iter()
        .filter(|field| !field.is_optional && !field.is_vector)
        .map(|field| &field.ident)
        .collect();
    let optional_idents: Vec<&Ident> = parsed_fields
        .iter()
        .filter(|field| field.is_optional)
        .map(|field| &field.ident)
        .collect();
    let vector_idents: Vec<&Ident> = parsed_fields
        .iter()
        .filter(|field| field.is_vector)
        .map(|field| &field.ident)
        .collect();
    let build_method = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            if #(self.#required_idents.is_none())||* {
                return Err("All required fields should be set".into());
            }

            Ok(#struct_ident {
                #( #required_idents: self.#required_idents.clone().unwrap(), )*
                #( #optional_idents: self.#optional_idents.clone(), )*
                #( #vector_idents: self.#vector_idents.clone(), )*
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
                    #( #required_idents: None, )*
                    #( #optional_idents: None, )*
                    #( #vector_idents: std::vec::Vec::new(), )*
                }
            }
        }
    };

    let builder_fields = parsed_fields.iter().map(|field| {
        let ParsedField { ident, ty, is_vector, .. } = field;

        if *is_vector {
            quote! {
                #ident: #ty,
            }
        } else {
            quote! {
                #ident: std::option::Option<#ty>,
            }
        }
    });
    let builder_struct = quote! {
        pub struct #builder_ident {
            #( #builder_fields )*
        }
    };

    let expanded = quote! {
        #builder_struct
        #builder_impl
        #builder_method
    };

    expanded.into()
}

/// Matches against a particular type with one generic and gets the inner type,
/// works for both `Option` and `Vec`
fn match_against_type(ident_to_match: Ident, ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(type_path) => {
            for segment in type_path.path.segments.iter() {
                if segment.ident != ident_to_match {
                    return None;
                }

                let inner_type = match &segment.arguments {
                    PathArguments::AngleBracketed(arguments) => {
                        match arguments.args.first()
                            .expect("Should have one generic argument") {
                            GenericArgument::Type(the_actual_type) => the_actual_type.clone(),
                            _ => panic!("Generic should be a type"),
                        }
                    },
                    _ => panic!("Should have generics"),
                };

                return Some(inner_type)
            };

            None
        },
        _ => panic!("Expected a path"),
    }
}

/// Return the name of the setter for individual items of the Vec in case
/// the attribute `builder(each = "<any ident>")` was used
/// A success means we either found the attribute and it was well formatted
/// or we didn't find the attribute, since it's optional.
/// A failure means we found the attribute but it had a bad format.
fn check_builder_attribute(attributes: &[Attribute]) -> Result<Option<Ident>, Error> {
    let builder = match attributes.first() {
        Some(builder) => builder.clone(),
        None => return Ok(None), // It's fine to not have this attribute
    };
    match builder.meta {
        Meta::List(ref list) => {
            let pair: MetaNameValue = syn::parse2(list.tokens.clone().into())?;
            let ident_to_match: Ident = parse_quote!(each);
            if *pair.path.get_ident().unwrap() != ident_to_match {
                return Err(Error::new_spanned(&builder.meta, "expected `builder(each = \"...\")`"));
            }
            let value: Ident = match pair.value {
                Expr::Lit(ref inner) => match &inner.lit {
                    Lit::Str(literal_string) => format_ident!("{}", literal_string.value()),
                    _ => return Err(Error::new_spanned(&pair, "expected `builder(each = \"...\")`")),
                },
                _ => return Err(Error::new_spanned(&pair, "expected `builder(each = \"...\")`")),
            };
            Ok(Some(value))
        },
        _ => return Err(Error::new_spanned(&builder.meta, "expected `builder(each = \"...\")`")),
    }
}
