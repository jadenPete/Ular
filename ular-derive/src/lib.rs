use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, FieldsNamed};

fn named_fields_has_field_with_name(fields: &FieldsNamed, expected_name: &str) -> bool {
    fields
        .named
        .iter()
        .any(|field| field.ident.as_ref().unwrap().to_string() == expected_name)
}

fn derive_single_getter_trait(
    input: TokenStream,
    trait_name: TokenStream2,
    field_name: TokenStream2,
    field_type: TokenStream2,
    getter_name: TokenStream2,
) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let body = match &input.data {
        Data::Struct(r#struct) => {
            let field_exists = match &r#struct.fields {
                Fields::Named(fields) => {
                    named_fields_has_field_with_name(fields, &field_name.to_string())
                }
                _ => false,
            };

            if !field_exists {
                return syn::Error::new_spanned(
                    &input,
                    format!(
                        "Expected `{}` to have a field named \"{}\".",
                        name, field_name
                    ),
                )
                .to_compile_error()
                .into();
            }

            quote! {
                self.#field_name.clone()
            }
        }
        Data::Enum(r#enum) => {
            let mut match_arms = Vec::new();

            for variant in &r#enum.variants {
                let variant_name = &variant.ident;
                let match_arm = match &variant.fields {
                    Fields::Named(fields)
                        if named_fields_has_field_with_name(fields, &field_name.to_string()) =>
                    {
                        quote! {
                            Self::#variant_name { #field_name, .. } => #field_name.clone(),
                        }
                    }

                    Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                        quote! {
                            Self::#variant_name(value) => value.#getter_name(),
                        }
                    }
                    _ => {
                        return syn::Error::new_spanned(variant, format!("When deriving `{}` for an enum, each variant must have a single unnamed field or contain a `{}` field.", trait_name, field_name))
                        .to_compile_error()
                        .into();
                    }
                };

                match_arms.push(match_arm);
            }

            quote! {
                match self {
                    #(#match_arms)*
                }
            }
        }
        Data::Union(_) => {
            return syn::Error::new_spanned(
                input,
                format!("`{}` can't be derived for unions.", trait_name),
            )
            .to_compile_error()
            .into();
        }
    };

    quote! {
        impl #trait_name for #name {
            fn #getter_name(&self) -> #field_type {
                #body
            }
        }
    }
    .into()
}

#[proc_macro_derive(Node)]
pub fn derive_node(input: TokenStream) -> TokenStream {
    derive_single_getter_trait(
        input,
        quote! { Node },
        quote! { position },
        quote! { Position },
        quote! { get_position },
    )
}

#[proc_macro_derive(Typed)]
pub fn derive_typed(input: TokenStream) -> TokenStream {
    derive_single_getter_trait(
        input,
        quote! { Typed },
        quote! { type_ },
        quote! { Type },
        quote! { get_type },
    )
}
