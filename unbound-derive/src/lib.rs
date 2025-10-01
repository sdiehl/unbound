//! Derive macros for unbound library

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

/// Derive macro for the Alpha trait
#[proc_macro_derive(Alpha)]
pub fn derive_alpha(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let aeq_impl = generate_aeq_impl(&input.data, name);
    let aeq_in_impl = generate_aeq_in_impl(&input.data);
    let fv_in_impl = generate_fv_in_impl(&input.data);

    let expanded = quote! {
        impl #impl_generics unbound::Alpha for #name #ty_generics #where_clause {
            fn aeq(&self, other: &Self) -> bool {
                #aeq_impl
            }

            fn aeq_in(&self, ctx: &mut unbound::alpha::AlphaCtx, other: &Self) -> bool {
                #aeq_in_impl
            }

            fn fv_in(&self, vars: &mut Vec<String>) {
                #fv_in_impl
            }
        }
    };

    TokenStream::from(expanded)
}

fn generate_aeq_impl(data: &Data, name: &Ident) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields) => {
                let field_checks = fields.named.iter().map(|f| {
                    let field_name = &f.ident;
                    quote! {
                        self.#field_name.aeq(&other.#field_name)
                    }
                });
                quote! {
                    #(#field_checks)&&*
                }
            }
            Fields::Unnamed(fields) => {
                let field_checks = fields.unnamed.iter().enumerate().map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! {
                        self.#index.aeq(&other.#index)
                    }
                });
                quote! {
                    #(#field_checks)&&*
                }
            }
            Fields::Unit => quote! { true },
        },
        Data::Enum(data_enum) => {
            let variant_matches = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                match &variant.fields {
                    Fields::Named(fields) => {
                        let field_names: Vec<_> = fields
                            .named
                            .iter()
                            .filter_map(|f| f.ident.as_ref())
                            .collect();
                        let other_field_names: Vec<_> = field_names
                            .iter()
                            .map(|f| quote::format_ident!("other_{}", f))
                            .collect();
                        let field_checks = field_names.iter().zip(other_field_names.iter()).map(
                            |(field_name, other_field_name)| {
                                quote! {
                                    #field_name.aeq(#other_field_name)
                                }
                            },
                        );
                        let other_bindings = field_names.iter().zip(other_field_names.iter()).map(
                            |(field_name, other_field_name)| {
                                quote! { #field_name: #other_field_name }
                            },
                        );
                        quote! {
                            (#name::#variant_name { #(#field_names),* },
                             #name::#variant_name { #(#other_bindings),* }) => {
                                #(#field_checks)&&*
                            }
                        }
                    }
                    Fields::Unnamed(fields) => {
                        let field_names: Vec<_> = (0..fields.unnamed.len())
                            .map(|i| quote::format_ident!("f{}", i))
                            .collect();
                        let other_names: Vec<_> = (0..fields.unnamed.len())
                            .map(|i| quote::format_ident!("other_f{}", i))
                            .collect();
                        let field_checks =
                            field_names
                                .iter()
                                .zip(other_names.iter())
                                .map(|(f, other_f)| {
                                    quote! {
                                        #f.aeq(#other_f)
                                    }
                                });
                        quote! {
                            (#name::#variant_name(#(#field_names),*),
                             #name::#variant_name(#(#other_names),*)) => {
                                #(#field_checks)&&*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            (#name::#variant_name, #name::#variant_name) => true
                        }
                    }
                }
            });
            quote! {
                match (self, other) {
                    #(#variant_matches,)*
                    _ => false,
                }
            }
        }
        Data::Union(_) => panic!("Unions are not supported"),
    }
}

fn generate_aeq_in_impl(data: &Data) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields) => {
                let field_checks = fields.named.iter().map(|f| {
                    let field_name = &f.ident;
                    quote! {
                        self.#field_name.aeq_in(ctx, &other.#field_name)
                    }
                });
                quote! {
                    #(#field_checks)&&*
                }
            }
            Fields::Unnamed(fields) => {
                let field_checks = fields.unnamed.iter().enumerate().map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! {
                        self.#index.aeq_in(ctx, &other.#index)
                    }
                });
                quote! {
                    #(#field_checks)&&*
                }
            }
            Fields::Unit => quote! { true },
        },
        Data::Enum(data_enum) => {
            // Generate proper pattern matching for enums using aeq_in
            let variant_matches = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                match &variant.fields {
                    Fields::Named(fields) => {
                        let field_names: Vec<_> = fields
                            .named
                            .iter()
                            .filter_map(|f| f.ident.as_ref())
                            .collect();
                        let other_field_names: Vec<_> = field_names
                            .iter()
                            .map(|f| quote::format_ident!("other_{}", f))
                            .collect();
                        let field_checks = field_names.iter().zip(other_field_names.iter()).map(
                            |(field_name, other_field_name)| {
                                quote! {
                                    #field_name.aeq_in(ctx, #other_field_name)
                                }
                            },
                        );
                        let other_bindings = field_names.iter().zip(other_field_names.iter()).map(
                            |(field_name, other_field_name)| {
                                quote! { #field_name: #other_field_name }
                            },
                        );
                        quote! {
                            (Self::#variant_name { #(#field_names),* },
                             Self::#variant_name { #(#other_bindings),* }) => {
                                #(#field_checks)&&*
                            }
                        }
                    }
                    Fields::Unnamed(fields) => {
                        let field_names: Vec<_> = (0..fields.unnamed.len())
                            .map(|i| quote::format_ident!("f{}", i))
                            .collect();
                        let other_names: Vec<_> = (0..fields.unnamed.len())
                            .map(|i| quote::format_ident!("other_f{}", i))
                            .collect();
                        let field_checks =
                            field_names
                                .iter()
                                .zip(other_names.iter())
                                .map(|(f, other_f)| {
                                    quote! {
                                        #f.aeq_in(ctx, #other_f)
                                    }
                                });
                        quote! {
                            (Self::#variant_name(#(#field_names),*),
                             Self::#variant_name(#(#other_names),*)) => {
                                #(#field_checks)&&*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            (Self::#variant_name, Self::#variant_name) => true
                        }
                    }
                }
            });
            quote! {
                match (self, other) {
                    #(#variant_matches,)*
                    _ => false,
                }
            }
        }
        Data::Union(_) => panic!("Unions are not supported"),
    }
}

fn generate_fv_in_impl(data: &Data) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields) => {
                let field_calls = fields.named.iter().map(|f| {
                    let field_name = &f.ident;
                    quote! {
                        self.#field_name.fv_in(vars);
                    }
                });
                quote! {
                    #(#field_calls)*
                }
            }
            Fields::Unnamed(fields) => {
                let field_calls = fields.unnamed.iter().enumerate().map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! {
                        self.#index.fv_in(vars);
                    }
                });
                quote! {
                    #(#field_calls)*
                }
            }
            Fields::Unit => quote! {},
        },
        Data::Enum(data_enum) => {
            let variant_matches = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                match &variant.fields {
                    Fields::Named(fields) => {
                        let field_names: Vec<_> = fields
                            .named
                            .iter()
                            .filter_map(|f| f.ident.as_ref())
                            .collect();
                        let field_calls = field_names.iter().map(|field_name| {
                            quote! {
                                #field_name.fv_in(vars);
                            }
                        });
                        quote! {
                            Self::#variant_name { #(#field_names),* } => {
                                #(#field_calls)*
                            }
                        }
                    }
                    Fields::Unnamed(fields) => {
                        let field_names: Vec<_> = (0..fields.unnamed.len())
                            .map(|i| quote::format_ident!("f{}", i))
                            .collect();
                        let field_calls = field_names.iter().map(|f| {
                            quote! {
                                #f.fv_in(vars);
                            }
                        });
                        quote! {
                            Self::#variant_name(#(#field_names),*) => {
                                #(#field_calls)*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
                            Self::#variant_name => {}
                        }
                    }
                }
            });
            quote! {
                match self {
                    #(#variant_matches)*
                }
            }
        }
        Data::Union(_) => panic!("Unions are not supported"),
    }
}

/// Derive macro for the Subst trait
#[proc_macro_derive(Subst, attributes(subst_var))]
pub fn derive_subst(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let (is_var_impl, subst_impl) = generate_subst_impl(&input.data, name);

    let expanded = quote! {
        impl #impl_generics unbound::Subst<#name #ty_generics> for #name #ty_generics #where_clause {
            fn is_var(&self) -> Option<unbound::SubstName<#name #ty_generics>> {
                #is_var_impl
            }

            fn subst(&self, var: &unbound::Name<#name #ty_generics>, value: &#name #ty_generics) -> Self {
                #subst_impl
            }
        }
    };

    TokenStream::from(expanded)
}

fn generate_subst_impl(
    data: &Data,
    name: &Ident,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    match data {
        Data::Enum(data_enum) => {
            // Check for a V or Var variant
            let var_variant = data_enum
                .variants
                .iter()
                .find(|v| v.ident == "V" || v.ident == "Var" || v.ident == "Variable");

            let is_var_impl = if let Some(var_variant) = var_variant {
                let variant_name = &var_variant.ident;
                quote! {
                    match self {
                        #name::#variant_name(x) => Some(unbound::SubstName::Name(x.clone())),
                        _ => None,
                    }
                }
            } else {
                quote! { None }
            };

            let subst_cases = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;

                // Special handling for variable variant
                if Some(&variant.ident) == var_variant.as_ref().map(|v| &v.ident) {
                    quote! {
                        #name::#variant_name(x) => {
                            if x == var {
                                value.clone()
                            } else {
                                self.clone()
                            }
                        }
                    }
                } else if variant.ident == "Lam" {
                    // Special handling for lambda variant with Bind
                    match &variant.fields {
                        Fields::Unnamed(_) => {
                            quote! {
                                #name::#variant_name(bnd) => {
                                    // Check if the bound variable is the same as the substitution variable
                                    let bound_var = bnd.pattern();
                                    if bound_var == var {
                                        // No substitution under the binder
                                        self.clone()
                                    } else {
                                        // Perform capture-avoiding substitution
                                        let body_subst = bnd.body().subst(var, value);
                                        #name::#variant_name(unbound::bind(bound_var.clone(), body_subst))
                                    }
                                }
                            }
                        }
                        _ => {
                            // Fallback for other field types
                            match &variant.fields {
                                Fields::Named(fields) => {
                                    let field_names: Vec<_> =
                                        fields.named.iter().filter_map(|f| f.ident.as_ref()).collect();
                                    let field_substs = field_names.iter().map(|field_name| {
                                        quote! {
                                            #field_name: #field_name.subst(var, value)
                                        }
                                    });
                                    quote! {
                                        #name::#variant_name { #(#field_names),* } => {
                                            #name::#variant_name {
                                                #(#field_substs),*
                                            }
                                        }
                                    }
                                }
                                Fields::Unnamed(fields) => {
                                    let field_names: Vec<_> = (0..fields.unnamed.len())
                                        .map(|i| quote::format_ident!("f{}", i))
                                        .collect();
                                    let field_substs = field_names.iter().map(|f| {
                                        quote! {
                                            #f.subst(var, value)
                                        }
                                    });
                                    quote! {
                                        #name::#variant_name(#(#field_names),*) => {
                                            #name::#variant_name(#(#field_substs),*)
                                        }
                                    }
                                }
                                Fields::Unit => {
                                    quote! {
                                        #name::#variant_name => #name::#variant_name
                                    }
                                }
                            }
                        }
                    }
                } else {
                    match &variant.fields {
                        Fields::Named(fields) => {
                            let field_names: Vec<_> =
                                fields.named.iter().filter_map(|f| f.ident.as_ref()).collect();
                            let field_substs = field_names.iter().map(|field_name| {
                                quote! {
                                    #field_name: #field_name.subst(var, value)
                                }
                            });
                            quote! {
                                #name::#variant_name { #(#field_names),* } => {
                                    #name::#variant_name {
                                        #(#field_substs),*
                                    }
                                }
                            }
                        }
                        Fields::Unnamed(fields) => {
                            let field_names: Vec<_> = (0..fields.unnamed.len())
                                .map(|i| quote::format_ident!("f{}", i))
                                .collect();
                            let field_substs = field_names.iter().map(|f| {
                                quote! {
                                    #f.subst(var, value)
                                }
                            });
                            quote! {
                                #name::#variant_name(#(#field_names),*) => {
                                    #name::#variant_name(#(#field_substs),*)
                                }
                            }
                        }
                        Fields::Unit => {
                            quote! {
                                #name::#variant_name => #name::#variant_name
                            }
                        }
                    }
                }
            });

            let subst_impl = quote! {
                match self {
                    #(#subst_cases),*
                }
            };

            (is_var_impl, subst_impl)
        }
        Data::Struct(data_struct) => {
            let is_var_impl = quote! { None };

            let subst_impl = match &data_struct.fields {
                Fields::Named(fields) => {
                    let field_names: Vec<_> = fields
                        .named
                        .iter()
                        .filter_map(|f| f.ident.as_ref())
                        .collect();
                    let field_substs = field_names.iter().map(|field_name| {
                        quote! {
                            #field_name: self.#field_name.subst(var, value)
                        }
                    });
                    quote! {
                        #name {
                            #(#field_substs),*
                        }
                    }
                }
                Fields::Unnamed(fields) => {
                    let field_substs = (0..fields.unnamed.len()).map(|i| {
                        let index = syn::Index::from(i);
                        quote! {
                            self.#index.subst(var, value)
                        }
                    });
                    quote! {
                        #name(#(#field_substs),*)
                    }
                }
                Fields::Unit => quote! { #name },
            };

            (is_var_impl, subst_impl)
        }
        Data::Union(_) => panic!("Unions are not supported"),
    }
}
