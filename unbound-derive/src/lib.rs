//! Derive macros for the unbound library.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

/// Derive the `Alpha` trait.
///
/// Every field is traversed structurally. Binding is handled by the `Bind`
/// fields themselves, so nothing here needs to know which variant is a
/// binder.
#[proc_macro_derive(Alpha)]
pub fn derive_alpha(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    let name = input.ident.clone();
    bound_type_params(&mut input.generics, syn::parse_quote!(unbound::Alpha));
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let aeq = aeq_body(&input.data);
    let close = traversal_body(&input.data, &format_ident!("close"), quote!(level, names));
    let open = traversal_body(&input.data, &format_ident!("open"), quote!(level, names));
    let fv_in = traversal_body(&input.data, &format_ident!("fv_in"), quote!(acc));

    quote! {
        impl #impl_generics unbound::Alpha for #name #ty_generics #where_clause {
            fn aeq(&self, other: &Self) -> bool {
                #aeq
            }

            fn close(&mut self, level: usize, names: &[unbound::AnyName]) {
                #close
            }

            fn open(&mut self, level: usize, names: &[unbound::AnyName]) {
                #open
            }

            fn fv_in(&self, acc: &mut Vec<unbound::AnyName>) {
                #fv_in
            }
        }
    }
    .into()
}

/// Require `bound` of every type parameter, as a derive normally would.
fn bound_type_params(generics: &mut syn::Generics, bound: syn::TypeParamBound) {
    for param in generics.type_params_mut() {
        param.bounds.push(bound.clone());
    }
}

/// The names bound by a variant's fields, and the pattern that binds them.
fn destructure(fields: &Fields, prefix: &str) -> (Vec<Ident>, TokenStream2) {
    match fields {
        Fields::Named(f) => {
            let names: Vec<Ident> = f
                .named
                .iter()
                .map(|f| f.ident.clone().expect("named field"))
                .collect();
            let bindings = names.iter().map(|n| {
                let local = format_ident!("{}_{}", prefix, n);
                quote!(#n: #local)
            });
            let locals: Vec<Ident> = names
                .iter()
                .map(|n| format_ident!("{}_{}", prefix, n))
                .collect();
            (locals, quote!({ #(#bindings),* }))
        }
        Fields::Unnamed(f) => {
            let locals: Vec<Ident> = (0..f.unnamed.len())
                .map(|i| format_ident!("{}_{}", prefix, i))
                .collect();
            (locals.clone(), quote!(( #(#locals),* )))
        }
        Fields::Unit => (Vec::new(), quote!()),
    }
}

/// Field accessors for a struct, either `self.name` or `self.0`.
fn struct_fields(fields: &Fields, receiver: TokenStream2) -> Vec<TokenStream2> {
    match fields {
        Fields::Named(f) => f
            .named
            .iter()
            .map(|f| {
                let n = f.ident.as_ref().expect("named field");
                quote!(#receiver.#n)
            })
            .collect(),
        Fields::Unnamed(f) => (0..f.unnamed.len())
            .map(|i| {
                let i = syn::Index::from(i);
                quote!(#receiver.#i)
            })
            .collect(),
        Fields::Unit => Vec::new(),
    }
}

fn aeq_body(data: &Data) -> TokenStream2 {
    match data {
        Data::Struct(s) => {
            let mine = struct_fields(&s.fields, quote!(self));
            let theirs = struct_fields(&s.fields, quote!(other));
            if mine.is_empty() {
                return quote!(true);
            }
            let checks = mine.iter().zip(&theirs).map(|(a, b)| quote!(#a.aeq(&#b)));
            quote!(#(#checks)&&*)
        }
        Data::Enum(e) => {
            let arms = e.variants.iter().map(|v| {
                let variant = &v.ident;
                let (mine, lhs) = destructure(&v.fields, "l");
                let (theirs, rhs) = destructure(&v.fields, "r");
                if mine.is_empty() {
                    return quote!((Self::#variant #lhs, Self::#variant #rhs) => true);
                }
                let checks = mine.iter().zip(&theirs).map(|(a, b)| quote!(#a.aeq(#b)));
                quote! {
                    (Self::#variant #lhs, Self::#variant #rhs) => #(#checks)&&*
                }
            });
            quote! {
                match (self, other) {
                    #(#arms,)*
                    _ => false,
                }
            }
        }
        Data::Union(_) => panic!("Alpha cannot be derived for unions"),
    }
}

/// A traversal that calls `method(args)` on every field in turn.
fn traversal_body(data: &Data, method: &Ident, args: TokenStream2) -> TokenStream2 {
    match data {
        Data::Struct(s) => {
            let calls = struct_fields(&s.fields, quote!(self))
                .into_iter()
                .map(|f| quote!(#f.#method(#args);));
            quote!(#(#calls)*)
        }
        Data::Enum(e) => {
            let arms = e.variants.iter().map(|v| {
                let variant = &v.ident;
                let (locals, pat) = destructure(&v.fields, "f");
                let calls = locals.iter().map(|l| quote!(#l.#method(#args);));
                quote! {
                    Self::#variant #pat => { #(#calls)* }
                }
            });
            quote! {
                match self {
                    #(#arms)*
                }
            }
        }
        Data::Union(_) => panic!("Alpha cannot be derived for unions"),
    }
}

/// Derive the `Subst` trait.
///
/// By default the type substitutes into itself, and a variant named `V`,
/// `Var` or `Variable` (or marked `#[subst_var]`) holding a single `Name` is
/// taken to be the variable case. `#[subst(Ty, ...)]` instead derives
/// `Subst<Ty>` for each listed type, so a term can take substitutions for
/// the types it mentions; list `Self` to keep the self-substitution too.
/// `#[subst(_)]` derives `Subst<V>` for every `V`, which suits a type with
/// no variables of its own, such as the kinds annotating a type binder.
/// Binders need no special treatment: a closed body has no name for an
/// incoming term to capture.
#[proc_macro_derive(Subst, attributes(subst_var, subst))]
pub fn derive_subst(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let self_ty: syn::Type = syn::parse_quote!(#name #ty_generics);

    let mut targets = Vec::new();
    for attr in input.attrs.iter().filter(|a| a.path().is_ident("subst")) {
        match attr.parse_args_with(
            syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::parse_terminated,
        ) {
            Ok(tys) => targets.extend(tys),
            Err(e) => return e.to_compile_error().into(),
        }
    }
    if targets.is_empty() {
        targets.push(syn::parse_quote!(Self));
    }

    let mut any_generics = input.generics.clone();
    any_generics.params.push(syn::parse_quote!(__V));
    let (any_impl_generics, _, _) = any_generics.split_for_impl();

    let impls = targets.into_iter().map(|target| {
        if let syn::Type::Infer(_) = target {
            let (is_var, subst) = subst_bodies(&input.data, false);
            return quote! {
                impl #any_impl_generics unbound::Subst<__V> for #name #ty_generics #where_clause {
                    fn is_var(&self) -> Option<unbound::SubstName<__V>> {
                        #is_var
                    }

                    fn subst(&self, var: &unbound::Name<__V>, value: &__V) -> Self {
                        #subst
                    }
                }
            };
        }
        let is_self = matches!(&target, syn::Type::Path(p)
            if p.qself.is_none() && (p.path.is_ident("Self") || p.path.is_ident(name)));
        let target = if is_self { self_ty.clone() } else { target };
        let (is_var, subst) = subst_bodies(&input.data, is_self);
        quote! {
            impl #impl_generics unbound::Subst<#target> for #name #ty_generics #where_clause {
                fn is_var(&self) -> Option<unbound::SubstName<#target>> {
                    #is_var
                }

                fn subst(&self, var: &unbound::Name<#target>, value: &#target) -> Self {
                    #subst
                }
            }
        }
    });

    quote!(#(#impls)*).into()
}

/// The `is_var` and `subst` bodies, recognising a variable case only when
/// substituting into the type itself.
fn subst_bodies(data: &Data, is_self: bool) -> (TokenStream2, TokenStream2) {
    match data {
        Data::Struct(s) => {
            let fields = struct_fields(&s.fields, quote!(self));
            let build = match &s.fields {
                Fields::Named(f) => {
                    let names: Vec<_> = f.named.iter().filter_map(|f| f.ident.as_ref()).collect();
                    let inits = names
                        .iter()
                        .zip(&fields)
                        .map(|(n, f)| quote!(#n: #f.subst(var, value)));
                    quote!(Self { #(#inits),* })
                }
                Fields::Unnamed(_) => {
                    let inits = fields.iter().map(|f| quote!(#f.subst(var, value)));
                    quote!(Self( #(#inits),* ))
                }
                Fields::Unit => quote!(Self),
            };
            (quote!(None), build)
        }
        Data::Enum(e) => {
            let var_variant = is_self
                .then(|| {
                    e.variants
                        .iter()
                        .find(|v| v.attrs.iter().any(|a| a.path().is_ident("subst_var")))
                        .or_else(|| {
                            e.variants.iter().find(|v| {
                                v.ident == "V" || v.ident == "Var" || v.ident == "Variable"
                            })
                        })
                })
                .flatten()
                .map(|v| &v.ident);

            let is_var = match var_variant {
                Some(v) => quote! {
                    match self {
                        Self::#v(x) => Some(unbound::SubstName::Name(x.clone())),
                        _ => None,
                    }
                },
                None => quote!(None),
            };

            let arms = e.variants.iter().map(|v| {
                let variant = &v.ident;
                if Some(variant) == var_variant {
                    return quote! {
                        Self::#variant(x) => {
                            if x == var { value.clone() } else { self.clone() }
                        }
                    };
                }
                let (locals, pat) = destructure(&v.fields, "f");
                let build = match &v.fields {
                    Fields::Named(f) => {
                        let names: Vec<_> =
                            f.named.iter().filter_map(|f| f.ident.as_ref()).collect();
                        let inits = names
                            .iter()
                            .zip(&locals)
                            .map(|(n, l)| quote!(#n: #l.subst(var, value)));
                        quote!(Self::#variant { #(#inits),* })
                    }
                    Fields::Unnamed(_) => {
                        let inits = locals.iter().map(|l| quote!(#l.subst(var, value)));
                        quote!(Self::#variant( #(#inits),* ))
                    }
                    Fields::Unit => quote!(Self::#variant),
                };
                quote!(Self::#variant #pat => #build)
            });

            (is_var, quote!(match self { #(#arms),* }))
        }
        Data::Union(_) => panic!("Subst cannot be derived for unions"),
    }
}
