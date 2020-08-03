extern crate proc_macro;
use crate::proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Diff)]
pub fn diff_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match input.data {
        Data::Struct(data_struct) => {
            let name = &input.ident;
            let diff_name = format_ident!("{}Diff", name);

            let (fields, is_named) = match &data_struct.fields {
                Fields::Named(fields) => (&fields.named, true),
                Fields::Unnamed(fields) => (&fields.unnamed, false),
                Fields::Unit => panic!("Cannot derive Diff on unit struct"),
            };

            if is_named {
                let names = fields.iter().map(|field| &field.ident).collect::<Vec<_>>();
                let types = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();
                quote! {
                    #[derive(Debug, PartialEq)]
                    pub struct #diff_name {
                        #(pub #names: <#types as Diff>::Repr),*
                    }

                    impl Diff for #name {
                        type Repr = #diff_name;

                        fn diff(&self, other: &Self) -> Self::Repr {
                            Self::Repr {
                                #(#names: self.#names.diff(&other.#names)),*
                            }
                        }

                        fn apply(&mut self, diff: &Self::Repr) {
                            #(self.#names.apply(&diff.#names);)*
                        }

                        fn identity() -> Self {
                            Self {
                                #(#names: <#types as Diff>::identity()),*
                            }
                        }
                    }
                }
            } else {
                let (numbers, types): (Vec<_>, Vec<_>) =
                    fields.iter().map(|field| &field.ty).enumerate().unzip();
                quote! {
                    #[derive(Debug, PartialEq)]
                    pub struct #diff_name (
                        #(pub <#types as Diff>::Repr),*
                    )

                    impl Diff for #name {
                        type Repr = #diff_name;

                        fn diff(&self, other: &Self) -> Self::Repr {
                            Self::Repr (
                                #(self.#numbers.diff(&other.#numbers))*
                            )
                        }

                        fn apply(&mut self, diff: &Self::Repr) {
                            #(self.#numbers.apply(&diff.#numbers);)*
                        }

                        fn identity() -> Self {
                            Self (
                                #(<#types as Diff>::identity()),*
                            )
                        }
                    }
                }
            }
            .into()
        }
        _ => todo!(),
    }
}
