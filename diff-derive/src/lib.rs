extern crate proc_macro;

use crate::proc_macro::TokenStream;

use quote::quote;

use syn::{parse_macro_input, DeriveInput, Data};

#[proc_macro_derive(Diff)]
pub fn diff_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match false {
        true => {

        }
        false => {

        }
    }

    match input.data {
        Data::Struct(data_struct) => {
            let name = &input.ident;
            let diff_name = name.to_string() + "Diff";

            let expanded = quote! {
                impl crate::Diff for #name {
                    type Repr = #diff_name;

                    fn diff(&self, other: &Self) -> Self::Repr {
                        
                    }

                    fn apply_new(&self, diff: &Self::Repr) -> Self {

                    }
                    fn identity() -> Self;
                }

                pub struct #diff_name {

                }
            };

            TokenStream::from(expanded)
        },
        _ => todo!(),
    }
}
