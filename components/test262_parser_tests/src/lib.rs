#![recursion_limit = "256"]

extern crate glob;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;

use self::proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, TokenStreamExt};

const DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"));

#[proc_macro]
pub fn test_explicit_js(_input: TokenStream) -> TokenStream {
    let paths = glob::glob(&format!("{}/tests/pass-explicit/*.js", DIR)[..]).unwrap();
    let mut res = quote!();
    for entry in paths {
        let entry = entry.unwrap();
        let path = entry.as_path().to_str().unwrap();
        let mut file_name = entry
            .as_path()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .replace(".", "_");
        file_name.insert_str(0, "explicit");
        let file_name = Ident::new(file_name.as_str(), Span::call_site());
        res.append_all(quote! {
            #[test]
            fn #file_name() {
                timeout_ms(|| {
                    let js = read_file(#path).unwrap();
                    let parsed_explicit = parse(&js[..]).unwrap();

                    let path_pass = #path.replace("pass-explicit", "pass");
                    let js_pass = read_file(path_pass.as_str()).unwrap();
                    let parsed_pass = parse(&js[..]).unwrap();

                    assert_eq!(parsed_explicit, parsed_pass);
                }, 1000)
            }

        });
    }
    res.into()
}

#[proc_macro]
pub fn test_fail_js(_input: TokenStream) -> TokenStream {
    test("fail", true)
}

#[proc_macro]
pub fn test_early_js(_input: TokenStream) -> TokenStream {
    test("early", true)
}

#[proc_macro]
pub fn test_pass_js(_input: TokenStream) -> TokenStream {
    test("pass", false)
}

fn test(name: &str, should_panic: bool) -> TokenStream {
    let paths = glob::glob(&format!("{}/tests/{}/*.js", DIR, name)[..]).unwrap();
    let mut res = quote!();
    for entry in paths {
        let entry = entry.unwrap();
        let path = entry.as_path().to_str().unwrap();
        let mut file_name = entry
            .as_path()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .replace(".", "_");
        file_name.insert_str(0, name);
        let file_name = Ident::new(file_name.as_str(), Span::call_site());
        let q = quote! {
            #[test]
            fn #file_name() {
                timeout_ms(|| {
                    let js = read_file(#path).unwrap();
                    let parsed = parse(&js[..]).unwrap();
                }, 1000)
            }

        };
        if should_panic {
            res.append_all(quote!(#[should_panic]));
        }
        res.append_all(q);
    }
    res.into()
}
