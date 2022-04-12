extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro]
pub fn parse_fails(_item: TokenStream) -> TokenStream {
    match parse::parse_string(_item.to_string().replace("! N", "\n"), vec!["test".into()]) {
        Ok(v) => format!("assert!(false, {:#?})", v).parse().unwrap(),
        Err(_) => "assert!(true)".parse().unwrap(),
    }
}

#[proc_macro]
pub fn typing_fails(_item: TokenStream) -> TokenStream {
    println!("{}", _item.to_string());
    let parsed =
        parse::parse_string(_item.to_string().replace("! N", "\n"), vec!["test".into()]).unwrap();
    let transformed = ir::transform(&parsed).unwrap();
    match typing::type_check(&transformed) {
        Ok(_) => "assert!(false, \"typing succeeded\")".parse().unwrap(),
        Err(_) => "assert!(true)".parse().unwrap(),
    }
}

#[proc_macro]
pub fn eval_fails(_item: TokenStream) -> TokenStream {
    let parsed =
        parse::parse_string(_item.to_string().replace("! N", "\n"), vec!["test".into()]).unwrap();
    let transformed = ir::transform(&parsed).unwrap();
    let ffi = typing::type_check(&transformed).unwrap();
    match eval::eval(&transformed, &ffi) {
        Ok(v) => format!("assert!(false, \"{:#?}\")", v).parse().unwrap(),
        Err(_) => "assert!(true)".parse().unwrap(),
    }
}

#[proc_macro]
pub fn succeeds(_item: TokenStream) -> TokenStream {
    let parsed =
        parse::parse_string(_item.to_string().replace("! N", "\n"), vec!["test".into()]).unwrap();
    let transformed = ir::transform(&parsed).unwrap();
    let ffi = typing::type_check(&transformed).unwrap();
    eval::eval(&transformed, &ffi).unwrap();
    "assert!(true)".parse().unwrap()
}
