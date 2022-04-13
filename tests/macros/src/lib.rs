#![feature(proc_macro_span)]
extern crate proc_macro;
use ast::{Debruijn, Named, _Top, _Vec};
use itertools::Itertools;
use proc_macro::{TokenStream, TokenTree};

fn token_stream_to_string(_item: TokenStream) -> String {
    let iter = _item.into_iter();
    let mut s = iter
        .clone()
        .tuple_windows()
        .map(|(this, next)| match this {
            TokenTree::Group(_) => token_stream_to_string(this.into()),
            _ => {
                if this.span().end().line != next.span().start().line {
                    format!("{}\n", this)
                } else if this.span().end().column == next.span().start().column - 1 {
                    format!("{} ", this)
                } else {
                   this.to_string()
                }
            }
        })
        .collect::<Vec<String>>()
        .join("");
    let last = iter.clone().last().unwrap();
    if iter.count() > 1 {
        match last {
            TokenTree::Group(_) => s.push_str(token_stream_to_string(last.into()).as_str()),
            _ => s.push_str(last.to_string().as_str()),
        }
    } else {
        s.push_str(last.to_string().as_str())
    }
    s
}

fn parse(src: &str) -> _Vec<Named, _Top<Named>> {
    parse::parse_string(src.to_owned(), vec!["test".into()])
        .map_err(|x| {
            println!("{}", x.build(Some(src.to_owned()), true));
            x.build(Some(src.to_owned()), false)
        })
        .unwrap()
}

fn ir(program: _Vec<Named, _Top<Named>>, src: &str) -> _Vec<Debruijn, _Top<Debruijn>> {
    ir::transform(&program)
        .map_err(|x| {
            println!("{}", x.build(Some(src.to_owned()), true));
            x.build(Some(src.to_owned()), false)
        })
        .unwrap()
}

#[proc_macro]
pub fn parse_fails(_item: TokenStream) -> TokenStream {
    let src = token_stream_to_string(_item);
    match parse::parse_string(src, vec!["test".into()]) {
        Ok(v) => format!("assert!(false, {:#?})", v).parse().unwrap(),
        Err(_) => "assert!(true)".parse().unwrap(),
    }
}

#[proc_macro]
pub fn ir_fails(_item: TokenStream) -> TokenStream {
    let src = token_stream_to_string(_item);
    let parsed = parse(&src);
    match ir::transform(&parsed) {
        Ok(v) => format!("assert!(false, {:#?})", v).parse().unwrap(),
        Err(_) => "assert!(true)".parse().unwrap(),
    }
}

#[proc_macro]
pub fn typing_fails(_item: TokenStream) -> TokenStream {
    let src = token_stream_to_string(_item);
    let parsed = parse(&src);
    let transformed = ir(parsed, &src);
    match typing::type_check(&transformed) {
        Ok(_) => "assert!(false, \"typing succeeded\")".parse().unwrap(),
        Err(_) => "assert!(true)".parse().unwrap(),
    }
}

#[proc_macro]
pub fn eval_fails(_item: TokenStream) -> TokenStream {
    let src = token_stream_to_string(_item);
    let parsed = parse(&src);
    let transformed = ir(parsed, &src);
    let ffi = typing::type_check(&transformed)
        .map_err(|x| {
            println!("{}", x.build(Some(src.clone()), true));
            x.build(Some(src.clone()), false)
        })
        .unwrap();
    match eval::eval(&transformed, &ffi) {
        Ok(v) => format!("assert!(false, \"{:#?}\")", v).parse().unwrap(),
        Err(_) => "assert!(true)".parse().unwrap(),
    }
}

#[proc_macro]
pub fn succeeds(_item: TokenStream) -> TokenStream {
    let src = token_stream_to_string(_item);
    let parsed = parse(&src);
    let transformed = ir(parsed, &src);
    let ffi = typing::type_check(&transformed)
        .map_err(|x| {
            println!("{}", x.build(Some(src.clone()), true));
            x.build(Some(src.clone()), false)
        })
        .unwrap();
    eval::eval(&transformed, &ffi)
        .map_err(|x| {
            println!("{}", x.build(Some(src.clone()), true));
            x.build(Some(src.clone()), false)
        })
        .unwrap();
    "assert!(true)".parse().unwrap()
}
