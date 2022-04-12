#![feature(trait_alias)]
#![allow(dead_code)]
pub mod ctx;
pub mod err;
mod fmt;
mod macros;
pub mod tag;

use crate::ctx::Ctx;
use crate::err::CodeRef;
use crate::tag::Tag;
use std::fmt::Debug;

pub type _Bind<R> = Tag<<R as Repr>::Ann, <R as Repr>::Bind>;
pub type _Var<R> = Tag<<R as Repr>::Ann, <R as Repr>::Var>;

pub type _Ident<R> = Tag<<R as Repr>::Ann, String>;
pub type _Int<R> = Tag<<R as Repr>::Ann, i64>;
pub type _Vec<R, T> = Tag<<R as Repr>::Ann, Vec<T>>;

pub type _Path<R> = _Vec<R, _Ident<R>>;
pub type Path = std::vec::Vec<String>;

pub type _Type<R> = Tag<<R as Repr>::Ann, Type<R>>;

#[derive(Clone, Debug)]
pub enum Type<R: Repr + Clone + Debug> {
    Var(_Var<R>),

    Unit,
    Bool,
    Int,
    Str,

    Tup(_Vec<R, _Type<R>>),
    // Sum(_Vec<R, _Type<R>>),
    Rec(_Vec<R, (_Ident<R>, _Type<R>)>),

    Fun(_Vec<R, _Type<R>>, _Type<R>),

    Struct(Tag<R::Ann, Path>, _Variant<R>),
    Enum(Tag<R::Ann, Path>, _Vec<R, (_Ident<R>, _Variant<R>)>),
}

pub type _UnOp<R> = Tag<<R as Repr>::Ann, UnOp>;
#[derive(Clone, Debug)]
pub enum UnOp {
    Not,
    Neg,
}

pub type _BinOp<R> = Tag<<R as Repr>::Ann, BinOp>;
#[derive(Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    And,
    Or,

    Gt,
    Gte,
    Lt,
    Lte,

    Eq,
    Neq,
}

pub type _Constructor<R> = Tag<<R as Repr>::Ann, Constructor<R>>;
#[derive(Clone, Debug)]
pub enum Constructor<R: Repr + Clone + Debug> {
    Unit,
    Tup(_Vec<R, _Term<R>>),
    Rec(_Vec<R, (_Ident<R>, _Term<R>)>),
}

pub type _Pattern<R> = Tag<<R as Repr>::Ann, Pattern<R>>;
#[derive(Clone, Debug)]
pub enum Pattern<R: Repr + Clone + Debug> {
    Wildcard,
    Var(_Bind<R>),

    Const(_Term<R>),
    Struct(_Var<R>, Path, _Pattern<R>),
    Variant(_Var<R>, Path, _Ident<R>, _Pattern<R>),

    Or(_Vec<R, _Pattern<R>>),

    Unit,
    Tup(_Vec<R, _Pattern<R>>),
    Rec(_Vec<R, (_Bind<R>, Option<_Pattern<R>>)>),
}

pub type _Term<R> = Tag<<R as Repr>::Ann, Term<R>>;

#[derive(Clone, Debug)]
pub enum Term<R: Repr + Clone + Debug> {
    Var(_Var<R>),

    Unit,
    True,
    False,
    Int(i64),
    Str(String),

    Seq(_Term<R>, _Term<R>),

    Tup(_Vec<R, _Term<R>>),
    Rec(_Vec<R, (_Ident<R>, _Term<R>)>),

    UnOp(_UnOp<R>, _Term<R>),
    BinOp(_Term<R>, _BinOp<R>, _Term<R>),

    Struct(_Var<R>, Path, _Constructor<R>),
    Enum(_Var<R>, Path, _Ident<R>, _Constructor<R>),

    App(_Term<R>, _Vec<R, _Term<R>>),

    TupProj(_Term<R>, _Int<R>),
    RecProj(_Term<R>, _Ident<R>),

    Let(_Pattern<R>, _Term<R>, _Term<R>),
    Assign(_Var<R>, _Term<R>, _Term<R>),

    Lam(_Vec<R, (_Pattern<R>, _Type<R>)>, _Term<R>),
    Match(_Term<R>, _Vec<R, (_Pattern<R>, _Term<R>)>),

    Fun(
        _Bind<R>,
        _Vec<R, (_Pattern<R>, _Type<R>)>,
        _Type<R>,
        _Term<R>,
        _Term<R>,
    ),
}

pub type _Body<R> = Tag<<R as Repr>::Ann, Body<R>>;
#[derive(Clone, Debug)]
pub enum Body<R: Repr + Clone + Debug> {
    Unit,
    Tup(_Vec<R, _Value<R>>),
    Rec(_Vec<R, (_Ident<R>, _Value<R>)>),
}

pub type _Value<R> = Tag<<R as Repr>::Ann, Value<R>>;
#[derive(Clone, Debug)]
pub enum Value<R: Repr + Clone + Debug> {
    Unit,
    Bool(bool),
    Int(i64),
    Str(String),

    Tup(_Vec<R, _Value<R>>),
    Rec(_Vec<R, (_Ident<R>, _Value<R>)>),

    Struct(Path, _Body<R>),
    Enum(Path, _Ident<R>, _Body<R>),

    Clos(_Vec<R, _Pattern<R>>, _Term<R>, Ctx<_Value<R>>),
    TopClos(_Vec<R, _Pattern<R>>, _Term<R>),
    FFIClos(_Ident<R>, _Vec<R, _Type<R>>, _Type<R>),
}

pub type _Variant<R> = Tag<<R as Repr>::Ann, Variant<R>>;
#[derive(Clone, Debug)]
pub enum Variant<R: Repr + Clone + Debug> {
    Unit,
    Tup(_Vec<R, _Type<R>>),
    Rec(_Vec<R, (_Ident<R>, _Type<R>)>),
}

pub type _Top<R> = Tag<<R as Repr>::Ann, Top<R>>;
#[derive(Clone, Debug)]
pub enum Top<R: Repr + Clone + Debug> {
    Use(_Path<R>),
    Fun(
        _Bind<R>,
        _Vec<R, (_Pattern<R>, _Type<R>)>,
        _Type<R>,
        _Term<R>,
    ),
    FFIFun(
        _Bind<R>,
        _Vec<R, (_Ident<R>, _Type<R>)>,
        _Type<R>,
        _Ident<R>,
    ),
    Alias(_Bind<R>, _Type<R>),
    Struct(_Bind<R>, _Variant<R>),
    Enum(_Bind<R>, _Vec<R, (_Ident<R>, _Variant<R>)>),
}

pub type _Program<R> = _Vec<R, _Top<R>>;

pub trait Repr {
    type Var: Clone + Debug;
    type Bind: Clone + Debug;
    type Ann: Clone + Debug;
}
#[derive(Clone, Debug)]
pub struct Named;
impl Repr for Named {
    type Var = std::vec::Vec<_Ident<Named>>;
    type Bind = String;
    type Ann = CodeRef;
}
#[derive(Clone, Debug)]
pub struct Debruijn;
impl Repr for Debruijn {
    type Var = usize;
    type Bind = String;
    type Ann = CodeRef;
}
