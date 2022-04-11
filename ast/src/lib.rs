#![feature(trait_alias)]
#![allow(dead_code)]
pub mod ctx;
pub mod err;
pub mod tag;

use crate::ctx::Ctx;
use crate::err::CodeRef;
use crate::tag::Tag;
use std::fmt::{Debug, Display};

// todo: write a macro

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

impl _Top<Debruijn> {
    pub fn id(&self) -> _Ident<Debruijn> {
        match &self.it() {
            Top::Fun(id, _, _, _) => id.clone(),
            Top::FFIFun(id, _, _, _) => id.clone(),
            Top::Alias(id, _) => id.clone(),
            Top::Struct(id, _) => id.clone(),
            Top::Enum(id, _) => id.clone(),
            Top::Use(_) => unimplemented!(),
        }
    }
}

impl _Path<Debruijn> {
    pub fn last(&self) -> _Ident<Debruijn> {
        self.it().last().unwrap().clone()
    }
}

/* impl From<i64> for Value<Debruijn> {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<String> for Value<Debruijn> {
    fn from(s: String) -> Self {
        Value::Str(s)
    }
}

impl From<()> for Value<Debruijn> {
    fn from(_: ()) -> Self {
        Value::Unit
    }
} */

impl<R: Repr + Clone + Debug> Display for Variant<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variant::Unit => Ok(()),
            Variant::Tup(els) => write!(
                f,
                "({})",
                els.it()
                    .iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Variant::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .it()
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
        }
    }
}

impl<R: Repr + Clone + Debug> Display for Type<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(_) => write!(f, "[bug] unresolved type variable",),
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Str => write!(f, "Str"),
            Type::Tup(els) => write!(
                f,
                "({})",
                els.it()
                    .iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .it()
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Fun(args, ret) => write!(
                f,
                "({}) -> {}",
                args.it()
                    .iter()
                    .map(|ty| format!("{}", ty))
                    .collect::<Vec<String>>()
                    .join(","),
                ret
            ),
            Type::Struct(id, _) => write!(
                f,
                "{}",
                id.it()
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join("::")
            ),
            Type::Enum(id, _) => write!(
                f,
                "{}",
                id.it()
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join("::")
            ),
        }
    }
}

impl<R: Repr + Clone + Debug> Display for Body<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Body::Unit => Ok(()),
            Body::Tup(els) => write!(
                f,
                "({})",
                els.it()
                    .iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Body::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .it()
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
        }
    }
}

impl<R: Repr + Clone + Debug> Display for Value<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Str(s) => write!(f, "{}", s),
            Value::Tup(els) => write!(
                f,
                "({})",
                els.it()
                    .iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Value::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .it()
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Value::Struct(id, _) => write!(
                f,
                "{}",
                id.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join("::")
            ),
            Value::Enum(id, _, _) => write!(
                f,
                "{}",
                id.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join("::")
            ),
            Value::Clos(_, _, _) => write!(f, "callable"),
            Value::TopClos(_, _) => write!(f, "toplevel callable"),
            Value::FFIClos(_, _, _) => write!(f, "ffi callable"),
        }
    }
}

#[macro_export]
macro_rules! path {
    ($($x:expr),+ $(,)?) => (
        {
            vec![$($x.clone().into()),+]
        }
    );
}

#[macro_export]
macro_rules! paths {
    ($x:expr) => {{
        let mut tmp = vec![];
        for v in $x {
            tmp.push(path![v])
        }
        tmp
    }};
}

#[macro_export]
macro_rules! def_from_to_ast_types {
    (from => $i:ident, to => $o:ident, prefix => $p:ident) => {
        type FromIdent = $p::_Ident<$p::$i>;
        type FromInt = $p::_Int<$p::$i>;
        type FromVec<T> = $p::_Vec<$p::$i, T>;
        type FromType = $p::_Type<$p::$i>;
        type FromBinOp = $p::_BinOp<$p::$i>;
        type FromUnOp = $p::_UnOp<$p::$i>;
        type FromConstructor = $p::_Constructor<$p::$i>;
        type FromPattern = $p::_Pattern<$p::$i>;
        type FromTerm = $p::_Term<$p::$i>;
        type FromBody = $p::_Body<$p::$i>;
        type FromValue = $p::_Value<$p::$i>;
        type FromVariant = $p::_Variant<$p::$i>;
        type FromTop = $p::_Top<$p::$i>;
        type FromProgram = $p::_Program<$p::$i>;
        type FromPath = $p::_Path<$p::$i>;

        type ToIdent = $p::_Ident<$p::$o>;
        type ToInt = $p::_Int<$p::$o>;
        type ToVec<T> = $p::_Vec<$p::$o, T>;
        type ToType = $p::_Type<$p::$o>;
        type ToBinOp = $p::_BinOp<$p::$o>;
        type ToUnOp = $p::_UnOp<$p::$o>;
        type ToConstructor = $p::_Constructor<$p::$o>;
        type ToPattern = $p::_Pattern<$p::$o>;
        type ToTerm = $p::_Term<$p::$o>;
        type ToBody = $p::_Body<$p::$o>;
        type ToValue = $p::_Value<$p::$o>;
        type ToVariant = $p::_Variant<$p::$o>;
        type ToTop = $p::_Top<$p::$o>;
        type ToProgram = $p::_Program<$p::$o>;
        type ToPath = $p::_Path<$p::$o>;
    };
}

#[macro_export]
macro_rules! def_ast_types {
    (type => $i:ident, prefix => $p:ident) => {
        type Ident = $p::_Ident<$p::$i>;
        type Int = $p::_Int<$p::$i>;
        type Vec<T> = $p::_Vec<$p::$i, T>;
        type Type = $p::_Type<$p::$i>;
        type BinOp = $p::_BinOp<$p::$i>;
        type UnOp = $p::_UnOp<$p::$i>;
        type Constructor = $p::_Constructor<$p::$i>;
        type Pattern = $p::_Pattern<$p::$i>;
        type Term = $p::_Term<$p::$i>;
        type Body = $p::_Body<$p::$i>;
        type Value = $p::_Value<$p::$i>;
        type Variant = $p::_Variant<$p::$i>;
        type Top = $p::_Top<$p::$i>;
        type Program = $p::_Program<$p::$i>;
        type Path = $p::_Path<$p::$i>;
    };
}
