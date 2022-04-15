use ast::ctx::Ctx;
use ast::err::{Error, Result};

use crate::{Program, Type, Variant};
// enum Body {
//     Unit,
//     Tup(Vec<Value>),
//     Rec(Vec<(String, Value)>)
// }

// enum Value {
//     Unit,
//     Bool(bool),
//     Int(i64),
//     Str(String),
//     Tup(Vec<Value>),
//     Rec(Vec<(String, Value)>),
//     Struct(String, Body),
//     Enum(String, String, Body)
// }

// trait ToValue {
//     fn to_value(&self) -> Value;
// }

const BOILERPLATE: &str = "#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]

// enum Body {
//     Unit,
//     Tup(Vec<Value>),
//     Rec(Vec<(String, Value)>)
// }

// enum Value {
//     Unit,
//     Bool(bool),
//     Int(i64),
//     Str(String),
//     Tup(Vec<Value>),
//     Rec(Vec<(String, Value)>),
//     Struct(String, Body),
//     Enum(String, String, Body)
// }

// trait ToValue {
//     fn to_value(&self) -> Value;
// }

// impl ToValue for () {
//     fn to_value(&self) -> Value {
//         Value::Unit
//     }
// }
// impl ToValue for bool {
//     fn to_value(&self) -> Value {
//         Value::Bool(*self)
//     }
// }
// impl ToValue for i64 {
//     fn to_value(&self) -> Value {
//         Value::Int(*self)
//     }
// }
// impl ToValue for String {
//     fn to_value(&self) -> Value {
//         Value::Str(self.clone())
//     }
// }
// impl<'a> ToValue for &'a str {
//     fn to_value(&self) -> Value {
//         Value::Str(self.to_string())
//     }
// }
";

pub trait ToRustCode {
    fn to_rust(&self, env: &Ctx<Type>) -> Result<String>;
}

impl ToRustCode for Program {
    fn to_rust(&self, env: &Ctx<Type>) -> Result<String> {
        let mut out = BOILERPLATE.to_string();
        for top in self {
            match top.as_ref() {
                ast::Top::FFIFun(b, args, ret, code) => out.push_str(
                    format!(
                        "#[no_mangle]\nfn {}({}) -> std::result::Result<{}, Box<dyn std::error::Error>> {{Ok({{{}}})}}\n",
                        b,
                        args.iter()
                            .map(|(x, t)| Ok(format!("{}: {}", x, t.to_rust(env)?)))
                            .collect::<Result<std::vec::Vec<String>>>()?
                            .join(", "),
                        ret.to_rust(env)?,
                        code
                    )
                    .as_str(),
                ),
                // not useful atm
                // ast::Top::Alias(alias, ty) => {
                //     out.push_str(format!("type {} = {};\n", alias.as_ref(), ty.to_rust(env)?).as_str())
                // }
                // ast::Top::Struct(s, var) => {
                //     out.push_str(format!("struct {} {}\n", s.as_ref(), var.to_rust(env)?).as_str())
                // }
                // ast::Top::Enum(s, vars) => out.push_str(
                //     format!(
                //         "enum {} {{\n{}\n}}\n",
                //         s.as_ref(),
                //         vars.iter()
                //             .map(|(i, x)| Ok(format!("    {}{}", i.as_ref(), x.to_rust(env)?)))
                //             .collect::<Result<Vec<String>>>()?
                //             .join(",\n")
                //     )
                //     .as_str(),
                // ), 
                _ => {}
            }
        }
        Ok(out)
    }
}

impl ToRustCode for Variant {
    fn to_rust(&self, env: &Ctx<Type>) -> Result<String> {
        Ok(match self.as_ref() {
            ast::Variant::Unit => "".into(),
            ast::Variant::Tup(els) => format!(
                "({})",
                els.iter()
                    .map(|x| x.to_rust(env))
                    .collect::<Result<std::vec::Vec<String>>>()?
                    .join(", ")
            ),
            ast::Variant::Rec(fields) => format!(
                "{{\n{}\n}}",
                fields
                    .iter()
                    .map(|(id, el)| Ok(format!("    {}: {}", id, el.to_rust(env)?)))
                    .collect::<Result<std::vec::Vec<String>>>()?
                    .join(",")
            ),
        })
    }
}

impl ToRustCode for Type {
    fn to_rust(&self, env: &Ctx<Type>) -> Result<String> {
        Ok(match self.as_ref() {
            ast::Type::Name(i) => env.lookup(i).unwrap().to_rust(env)?,
            ast::Type::Unit => "()".into(),
            ast::Type::Bool => "bool".into(),
            ast::Type::Int => "i64".into(),
            ast::Type::Str => "String".into(),
            ast::Type::Tup(els) => format!(
                "({})",
                els.iter()
                    .map(|x| x.to_rust(env))
                    .collect::<Result<std::vec::Vec<String>>>()?
                    .join(", ")
            ),
            ast::Type::Rec(_) => {
                return Err(
                    Error::new("ffi does not support record types").label(self, "not supported")
                )
            }
            ast::Type::Fun(_, _) => {
                return Err(Error::new("ffi does not support higher order functions")
                    .label(self, "not supported"))
            }
            ast::Type::Struct(id, _) => id.iter().last().unwrap().to_string(),
            ast::Type::Enum(id, _) => id.iter().last().unwrap().to_string(),
        })
    }
}
