use ast::ctx::Ctx;
use ast::err::{Error, Result};
use ast::{Debruijn, _Program, _Type, _Variant};

pub trait ToRustCode {
    fn to_rust(&self, env: &Ctx<_Type<Debruijn>>) -> Result<String>;
}

impl ToRustCode for _Program<Debruijn> {
    fn to_rust(&self, env: &Ctx<_Type<Debruijn>>) -> Result<String> {
        let mut out = "#![allow(dead_code)]\n".to_string();
        for top in self.it() {
            match top.it() {
                ast::Top::FFIFun(b, args, ret, code) => out.push_str(
                    format!(
                        "#[no_mangle]\nfn {}({}) -> {} {{{}}}\n",
                        b,
                        args.it()
                            .iter()
                            .map(|(x, t)| Ok(format!("{}: {}", x.it(), t.to_rust(env)?)))
                            .collect::<Result<Vec<String>>>()?
                            .join(", "),
                        ret.to_rust(env)?,
                        code.it()
                    )
                    .as_str(),
                ),
                // not useful atm
                /* ast::Top::Alias(alias, ty) => {
                    out.push_str(format!("type {} = {};\n", alias.it(), ty.to_rust(env)?).as_str())
                }
                ast::Top::Struct(s, var) => {
                    out.push_str(format!("struct {} {}\n", s.it(), var.to_rust(env)?).as_str())
                }
                ast::Top::Enum(s, vars) => out.push_str(
                    format!(
                        "enum {} {{\n{}\n}}\n",
                        s.it(),
                        vars.it()
                            .iter()
                            .map(|(i, x)| Ok(format!("    {}{}", i.it(), x.to_rust(env)?)))
                            .collect::<Result<Vec<String>>>()?
                            .join(",\n")
                    )
                    .as_str(),
                ), */
                _ => {}
            }
        }
        Ok(out)
    }
}

impl ToRustCode for _Variant<Debruijn> {
    fn to_rust(&self, env: &Ctx<_Type<Debruijn>>) -> Result<String> {
        Ok(match self.it() {
            ast::Variant::Unit => "".into(),
            ast::Variant::Tup(els) => format!(
                "({})",
                els.it()
                    .iter()
                    .map(|x| Ok(x.to_rust(env)?))
                    .collect::<Result<Vec<String>>>()?
                    .join(", ")
            ),
            ast::Variant::Rec(fields) => format!(
                "{{\n{}\n}}",
                fields
                    .it()
                    .iter()
                    .map(|(id, el)| Ok(format!("    {}: {}", id, el.to_rust(env)?)))
                    .collect::<Result<Vec<String>>>()?
                    .join(",")
            ),
        })
    }
}

impl ToRustCode for _Type<Debruijn> {
    fn to_rust(&self, env: &Ctx<_Type<Debruijn>>) -> Result<String> {
        Ok(match self.it() {
            ast::Type::Var(i) => env.lookup(i.it().clone()).unwrap().to_rust(env)?,
            ast::Type::Unit => "()".into(),
            ast::Type::Bool => "bool".into(),
            ast::Type::Int => "i64".into(),
            ast::Type::Str => "String".into(),
            ast::Type::Tup(els) => format!(
                "({})",
                els.it()
                    .iter()
                    .map(|x| Ok(x.to_rust(env)?))
                    .collect::<Result<Vec<String>>>()?
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
            ast::Type::Struct(id, _) => format!("{}", id.it().last().unwrap()),
            ast::Type::Enum(id, _) => format!("{}", id.it().last().unwrap()),
        })
    }
}