use std::fmt::Display;

use crate::Body;
use crate::ReprItem;
use crate::Type;
use crate::Value;
use crate::Variant;

impl<R: ReprItem> Display for Variant<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variant::Unit => Ok(()),
            Variant::Tup(els) => write!(
                f,
                "({})",
                els.iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Variant::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
        }
    }
}

impl<R: ReprItem> Display for Type<R> {
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
                els.iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Fun(args, ret) => write!(
                f,
                "({}) -> {}",
                args.iter()
                    .map(|ty| format!("{}", ty))
                    .collect::<Vec<String>>()
                    .join(","),
                ret
            ),
            Type::Struct(id, _) => write!(
                f,
                "{}",
                id.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("::")
            ),
            Type::Enum(id, _) => write!(
                f,
                "{}",
                id.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("::")
            ),
        }
    }
}

impl<R: ReprItem> Display for Body<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Body::Unit => Ok(()),
            Body::Tup(els) => write!(
                f,
                "({})",
                els.iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Body::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
        }
    }
}

impl<R: ReprItem> Display for Value<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Str(s) => write!(f, "{}", s),
            Value::Tup(els) => write!(
                f,
                "({})",
                els.iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Value::Rec(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|(id, el)| format!("{}: {}", id, el))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Value::Struct(id, _) => write!(
                f,
                "{}",
                id.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("::")
            ),
            Value::Enum(id, _, _) => write!(
                f,
                "{}",
                id.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("::")
            ),
            Value::Clos(_, _, _) => write!(f, "closure"),
            Value::TopClos(_, _) => write!(f, "toplevel closure"),
            Value::FFIClos(_, _, _) => write!(f, "ffi closure"),
        }
    }
}
