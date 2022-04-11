use ast::err::{Error, Result};
use ast::tag::Item;

use ast::Type;
use ast::{Debruijn, Variant, _Ident, _Int, _Type};

pub trait TypeAt<T> {
    fn type_at(&self, i: &T) -> Result<Type<Debruijn>>;
}

impl TypeAt<_Int<Debruijn>> for _Type<Debruijn> {
    fn type_at(&self, i: &_Int<Debruijn>) -> Result<Type<Debruijn>> {
        if i.it() < &0 {
            return Err(Error::new("negative tuple projection").label(i, "is negative"));
        }
        let idx = usize::try_from(i.it().clone()).unwrap();
        match self.it() {
            Type::Tup(els) => {
                if idx >= els.it().len() {
                    return Err(Error::new("tuple index out of bounds")
                        .label(els, format!("has len {}", els.it().len()))
                        .label(i, " is out of bounds"));
                }
                Ok(els.it()[idx].it().clone())
            }
            Type::Struct(_, var) => match var.it() {
                Variant::Tup(els) => self.set(Type::Tup(els.clone())).type_at(i),
                _ => return Err(Error::new("expected tuple struct to project").label(self, "got")),
            },
            _ => return Err(Error::new("expected tuple to project").label(self, "got")),
        }
    }
}

impl TypeAt<_Ident<Debruijn>> for _Type<Debruijn> {
    fn type_at(&self, i: &_Ident<Debruijn>) -> Result<Type<Debruijn>> {
        match self.it() {
            Type::Rec(fields) => Ok(fields
                .it()
                .iter()
                .find(|(x, _)| x == i)
                .ok_or_else(|| {
                    Error::new("struct field not found")
                        .label(i, "here")
                        .label(self, format!("has no field {}", i.it()))
                })?
                .1
                .it()
                .clone()),
            Type::Struct(_, var) => match var.it() {
                Variant::Rec(fields) => self.set(Type::Rec(fields.clone())).type_at(i),
                _ => return Err(Error::new("expected record struct to project").label(self, "got")),
            },
            _ => return Err(Error::new("expected record to project").label(self, "got")),
        }
    }
}
