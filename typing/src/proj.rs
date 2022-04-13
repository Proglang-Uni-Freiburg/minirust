use ast::err::{Error, Result};
use ast::tag::Item;
use ast::Debruijn;

use crate::{Ident, Int, Type};

pub trait TypeAt<T> {
    fn type_at(&self, i: &T) -> Result<ast::Type<Debruijn>>;
}

impl TypeAt<Int> for Type {
    fn type_at(&self, i: &Int) -> Result<ast::Type<Debruijn>> {
        if i.as_ref() < &0 {
            return Err(Error::new("negative tuple projection").label(i, "is negative"));
        }
        let idx = usize::try_from(i.clone().into()).unwrap();
        match self.as_ref() {
            ast::Type::Tup(els) => {
                if idx >= els.iter().count() {
                    return Err(Error::new("tuple index out of bounds")
                        .label(els, format!("has len {}", els.as_ref().len()))
                        .label(i, " is out of bounds"));
                }
                Ok(els.as_ref()[idx].as_ref().clone())
            }
            ast::Type::Struct(_, var) => match var.as_ref() {
                ast::Variant::Tup(els) => self.to(ast::Type::Tup(els.clone())).type_at(i),
                _ => Err(Error::new("expected tuple struct to project").label(self, "got")),
            },
            _ => Err(Error::new("expected tuple to project").label(self, "got")),
        }
    }
}

impl TypeAt<Ident> for Type {
    fn type_at(&self, i: &Ident) -> Result<ast::Type<Debruijn>> {
        match self.as_ref() {
            ast::Type::Rec(fields) => Ok(fields
                .iter()
                .find(|(x, _)| x == i)
                .ok_or_else(|| {
                    Error::new("struct field not found")
                        .label(i, "here")
                        .label(self, format!("has no field {}", i.as_ref()))
                })?
                .1
                .clone()
                .into()),
            ast::Type::Struct(_, var) => match var.as_ref() {
                ast::Variant::Rec(fields) => self.to(ast::Type::Rec(fields.clone())).type_at(i),
                _ => Err(Error::new("expected record struct to project").label(self, "got")),
            },
            _ => Err(Error::new("expected record to project").label(self, "got")),
        }
    }
}
