use ast::err::{CodeRef, Error, GetCodeRef, Result};
use ast::tag::{Item, Tag};
use ast::Path;
use std::iter::zip;

use crate::{Env, Ident, Type, Variant, Vec};

pub trait TypeEq<T> {
    fn eq(&self, rhs: &T, env: &Env) -> Result<()>;
}

impl TypeEq<Type> for Type {
    fn eq(&self, rhs: &Type, env: &Env) -> Result<()> {
        match (self.as_ref(), rhs.as_ref()) {
            (ast::Type::Name(id), _) => env.lookup(id).unwrap().eq(rhs, env),
            (_, ast::Type::Name(id)) => self.eq(&env.lookup(id).unwrap(), env),
            (ast::Type::Unit, ast::Type::Unit) => Ok(()),
            (ast::Type::Bool, ast::Type::Bool) => Ok(()),
            (ast::Type::Int, ast::Type::Int) => Ok(()),
            (ast::Type::Str, ast::Type::Str) => Ok(()),
            (ast::Type::Tup(left_els), ast::Type::Tup(right_els)) => left_els.eq(right_els, env),
            (ast::Type::Rec(left_fields), ast::Type::Rec(right_fields)) => {
                left_fields.eq(right_fields, env)
            }
            (ast::Type::Fun(left_args, left_ret), ast::Type::Fun(right_args, right_ret)) => {
                left_args.eq(right_args, env)?;
                left_ret.eq(right_ret, env)
            }
            (
                ast::Type::Struct(left_id, left_fields),
                ast::Type::Struct(right_id, right_fields),
            ) => {
                path_eq(left_id, right_id)?;
                left_fields.eq(right_fields, env)
            }
            (ast::Type::Enum(left_id, _), ast::Type::Enum(right_id, _)) => {
                path_eq(left_id, right_id)
                // left_vars.eq(right_vars, env)
            }
            (_, _) => Err(Error::new("type mismatch")
                .label(self, format!("got {}", self))
                .label(rhs, format!("and {}", rhs))),
        }
    }
}

pub fn path_eq(left: &Tag<CodeRef, Path>, right: &Tag<CodeRef, Path>) -> Result<()> {
    if left.as_ref() != right.as_ref() {
        return Err(Error::new("path mismatch")
            .label(left, "this")
            .label(right, "that"));
    }
    Ok(())
}

pub fn len_eq<A: Item, B: Item>(left: &Vec<A>, right: &Vec<B>) -> Result<()> {
    if left.iter().count() != right.iter().count() {
        return Err(Error::new("length mismatch")
            .label(left, format!("has len {}", left.as_ref().len()))
            .label(right, format!("has len {}", right.as_ref().len())));
    }
    Ok(())
}

impl<A: Item + PartialEq + GetCodeRef> TypeEq<Vec<(A, Type)>> for Vec<(A, Type)> {
    fn eq(&self, rhs: &Vec<(A, Type)>, env: &Env) -> Result<()> {
        len_eq(self, rhs)?;
        for (left_id, left) in self.as_ref() {
            rhs.iter()
                .find(|(x, _)| x == left_id)
                .ok_or_else(|| Error::new("struct field not found").label(left_id, "not found"))?
                .1
                .eq(left, env)?
        }
        Ok(())
    }
}

impl TypeEq<Vec<Type>> for Vec<Type> {
    fn eq(&self, rhs: &Vec<Type>, env: &Env) -> Result<()> {
        len_eq(self, rhs)?;
        for (left, right) in zip(self.as_ref(), rhs.as_ref()) {
            left.eq(right, env)?
        }
        Ok(())
    }
}

impl TypeEq<Variant> for Variant {
    fn eq(&self, rhs: &Variant, env: &Env) -> Result<()> {
        match (self.as_ref(), rhs.as_ref()) {
            (ast::Variant::Unit, ast::Variant::Unit) => Ok(()),
            (ast::Variant::Tup(left_els), ast::Variant::Tup(right_els)) => {
                left_els.eq(right_els, env)
            }
            (ast::Variant::Rec(left_fields), ast::Variant::Rec(right_fields)) => {
                left_fields.eq(right_fields, env)
            }
            (_, _) => Err(Error::new("enum variant mismatch")
                .label(self, format!("got {}", self.as_ref()))
                .label(rhs, format!("and {}", rhs.as_ref()))),
        }
    }
}

impl TypeEq<Vec<(Ident, Variant)>> for Vec<(Ident, Variant)> {
    fn eq(&self, rhs: &Vec<(Ident, Variant)>, env: &Env) -> Result<()> {
        len_eq(self, rhs)?;
        for (left, var) in self.as_ref() {
            rhs.iter()
                .find(|(x, _)| x == left)
                .ok_or_else(|| Error::new("enum variant not found").label(left, "not found"))?
                .1
                .eq(var, env)?
        }
        Ok(())
    }
}

impl TypeEq<Variant> for Vec<Variant> {
    fn eq(&self, rhs: &Variant, env: &Env) -> Result<()> {
        let matches = self
            .iter()
            .map(|v| v.eq(rhs, env))
            .filter(|x| x.is_ok())
            .count();
        if matches != 1 {
            return Err(Error::new("enum variant not found")
                .label(self, format!("does not have {}", rhs))
                .label(rhs, "not found"));
        }
        Ok(())
    }
}
