use ast::err::{Error, GetCodeRef, Result, CodeRef};
use ast::tag::{Item, Tag};

use ast::{Type, Path};
use ast::{Debruijn, Variant, _Ident, _Type, _Variant, _Vec};
use std::iter::zip;

use crate::Env;

pub trait TypeEq<T> {
    fn eq(&self, rhs: &T, env: &Env) -> Result<()>;
}

impl TypeEq<_Type<Debruijn>> for _Type<Debruijn> {
    fn eq(&self, rhs: &_Type<Debruijn>, env: &Env) -> Result<()> {
        match (self.it(), rhs.it()) {
            (Type::Var(id), _) => env.lookup(id.clone()).unwrap().eq(rhs, env),
            (_, Type::Var(id)) => self.eq(&env.lookup(id.clone()).unwrap(), env),
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Str, Type::Str) => Ok(()),
            (Type::Tup(left_els), Type::Tup(right_els)) => left_els.eq(right_els, env),
            (Type::Rec(left_fields), Type::Rec(right_fields)) => left_fields.eq(right_fields, env),
            (Type::Fun(left_args, left_ret), Type::Fun(right_args, right_ret)) => {
                left_args.eq(right_args, env)?;
                left_ret.eq(right_ret, env)
            }
            (Type::Struct(left_id, left_fields), Type::Struct(right_id, right_fields)) => {
                path_eq(left_id, right_id)?;
                left_fields.eq(right_fields, env)
            }
            (Type::Enum(left_id, _), Type::Enum(right_id, _)) => {
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
    if left.it() != right.it() {
        return Err(Error::new("path mismatch")
            .label(left, "this")
            .label(right, "that"));
    }
    Ok(())
}

pub fn len_eq<A: Item, B: Item>(left: &_Vec<Debruijn, A>, right: &_Vec<Debruijn, B>) -> Result<()> {
    if left.it().len() != right.it().len() {
        return Err(Error::new("length mismatch")
            .label(left, format!("has len {}", left.it().len()))
            .label(right, format!("has len {}", right.it().len())));
    }
    Ok(())
}

impl<A: Item + PartialEq + GetCodeRef> TypeEq<_Vec<Debruijn, (A, _Type<Debruijn>)>>
    for _Vec<Debruijn, (A, _Type<Debruijn>)>
{
    fn eq(&self, rhs: &_Vec<Debruijn, (A, _Type<Debruijn>)>, env: &Env) -> Result<()> {
        len_eq(self, rhs)?;
        for (left_id, left) in self.it() {
            rhs.it()
                .iter()
                .find(|(x, _)| x == left_id)
                .ok_or_else(|| Error::new("struct field not found").label(left_id, "not found"))?
                .1
                .eq(left, env)?
        }
        Ok(())
    }
}

impl TypeEq<_Vec<Debruijn, _Type<Debruijn>>> for _Vec<Debruijn, _Type<Debruijn>> {
    fn eq(&self, rhs: &_Vec<Debruijn, _Type<Debruijn>>, env: &Env) -> Result<()> {
        len_eq(self, rhs)?;
        for (left, right) in zip(self.it(), rhs.it()) {
            left.eq(right, env)?
        }
        Ok(())
    }
}

impl TypeEq<_Variant<Debruijn>> for _Variant<Debruijn> {
    fn eq(&self, rhs: &_Variant<Debruijn>, env: &Env) -> Result<()> {
        match (self.it(), rhs.it()) {
            (Variant::Unit, Variant::Unit) => Ok(()),
            (Variant::Tup(left_els), Variant::Tup(right_els)) => left_els.eq(right_els, env),
            (Variant::Rec(left_fields), Variant::Rec(right_fields)) => {
                left_fields.eq(right_fields, env)
            }
            (_, _) => Err(Error::new("enum variant mismatch")
                .label(self, format!("got {}", self.it()))
                .label(rhs, format!("and {}", rhs.it()))),
        }
    }
}

impl TypeEq<_Vec<Debruijn, (_Ident<Debruijn>, _Variant<Debruijn>)>>
    for _Vec<Debruijn, (_Ident<Debruijn>, _Variant<Debruijn>)>
{
    fn eq(
        &self,
        rhs: &_Vec<Debruijn, (_Ident<Debruijn>, _Variant<Debruijn>)>,
        env: &Env,
    ) -> Result<()> {
        len_eq(self, rhs)?;
        for (left, var) in self.it() {
            rhs.it()
                .iter()
                .find(|(x, _)| x == left)
                .ok_or_else(|| Error::new("enum variant not found").label(left, "not found"))?
                .1
                .eq(var, env)?
        }
        Ok(())
    }
}

impl TypeEq<_Variant<Debruijn>> for _Vec<Debruijn, _Variant<Debruijn>> {
    fn eq(&self, rhs: &_Variant<Debruijn>, env: &Env) -> Result<()> {
        let matches = self
            .iter()
            .map(|v| v.eq(rhs, env))
            .filter(|x| match x {
                Result::Ok(_) => true,
                Result::Err(_) => false,
            })
            .count();
        if matches != 1 {
            return Err(Error::new("enum variant not found")
                .label(self, format!("does not have {}", rhs))
                .label(rhs, "not found"));
        }
        Ok(())
    }
}
