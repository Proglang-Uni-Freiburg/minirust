mod dups;
mod eq;
mod proj;
mod usefulness;

use ast::err::{Error, Result};
use ast::tag::{Item, MapTag, Split, Tag};
use ast::{def_from_to_ast_types, map};
use ast::{BinOp, Constructor, Debruijn, Pattern, UnOp, Variant, _Ident, _Pattern, _Type, _Vec};
use ast::{Term, Top, Type};
use eq::len_eq;
use ffi::FFI;

use crate::dups::NoDups;
use crate::eq::TypeEq;
use crate::proj::TypeAt;
use std::iter::zip;

def_from_to_ast_types! {
    from => Debruijn,
    to => Debruijn,
    prefix => ast
}

type Ctx = ast::ctx::Ctx<FromType>;
type Env = ast::ctx::Ctx<FromType>;

pub fn type_check(program: &FromProgram) -> Result<Option<FFI>> {
    let mut ctx: Ctx = Ctx::default();
    let mut env: Ctx = Ctx::default();

    program.no_dups()?;

    let mut ffi = false;

    for top in program.it() {
        match top.it() {
            Top::Fun(id, args, ret, _) => {
                ctx = ctx.mutate(id.set(Type::Fun(args.rights(), ret.clone())))
            }
            Top::FFIFun(id, args, ret, _) => {
                ffi = true;
                ctx = ctx.mutate(id.set(Type::Fun(args.rights(), ret.clone())))
            }
            Top::Alias(id, ty) => env = env.mutate(id.set(ty.it().clone())),
            Top::Struct(id, fields) => {
                let mut path = id.tag.0.clone();
                path.push(id.it().clone());
                env = env.mutate(id.set(Type::Struct(id.set(path), fields.clone())))
            }
            Top::Enum(id, vars) => {
                let mut path = id.tag.0.clone();
                path.push(id.it().clone());
                env = env.mutate(id.set(Type::Enum(id.set(path), vars.clone())))
            }
            _ => unreachable!(),
        }
    }
    for top in program.it() {
       type_check_top(top, &ctx, &env)?;
    }
    if ffi {
        // type check rust code
        return Ok(Some(FFI::new(program, &env)?));
    }
    Ok(None)
}

fn type_check_top(top: &FromTop, ctx: &Ctx, env: &Ctx) -> Result<()> {
    match top.it() {
        Top::Fun(_, args, ret, body) => {
            args.lefts().no_dups()?;
            let mut _ctx = ctx.clone();
            for (pat, ty) in args.it() {
                types_of_pattern(pat, &ty, &mut _ctx, env)?;
            }
            for (pat, ty) in args.it() {
                usefulness::is_exhaustive(&pat.set(vec![pat.clone()]), ty, pat, env)?;
            }
            type_of_term(body, &_ctx, env)?.eq(ret, env)?;
            Ok(())
        }
        Top::FFIFun(_, args, _, _) => {
            args.lefts().no_dups()?;
            if args.len() > 1 {
                return Err(
                    Error::new("too many arguments to ffi function").label(args, "maximum is 1").help("use a struct to pass more arguments")
                );
            }
            Ok(())
        }
        Top::Alias(_, _) => Ok(()),
        Top::Struct(_, body) => body.no_dups(),
        Top::Enum(_, body) => body.no_dups(),
        _ => unreachable!(),
    }
}

fn type_of_term(term: &FromTerm, ctx: &Ctx, env: &Env) -> Result<ToType> {
    Ok(term.set(match term.it() {
        Term::Var(id) => ctx.lookup(id.clone()).unwrap().into_it(),
        Term::Unit => Type::Unit,
        Term::True => Type::Bool,
        Term::False => Type::Bool,
        Term::Int(_) => Type::Int,
        Term::Str(_) => Type::Str,
        Term::Seq(left, right) => {
            type_of_term(left, ctx, env)?;
            type_of_term(right, ctx, env)?.into_it()
        }
        Term::Tup(els) => Type::Tup(map!(type_of_term(els, ctx, env))),
        Term::Rec(fields) => Type::Rec(map!(type_of_term(fields, ctx, env))),
        Term::UnOp(op, t) => {
            let ty = type_of_term(t, ctx, env)?;
            match op.it() {
                UnOp::Not => ty.eq(&ty.set(Type::Bool), env)?,
                UnOp::Neg => ty.eq(&ty.set(Type::Int), env)?,
            }
            ty.into_it()
        }
        Term::BinOp(left, op, right) => {
            let left_ty = type_of_term(left, ctx, env)?;
            let right_ty = type_of_term(right, ctx, env)?;
            left_ty.eq(&right_ty, env)?;
            let left_ty = resolve(&left_ty, env)?;
            match op.it() {
                BinOp::Add => match left_ty.eq(&left.set(Type::Int), env) {
                    Ok(_) => Type::Int,
                    Err(_) => match left_ty.eq(&left.set(Type::Str), env) {
                        Ok(_) => Type::Str,
                        Err(_) => {
                            return Err(Error::new(
                                "expected Int or Str to apply arithmetic operation",
                            )
                            .label(term, format!("these are {}", left_ty)))
                        }
                    },
                },
                BinOp::Sub | BinOp::Mul | BinOp::Div => {
                    match left_ty.eq(&left.set(Type::Int), env) {
                        Ok(_) => Type::Int,
                        Err(_) => {
                            return Err(Error::new("expected Int to apply arithmetic operation")
                                .label(term, format!("these are {}", left_ty)))
                        }
                    }
                }
                BinOp::And | BinOp::Or => match left_ty.eq(&left.set(Type::Int), env) {
                    Ok(_) => Type::Bool,
                    Err(_) => {
                        return Err(Error::new("expected Int to apply arithmetic operation")
                            .label(term, format!("these are {}", left_ty)))
                    }
                },
                BinOp::Gt | BinOp::Gte | BinOp::Lt | BinOp::Lte => {
                    match left_ty.eq(&left.set(Type::Int), env) {
                        Ok(_) => Type::Bool,
                        Err(_) => {
                            return Err(Error::new("expected Int to apply comparison")
                                .label(term, format!("these are {}", left_ty)))
                        }
                    }
                }
                BinOp::Eq | BinOp::Neq => Type::Bool,
            }
        }
        Term::Struct(id, _, constr) => {
            let s = env.lookup(id.clone()).unwrap();
            match &s.it() {
                Type::Struct(_, s_var) => {
                    map!(type_of_constructor(constr, ctx, env)).eq(&s_var, env)?
                }
                _ => {
                    return Err(Error::new("expected struct to instantiate")
                        .label(term, format!("got {}", s)))
                }
            };
            s.into_it()
        }
        Term::Enum(id, _, var, constr) => {
            let e = env.lookup(id.clone()).unwrap();
            match &e.it() {
                Type::Enum(_, e_variants) => {
                    let e_var =
                        e_variants
                            .it()
                            .iter()
                            .find(|(x, _)| x == var)
                            .ok_or_else(|| {
                                Error::new("enum variant not found")
                                    .label(var, "here")
                                    .label(&e, format!("has no variant {}", var.it()))
                            })?;
                    e_var.1.eq(&type_of_constructor(constr, ctx, env)?, env)?;
                }
                _ => {
                    return Err(Error::new("expected enum to instantiate")
                        .label(term, format!("got {}", e)))
                }
            };
            e.into_it()
        }
        Term::App(fun, apps) => {
            let fun_ty = resolve(&type_of_term(fun, ctx, env)?, env)?;
            match fun_ty.it() {
                Type::Fun(args, ret) => {
                    args.eq(&map!(type_of_term(apps, ctx, env)), env)?;
                    ret.it().clone()
                }
                _ => {
                    return Err(Error::new("expected callable to call")
                        .label(term, format!("got {}", fun_ty)))
                }
            }
        }
        Term::TupProj(tup, int) => type_of_term(tup, ctx, env)?.type_at(int)?,
        Term::RecProj(rec, id) => type_of_term(rec, ctx, env)?.type_at(id)?,
        Term::Let(pat, t, cnt) => {
            pat.no_dups()?;
            let ty = type_of_term(t, ctx, env)?;
            let mut _ctx = ctx.clone();
            match pat.it() {
                Pattern::Var(_) => _ctx._mutate(ty.clone()),
                _ => types_of_pattern(pat, &ty, &mut _ctx, env)?,
            }
            usefulness::is_exhaustive(&pat.set(vec![pat.clone()]), &ty, &term, env)?;
            type_of_term(cnt, &_ctx, env)?.into_it()
        }
        Term::Lam(args, body) => {
            args.lefts().no_dups()?;
            for (pat, ty) in args.it() {
                usefulness::is_exhaustive(&pat.set(vec![pat.clone()]), ty, &term, env)?;
            }
            let mut _ctx = ctx.clone();
            for (pat, ty) in args.it() {
                types_of_pattern(pat, &ty, &mut _ctx, env)?;
            }
            Type::Fun(args.rights(), type_of_term(body, &_ctx, env)?)
        }
        Term::Match(t, arms) => {
            let ty = type_of_term(t, ctx, env)?;
            let arms: Tag<_, Vec<_>> = arms.set(
                arms.it()
                    .iter()
                    .map(|(pat, term)| {
                        pat.no_dups()?;
                        let mut _ctx = ctx.clone();
                        types_of_pattern(pat, &ty, &mut _ctx, env)?;
                        Ok((pat.clone(), type_of_term(term, &_ctx, env)?))
                    })
                    .collect::<Result<_>>()?,
            );
            usefulness::is_exhaustive(&arms.lefts(), &ty, &term, env)?;
            // equal return types
            let mut arms_tys = arms.rights().into_it().into_iter();
            let first = arms_tys.next().unwrap();
            for arm_ty in arms_tys {
                first.eq(&arm_ty, env)?;
            }
            first.into_it()
        }
        Term::Fun(id, args, ret, body, cnt) => {
            args.lefts().no_dups()?;
            let mut _ctx = ctx.clone();
            for (pat, ty) in args.it() {
                types_of_pattern(pat, &ty, &mut _ctx, env)?;
            }
            type_of_term(body, &_ctx, env)?.eq(ret, env)?;
            let fun_ty = id.set(Type::Fun(args.rights(), ret.clone()));
            for (pat, ty) in args.it() {
                usefulness::is_exhaustive(&pat.set(vec![pat.clone()]), ty, &term, env)?;
            }
            type_of_term(cnt, &ctx.mutate(fun_ty.clone()), env)?.into_it()
        }
    }))
}

fn types_of_pattern_rec(
    fields: &_Vec<Debruijn, (_Ident<Debruijn>, _Type<Debruijn>)>,
    pats: &_Vec<Debruijn, (_Ident<Debruijn>, Option<_Pattern<Debruijn>>)>,
    ctx: &mut Ctx,
    env: &Env,
) -> Result<()> {
    for (i, pat) in pats.it() {
        let ty = fields
            .it()
            .iter()
            .find(|x| &x.0 == i)
            .ok_or_else(|| {
                Error::new("struct field not found")
                    .label(i, "here")
                    .label(fields, format!("has no field {}", i.it()))
            })?
            .1
            .clone();
        if let Some(pat) = pat {
            types_of_pattern(pat, &ty, ctx, env)?
        } else {
            ctx._mutate(ty)
        }
    }
    Ok(())
}

fn types_of_pattern_tup(
    els: &_Vec<Debruijn, _Type<Debruijn>>,
    pats: &_Vec<Debruijn, _Pattern<Debruijn>>,
    ctx: &mut Ctx,
    env: &Env,
) -> Result<()> {
    len_eq(pats, els)?;
    for (idx, pat) in pats.it().iter().enumerate() {
        types_of_pattern(pat, &els[idx].clone(), ctx, env)?
    }
    Ok(())
}

fn types_of_pattern_variant(
    pat: &FromPattern,
    var: &FromVariant,
    ctx: &mut Ctx,
    env: &Env,
) -> Result<()> {
    match pat.it() {
        Pattern::Unit => Ok(()),
        Pattern::Tup(pats) => match var.it() {
            Variant::Tup(els) => types_of_pattern_tup(els, pats, ctx, env),
            _ => {
                return Err(Error::new("pattern type mismatch")
                    .label(pat, "here")
                    .label(var, "want"))
            }
        },
        Pattern::Rec(pats) => match var.it() {
            Variant::Rec(fields) => types_of_pattern_rec(fields, pats, ctx, env),
            _ => {
                return Err(Error::new("pattern type mismatch")
                    .label(pat, "here")
                    .label(var, "want"))
            }
        },
        _ => {
            return Err(Error::new("pattern type mismatch")
                .label(pat, "here")
                .label(var, "want"))
        }
    }
}

fn types_of_pattern(pat: &FromPattern, ty: &FromType, ctx: &mut Ctx, env: &Env) -> Result<()> {
    let ty = resolve(ty, env)?;
    match pat.it() {
        Pattern::Var(_) => {
            ctx._mutate(ty.clone());
            Ok(())
        }
        Pattern::Or(pats) => {
            let mut _ctx = Ctx::default();
            let mut pats = pats.it().clone().into_iter();
            let first = pats.next().unwrap();
            types_of_pattern(&first, &ty, &mut _ctx, env)?;
            for other in pats {
                let mut o_ctx = Ctx::default();
                types_of_pattern(&other, &ty, &mut o_ctx, env)?;
                for (t1, t2) in zip(_ctx.iter(), o_ctx.iter()) {
                    t1.eq(t2, env)?;
                }
            }
            types_of_pattern(&first, &ty, ctx, env)
        }
        Pattern::Wildcard => Ok(()),
        Pattern::Const(t) => ty.eq(&type_of_term(t, ctx, env)?, env),
        Pattern::Struct(id, _, pat) => {
            ty.eq(&id.set(Type::Var(id.clone())), env)?;
            match ty.it() {
                Type::Struct(_, var) => types_of_pattern_variant(pat, var, ctx, env),
                _ => {
                    return Err(Error::new("pattern type mismatch")
                        .label(pat, "here")
                        .label(&ty, format!("want {}", ty)))
                }
            }
        }
        Pattern::Variant(id, _, var, pat) => {
            ty.eq(&id.set(Type::Var(id.clone())), env)?;
            match ty.it() {
                Type::Enum(_, vars) => {
                    let var = vars
                        .it()
                        .iter()
                        .find(|(x, _)| x == var)
                        .ok_or_else(|| {
                            Error::new("enum variant not found")
                                .label(var, "here")
                                .label(&ty, format!("has no variant {}", var.it()))
                        })?
                        .1
                        .clone();
                    types_of_pattern_variant(pat, &var, ctx, env)
                }
                _ => {
                    return Err(
                        Error::new("pattern type mismatch").label(&ty, format!("want {}", ty))
                    )
                }
            }
        }
        Pattern::Unit => unreachable!(),
        Pattern::Tup(pats) => match ty.it() {
            Type::Tup(els) => types_of_pattern_tup(els, pats, ctx, env),
            _ => {
                return Err(Error::new("pattern type mismatch")
                    .label(pat, "here")
                    .label(&ty, format!("want {}", ty)))
            }
        },
        Pattern::Rec(pats) => match ty.it() {
            Type::Rec(fields) => types_of_pattern_rec(fields, pats, ctx, env),
            _ => {
                return Err(Error::new("pattern type mismatch")
                    .label(pat, "here")
                    .label(&ty, format!("want {}", ty)))
            }
        },
    }
}

fn type_of_constructor(constr: &FromConstructor, ctx: &Ctx, env: &Env) -> Result<ToVariant> {
    Ok(constr.set(match constr.it() {
        Constructor::Unit => Variant::Unit,
        Constructor::Tup(els) => Variant::Tup(map!(type_of_term(els, ctx, env))),
        Constructor::Rec(fields) => Variant::Rec(map!(type_of_term(fields, ctx, env))),
    }))
}

fn resolve(t: &FromType, env: &Env) -> Result<FromType> {
    match t.it() {
        Type::Var(v) => resolve(&env.lookup(v.it().clone()).unwrap(), env),
        _ => Ok(t.clone()),
    }
}
