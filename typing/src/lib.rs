mod dups;
mod eq;
mod proj;
mod useful;

use ast::err::{Error, Result};
use ast::map;
use ast::tag::{Item, MapTag, Split};
use eq::len_eq;
use ffi::FFI;

use crate::dups::NoDups;
use crate::eq::TypeEq;
use crate::proj::TypeAt;

ast::def_ast_types! {
    type => Debruijn,
    prefix => ast
}

type Ctx = ast::ctx::Ctx<Type>;
type Env = ast::ctx::Ctx<Type>;

pub fn type_check(program: &Program) -> Result<Option<FFI>> {
    let mut ctx: Ctx = Ctx::default();
    let mut env: Ctx = Ctx::default();

    program.no_dups()?;

    let mut ffi = false;
    let mut main = false;

    for top in program {
        match top.as_ref() {
            ast::Top::Fun(id, args, ret, _) => {
                if id.as_ref() == "main" {
                    if !matches!(ret.as_ref(), ast::Type::Unit) {
                        return Err(Error::new("main function does not have unit type")
                            .label(ret, "unexpected type"));
                    }
                    if args.iter().count() > 0 {
                        return Err(Error::new("main function with arguments")
                            .label(args, "unexpected arguments"));
                    }
                    if main {
                        return Err(Error::new("duplicated main function").label(top, "here"));
                    }
                    main = true;
                }
                ctx.insert(id.to(ast::Type::Fun(args.rights(), ret.clone())))
            }
            ast::Top::FFIFun(id, args, ret, _) => {
                ffi = true;
                ctx.insert(id.to(ast::Type::Fun(args.rights(), ret.clone())))
            }
            ast::Top::Alias(id, ty) => env.insert(id.to(ty.clone().into())),
            ast::Top::Struct(id, fields) => {
                let mut path = id.tag.0.clone();
                path.push(id.clone().into());
                env.insert(id.to(ast::Type::Struct(id.to(path), fields.clone())))
            }
            ast::Top::Enum(id, vars) => {
                let mut path = id.tag.0.clone();
                path.push(id.as_ref().clone());
                env.insert(id.to(ast::Type::Enum(id.to(path), vars.clone())))
            }
            _ => unreachable!(),
        }
    }
    for top in program {
        type_check_top(top, &ctx, &env)?;
    }
    if !main {
        return Err(Error::new(format!(
            "main function not found in {}.foo",
            program.tag.0.join("/")
        )));
    }
    if ffi {
        // type check rust code
        return Ok(Some(FFI::new(program, &env)?));
    }
    Ok(None)
}

fn type_check_top(top: &Top, ctx: &Ctx, env: &Ctx) -> Result<()> {
    match top.as_ref() {
        ast::Top::Fun(_, args, ret, body) => {
            args.lefts().no_dups()?;
            let mut _ctx = ctx.clone();
            for (pat, ty) in args {
                types_of_pattern(pat, ty, &mut _ctx, env)?;
            }
            for (pat, ty) in args {
                useful::is_exhaustive(&pat.to(vec![pat.clone()]), ty, pat, env)?;
            }
            type_of_term(body, &_ctx, env)?.eq(ret, env)?;
            Ok(())
        }
        ast::Top::FFIFun(_, args, _, _) => {
            args.lefts().no_dups()?;
            if args.iter().count() > 2 {
                return Err(
                    Error::new("too many arguments to ffi function").label(args, "maximum is 2")
                );
            }
            Ok(())
        }
        ast::Top::Alias(_, _) => Ok(()),
        ast::Top::Struct(_, body) => body.no_dups(),
        ast::Top::Enum(_, body) => body.no_dups(),
        _ => unreachable!(),
    }
}

fn type_of_term(term: &Term, ctx: &Ctx, env: &Env) -> Result<Type> {
    Ok(term.to(match term.as_ref() {
        ast::Term::Var(id) => ctx.lookup(id).unwrap().into(),
        ast::Term::Unit => ast::Type::Unit,
        ast::Term::True => ast::Type::Bool,
        ast::Term::False => ast::Type::Bool,
        ast::Term::Int(_) => ast::Type::Int,
        ast::Term::Str(_) => ast::Type::Str,
        ast::Term::Seq(left, right) => {
            type_of_term(left, ctx, env)?;
            type_of_term(right, ctx, env)?.into()
        }
        ast::Term::Tup(els) => ast::Type::Tup(map!(type_of_term(els, ctx, env))),
        ast::Term::Rec(fields) => ast::Type::Rec(map!(type_of_term(fields, ctx, env))),
        ast::Term::UnOp(op, t) => {
            let ty = type_of_term(t, ctx, env)?;
            match op.as_ref() {
                ast::UnOp::Not => ty.eq(&ty.to(ast::Type::Bool), env)?,
                ast::UnOp::Neg => ty.eq(&ty.to(ast::Type::Int), env)?,
            }
            ty.into()
        }
        ast::Term::BinOp(left, op, right) => {
            let left_ty = type_of_term(left, ctx, env)?;
            let right_ty = type_of_term(right, ctx, env)?;
            left_ty.eq(&right_ty, env)?;
            let left_ty = resolve(&left_ty, env)?;
            match op.as_ref() {
                ast::BinOp::Add => match left_ty.eq(&left.to(ast::Type::Int), env) {
                    Ok(_) => ast::Type::Int,
                    Err(_) => match left_ty.eq(&left.to(ast::Type::Str), env) {
                        Ok(_) => ast::Type::Str,
                        Err(_) => {
                            return Err(Error::new(
                                "expected Int or Str to apply arithmetic operation",
                            )
                            .label(term, format!("these are {}", left_ty)))
                        }
                    },
                },
                ast::BinOp::Sub | ast::BinOp::Mul | ast::BinOp::Div => {
                    match left_ty.eq(&left.to(ast::Type::Int), env) {
                        Ok(_) => ast::Type::Int,
                        Err(_) => {
                            return Err(Error::new("expected Int to apply arithmetic operation")
                                .label(term, format!("these are {}", left_ty)))
                        }
                    }
                }
                ast::BinOp::And | ast::BinOp::Or => {
                    match left_ty.eq(&left.to(ast::Type::Bool), env) {
                        Ok(_) => ast::Type::Bool,
                        Err(_) => {
                            return Err(Error::new("expected Bool to apply arithmetic operation")
                                .label(term, format!("these are {}", left_ty)))
                        }
                    }
                }
                ast::BinOp::Gt | ast::BinOp::Gte | ast::BinOp::Lt | ast::BinOp::Lte => {
                    match left_ty.eq(&left.to(ast::Type::Int), env) {
                        Ok(_) => ast::Type::Bool,
                        Err(_) => {
                            return Err(Error::new("expected Int to apply comparison")
                                .label(term, format!("these are {}", left_ty)))
                        }
                    }
                }
                ast::BinOp::Eq | ast::BinOp::Neq => ast::Type::Bool,
            }
        }
        ast::Term::Struct(id, _, constr) => {
            let s = env.lookup(id).unwrap();
            match &s.as_ref() {
                ast::Type::Struct(_, s_var) => {
                    map!(type_of_constructor(constr, ctx, env)).eq(s_var, env)?
                }
                _ => {
                    return Err(Error::new("expected struct to instantiate")
                        .label(term, format!("got {}", s)))
                }
            };
            s.into()
        }
        ast::Term::Enum(id, _, var, constr) => {
            let e = env.lookup(id).unwrap();
            match &e.as_ref() {
                ast::Type::Enum(_, e_variants) => {
                    let e_var = e_variants.iter().find(|(x, _)| x == var).ok_or_else(|| {
                        Error::new("enum variant not found")
                            .label(var, "here")
                            .label(&e, format!("has no variant {}", var))
                    })?;
                    e_var.1.eq(&type_of_constructor(constr, ctx, env)?, env)?;
                }
                _ => {
                    return Err(Error::new("expected enum to instantiate")
                        .label(term, format!("got {}", e)))
                }
            };
            e.into()
        }
        ast::Term::App(fun, apps) => {
            let fun_ty = resolve(&type_of_term(fun, ctx, env)?, env)?;
            match fun_ty.as_ref() {
                ast::Type::Fun(args, ret) => {
                    args.eq(&map!(type_of_term(apps, ctx, env)), env)?;
                    ret.clone().into()
                }
                _ => {
                    return Err(Error::new("expected callable to call")
                        .label(term, format!("got {}", fun_ty)))
                }
            }
        }
        ast::Term::TupProj(tup, int) => {
            resolve(&type_of_term(tup, ctx, env)?, env)?.type_at(int)?
        }
        ast::Term::RecProj(rec, id) => resolve(&type_of_term(rec, ctx, env)?, env)?.type_at(id)?,
        ast::Term::Let(pat, a, t, cnt) => {
            pat.no_dups()?;
            let ty = type_of_term(t, ctx, env)?;
            if let Some(a) = a {
                ty.eq(a, env)?;
            }
            let mut _ctx = ctx.clone();
            types_of_pattern(pat, &ty, &mut _ctx, env)?;
            useful::is_exhaustive(&pat.to(vec![pat.clone()]), &ty, term, env)?;
            type_of_term(cnt, &_ctx, env)?.into()
        }
        ast::Term::Assign(var, t, cnt) => {
            type_of_term(t, ctx, env)?.eq(&ctx.lookup(var).unwrap(), env)?;
            type_of_term(cnt, ctx, env)?.into()
        }
        ast::Term::Lam(args, body) => {
            args.lefts().no_dups()?;
            for (pat, ty) in args {
                useful::is_exhaustive(&pat.to(vec![pat.clone()]), ty, term, env)?;
            }
            let mut _ctx = ctx.clone();
            for (pat, ty) in args {
                types_of_pattern(pat, ty, &mut _ctx, env)?;
            }
            ast::Type::Fun(args.rights(), type_of_term(body, &_ctx, env)?)
        }
        ast::Term::Match(t, arms) => {
            let ty = type_of_term(t, ctx, env)?;
            let arms: Vec<_> = arms.to(arms
                .iter()
                .map(|(pat, term)| {
                    pat.no_dups()?;
                    let mut _ctx = ctx.clone();
                    types_of_pattern(pat, &ty, &mut _ctx, env)?;
                    Ok((pat.clone(), type_of_term(term, &_ctx, env)?))
                })
                .collect::<Result<_>>()?);
            useful::is_exhaustive(&arms.lefts(), &ty, term, env)?;
            // equal return types
            let mut arms_tys = arms.rights().into().into_iter();
            let first = arms_tys.next().unwrap();
            for arm_ty in arms_tys {
                first.eq(&arm_ty, env)?;
            }
            first.into()
        }
        ast::Term::Fun(id, args, ret, body, cnt) => {
            args.lefts().no_dups()?;
            let mut _ctx = ctx.clone();
            for (pat, ty) in args {
                types_of_pattern(pat, ty, &mut _ctx, env)?;
            }
            type_of_term(body, &_ctx, env)?.eq(ret, env)?;
            for (pat, ty) in args {
                useful::is_exhaustive(&pat.to(vec![pat.clone()]), ty, term, env)?;
            }
            type_of_term(
                cnt,
                &ctx.mutate(id.to(ast::Type::Fun(args.rights(), ret.clone()))),
                env,
            )?
            .into()
        }
    }))
}

fn types_of_pattern_rec(
    fields: &Vec<(Ident, Type)>,
    pats: &Vec<(Ident, Option<Pattern>)>,
    ctx: &mut Ctx,
    env: &Env,
) -> Result<()> {
    for (i, pat) in pats {
        let ty = fields
            .iter()
            .find(|x| &x.0 == i)
            .ok_or_else(|| {
                Error::new("struct field not found")
                    .label(i, "here")
                    .label(fields, format!("has no field {}", i.as_ref()))
            })?
            .1
            .clone();
        if let Some(pat) = pat {
            types_of_pattern(pat, &ty, ctx, env)?
        } else {
            ctx.insert(ty)
        }
    }
    Ok(())
}

fn types_of_pattern_tup(
    els: &Vec<Type>,
    pats: &Vec<Pattern>,
    ctx: &mut Ctx,
    env: &Env,
) -> Result<()> {
    len_eq(pats, els)?;
    for (idx, pat) in pats.iter().enumerate() {
        types_of_pattern(pat, &els[idx].clone(), ctx, env)?
    }
    Ok(())
}

fn types_of_pattern_variant(pat: &Pattern, var: &Variant, ctx: &mut Ctx, env: &Env) -> Result<()> {
    match pat.as_ref() {
        ast::Pattern::Unit => Ok(()),
        ast::Pattern::Tup(pats) => match var.as_ref() {
            ast::Variant::Tup(els) => types_of_pattern_tup(els, pats, ctx, env),
            _ => Err(Error::new("pattern type mismatch")
                .label(pat, "here")
                .label(var, "want")),
        },
        ast::Pattern::Rec(pats) => match var.as_ref() {
            ast::Variant::Rec(fields) => types_of_pattern_rec(fields, pats, ctx, env),
            _ => Err(Error::new("pattern type mismatch")
                .label(pat, "here")
                .label(var, "want")),
        },
        _ => Err(Error::new("pattern type mismatch")
            .label(pat, "here")
            .label(var, "want")),
    }
}

fn types_of_pattern(pat: &Pattern, ty: &Type, ctx: &mut Ctx, env: &Env) -> Result<()> {
    let ty = resolve(ty, env)?;
    match pat.as_ref() {
        ast::Pattern::Var(_) => {
            ctx.insert(ty.clone());
            Ok(())
        }
        ast::Pattern::Or(pats) => {
            let mut _ctx = Ctx::default();
            let mut pats = pats.clone().into_iter();
            let first = pats.next().unwrap();
            types_of_pattern(&first, &ty, &mut _ctx, env)?;
            for other in pats {
                let mut o_ctx = Ctx::default();
                types_of_pattern(&other, &ty, &mut o_ctx, env)?;
                for (t1, t2) in _ctx.iter().zip(o_ctx.iter()) {
                    t1.eq(t2, env)?;
                }
            }
            types_of_pattern(&first, &ty, ctx, env)
        }
        ast::Pattern::Wildcard => Ok(()),
        ast::Pattern::Const(t) => ty.eq(&type_of_term(t, ctx, env)?, env),
        ast::Pattern::Struct(id, _, pat) => {
            ty.eq(&id.to(ast::Type::Name(id.clone())), env)?;
            match ty.as_ref() {
                ast::Type::Struct(_, var) => types_of_pattern_variant(pat, var, ctx, env),
                _ => Err(Error::new("pattern type mismatch")
                    .label(pat, "here")
                    .label(&ty, "want")),
            }
        }
        ast::Pattern::Variant(id, _, var, pat) => {
            ty.eq(&id.to(ast::Type::Name(id.clone())), env)?;
            match ty.as_ref() {
                ast::Type::Enum(_, vars) => {
                    let var = vars
                        .iter()
                        .find(|(x, _)| x == var)
                        .ok_or_else(|| {
                            Error::new("enum variant not found")
                                .label(var, "here")
                                .label(&ty, format!("has no variant {}", var))
                        })?
                        .1
                        .clone();
                    types_of_pattern_variant(pat, &var, ctx, env)
                }
                _ => Err(Error::new("pattern type mismatch").label(&ty, "want")),
            }
        }
        ast::Pattern::Unit => unreachable!(),
        ast::Pattern::Tup(pats) => match ty.as_ref() {
            ast::Type::Tup(els) => types_of_pattern_tup(els, pats, ctx, env),
            _ => Err(Error::new("pattern type mismatch")
                .label(pat, "here")
                .label(&ty, "want")),
        },
        ast::Pattern::Rec(pats) => match ty.as_ref() {
            ast::Type::Rec(fields) => types_of_pattern_rec(fields, pats, ctx, env),
            _ => Err(Error::new("pattern type mismatch")
                .label(pat, "here")
                .label(&ty, "want")),
        },
    }
}

fn type_of_constructor(constr: &Constructor, ctx: &Ctx, env: &Env) -> Result<Variant> {
    Ok(constr.to(match constr.as_ref() {
        ast::Constructor::Unit => ast::Variant::Unit,
        ast::Constructor::Tup(els) => ast::Variant::Tup(map!(type_of_term(els, ctx, env))),
        ast::Constructor::Rec(fields) => ast::Variant::Rec(map!(type_of_term(fields, ctx, env))),
    }))
}

fn resolve(t: &Type, env: &Env) -> Result<Type> {
    match t.as_ref() {
        ast::Type::Name(v) => resolve(&env.lookup(v).unwrap(), env),
        _ => Ok(t.clone()),
    }
}
