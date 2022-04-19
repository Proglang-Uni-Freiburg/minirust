use ast::ctx::{self, Ctx};
use ast::err::{Error, GetCodeRef, Result};
use ast::tag::{MapTag, Untag};
use ast::{map, path, Constructor, Path};

ast::def_from_to_ast_types! {
    from => Named,
    to => Debruijn,
    prefix => ast
}

fn ctx_resolve<T: GetCodeRef + Untag<Vec<String>> + std::fmt::Debug>(
    ctx: &Ctx<Path>,
    id: &T,
) -> Result<usize> {
    let u = id.untag();
    if u.len() > 1 {
        if let Some(s) = ctx::resolve(ctx, u.clone()) {
            return Ok(s);
        }
    }
    let mut oid = vec![id.code_ref().0.last().unwrap().clone()];
    oid.extend(u);
    ctx::resolve(ctx, oid).ok_or_else(|| Error::new("unresolved identifier").label(id, "not found"))
}

pub fn debruijn_program(
    program: &FromProgram,
    ctx: &mut Ctx<Path>,
    env: &mut Ctx<Path>,
) -> Result<ToProgram> {
    for top in program {
        match top.as_ref() {
            ast::Top::FFIFun(bind, _, _, _) => ctx.insert(path![bind]),
            ast::Top::Fun(bind, _, _, _) => ctx.insert(path![bind]),
            ast::Top::Alias(bind, _) => env.insert(path![bind]),
            ast::Top::Struct(bind, _) => env.insert(path![bind]),
            ast::Top::Enum(bind, _) => env.insert(path![bind]),
            _ => unreachable!(),
        }
    }
    Ok(map!(debruijn_top(program, ctx, env)))
}

fn debruijn_top(top: &FromTop, ctx: &Ctx<Path>, env: &Ctx<Path>) -> Result<ToTop> {
    Ok(top.to(match top.as_ref() {
        ast::Top::Fun(bind, args, ty, body) => {
            let mut _ctx = ctx.clone();
            ast::Top::Fun(
                bind.clone(),
                args.to(args
                    .iter()
                    .map(|(pat, t)| {
                        Ok((
                            map!(debruijn_pattern(pat, &mut _ctx, env)),
                            map!(debruijn_type(t, env)),
                        ))
                    })
                    .collect::<Result<_>>()?),
                map!(debruijn_type(ty, env)),
                map!(debruijn_term(body, &_ctx, env)),
            )
        }
        ast::Top::FFIFun(bind, args, ty, body) => ast::Top::FFIFun(
            bind.clone(),
            map!(debruijn_type(args, env)),
            map!(debruijn_type(ty, env)),
            body.clone(),
        ),
        ast::Top::Alias(bind, ty) => ast::Top::Alias(bind.clone(), map!(debruijn_type(ty, env))),
        ast::Top::Struct(bind, body) => {
            ast::Top::Struct(bind.clone(), map!(debruijn_variant(body, env)))
        }
        ast::Top::Enum(bind, body) => {
            ast::Top::Enum(bind.clone(), map!(debruijn_variant(body, env)))
        }
        _ => unreachable!(),
    }))
}

fn debruijn_type(typ: &FromType, env: &Ctx<Path>) -> Result<ToType> {
    Ok(typ.to(match typ.as_ref() {
        ast::Type::Name(id) => ast::Type::Name(id.to(ctx_resolve(env, id)?)),
        ast::Type::Unit => ast::Type::Unit,
        ast::Type::Bool => ast::Type::Bool,
        ast::Type::Int => ast::Type::Int,
        ast::Type::Str => ast::Type::Str,
        ast::Type::Tup(els) => ast::Type::Tup(map!(debruijn_type(els, env))),
        ast::Type::Rec(fields) => ast::Type::Rec(map!(debruijn_type(fields, env))),
        ast::Type::Fun(in_tys, out_ty) => ast::Type::Fun(
            map!(debruijn_type(in_tys, env)),
            map!(debruijn_type(out_ty, env)),
        ),
        _ => unreachable!(),
    }))
}

fn debruijn_term(term: &FromTerm, ctx: &Ctx<Path>, env: &Ctx<Path>) -> Result<ToTerm> {
    Ok(term.to(match term.as_ref() {
        ast::Term::Unit => ast::Term::Unit,
        ast::Term::True => ast::Term::True,
        ast::Term::False => ast::Term::False,
        ast::Term::Int(i) => ast::Term::Int(*i),
        ast::Term::Str(s) => ast::Term::Str(s.clone()),
        ast::Term::Tup(els) => ast::Term::Tup(map!(debruijn_term(els, ctx, env))),
        ast::Term::Rec(fields) => ast::Term::Rec(map!(debruijn_term(fields, ctx, env))),
        ast::Term::Var(id) => {
            match ctx_resolve(ctx, id) {
                Ok(i) => ast::Term::Var(id.to(i)),
                // might be a enum / struct unit variant call (no syntactical difference)
                Err(_) => debruijn_term(
                    &(term.to(ast::Term::Struct(
                        id.clone(),
                        id.untag(),
                        id.iter().last().unwrap().to(Constructor::Unit),
                    ))),
                    ctx,
                    env,
                )?
                .into(),
            }
        }
        ast::Term::Lam(args, body) => {
            let mut _ctx = ctx.clone();
            ast::Term::Lam(
                args.to(args
                    .iter()
                    .map(|(pat, t)| {
                        Ok((
                            map!(debruijn_pattern(pat, &mut _ctx, env)),
                            map!(debruijn_type(t, env)),
                        ))
                    })
                    .collect::<Result<_>>()?),
                map!(debruijn_term(body, &_ctx, env)),
            )
        }
        ast::Term::Seq(left, right) => ast::Term::Seq(
            map!(debruijn_term(left, ctx, env)),
            map!(debruijn_term(right, ctx, env)),
        ),
        ast::Term::App(fun, args) => {
            let f = map!(debruijn_term(fun, ctx, env));
            let a = map!(debruijn_term(args, ctx, env));
            match f.as_ref() {
                // might be a enum / struct tup variant call (no syntactical difference)
                ast::Term::Enum(i, path, var, _) if args.iter().count() > 0 => ast::Term::Enum(
                    i.clone(),
                    path.clone(),
                    var.clone(),
                    a.to(Constructor::Tup(a.clone())),
                ),
                ast::Term::Struct(i, path, _) if args.iter().count() > 0 => {
                    ast::Term::Struct(i.clone(), path.clone(), a.to(Constructor::Tup(a.clone())))
                }
                _ => ast::Term::App(f, a),
            }
        }
        ast::Term::Let(pat, ty, body, cnt) => {
            let mut _ctx = ctx.clone();
            ast::Term::Let(
                map!(debruijn_pattern(pat, &mut _ctx, env)),
                map!(debruijn_type(ty, env)),
                map!(debruijn_term(body, ctx, env)),
                map!(debruijn_term(cnt, &_ctx, env)),
            )
        }
        ast::Term::Assign(var, t, cnt) => ast::Term::Assign(
            var.to(ctx_resolve(ctx, var)?),
            map!(debruijn_term(t, ctx, env)),
            map!(debruijn_term(cnt, ctx, env)),
        ),
        ast::Term::Fun(bind, args, ty, body, cnt) => {
            let mut _ctx = ctx.clone();
            ast::Term::Fun(
                bind.clone(),
                args.to(args
                    .iter()
                    .map(|(pat, t)| {
                        Ok((
                            map!(debruijn_pattern(pat, &mut _ctx, env)),
                            map!(debruijn_type(t, env)),
                        ))
                    })
                    .collect::<Result<_>>()?),
                map!(debruijn_type(ty, env)),
                map!(debruijn_term(body, &_ctx, env)),
                map!(debruijn_term(cnt, &ctx.mutate(path![bind]), env)),
            )
        }
        ast::Term::BinOp(left, op, right) => ast::Term::BinOp(
            map!(debruijn_term(left, ctx, env)),
            op.clone(),
            map!(debruijn_term(right, ctx, env)),
        ),
        ast::Term::UnOp(op, t) => ast::Term::UnOp(op.clone(), map!(debruijn_term(t, ctx, env))),
        ast::Term::Struct(id, path, constr) => {
            match ctx_resolve(env, id) {
                Ok(i) => ast::Term::Struct(
                    id.to(i),
                    path.clone(),
                    map!(debruijn_constructor(constr, ctx, env)),
                ),
                // might be a enum rec variant call (no syntactical difference)
                Err(_) => {
                    if path.len() > 1 {
                        let mut oid = id.clone().into();
                        let variant = oid.pop().unwrap();
                        let path = oid.untag();
                        debruijn_term(
                            &(term.to(ast::Term::Enum(id.to(oid), path, variant, constr.clone()))),
                            ctx,
                            env,
                        )?
                        .into()
                    } else {
                        return Err(Error::new("unresolved identifier").label(id, "not found"));
                    }
                }
            }
        }
        ast::Term::Enum(id, path, var, constr) => ast::Term::Enum(
            id.to(ctx_resolve(env, id)?),
            path.clone(),
            var.clone(),
            map!(debruijn_constructor(constr, ctx, env)),
        ),
        ast::Term::TupProj(t, i) => ast::Term::TupProj(map!(debruijn_term(t, ctx, env)), i.clone()),
        ast::Term::RecProj(t, s) => ast::Term::RecProj(map!(debruijn_term(t, ctx, env)), s.clone()),
        ast::Term::Match(t, arms) => ast::Term::Match(
            map!(debruijn_term(t, ctx, env)),
            arms.to(arms
                .iter()
                .map(|(pat, t)| {
                    let mut _ctx = ctx.clone();
                    Ok((
                        map!(debruijn_pattern(pat, &mut _ctx, env)),
                        map!(debruijn_term(t, &_ctx, env)),
                    ))
                })
                .collect::<Result<_>>()?),
        ),
    }))
}

fn debruijn_variant(var: &FromVariant, env: &Ctx<Path>) -> Result<ToVariant> {
    Ok(var.to(match var.as_ref() {
        ast::Variant::Unit => ast::Variant::Unit,
        ast::Variant::Tup(els) => ast::Variant::Tup(map!(debruijn_type(els, env))),
        ast::Variant::Rec(fields) => ast::Variant::Rec(map!(debruijn_type(fields, env))),
    }))
}

fn debruijn_constructor(
    constr: &FromConstructor,
    ctx: &Ctx<Path>,
    env: &Ctx<Path>,
) -> Result<ToConstructor> {
    Ok(constr.to(match constr.as_ref() {
        Constructor::Unit => Constructor::Unit,
        Constructor::Tup(els) => Constructor::Tup(map!(debruijn_term(els, ctx, env))),
        Constructor::Rec(fields) => Constructor::Rec(map!(debruijn_term(fields, ctx, env))),
    }))
}

fn debruijn_pattern(pat: &FromPattern, ctx: &mut Ctx<Path>, env: &Ctx<Path>) -> Result<ToPattern> {
    Ok(pat.to(match pat.as_ref() {
        ast::Pattern::Wildcard => ast::Pattern::Wildcard,
        ast::Pattern::Var(id) => {
            ctx.insert(path![id]);
            ast::Pattern::Var(id.clone())
        }
        ast::Pattern::Or(pats) => {
            let mut _ctx = Ctx::default();
            let mut iter = pats.clone().into_iter();
            let first = iter.next().unwrap();
            debruijn_pattern(&first, &mut _ctx, env)?;
            for other in iter {
                let mut o_ctx = Ctx::default();
                let pat = debruijn_pattern(&other, &mut o_ctx, env)?;
                if _ctx.iter().count() != o_ctx.iter().count() {
                    return Err(Error::new("binder amount mismatch")
                        .label(
                            &first,
                            format!(
                                "binds {}",
                                _ctx.iter()
                                    .map(|x| x.join("::"))
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            ),
                        )
                        .label(
                            &pat,
                            format!(
                                "binds {}",
                                o_ctx
                                    .iter()
                                    .map(|x| x.join("::"))
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            ),
                        ));
                }
            }
            debruijn_pattern(&first, ctx, env)?;
            ast::Pattern::Or(map!(debruijn_pattern(pats, &mut _ctx, env)))
        }
        ast::Pattern::Struct(id, path, pat) => {
            match ctx_resolve(env, id) {
                Ok(i) => ast::Pattern::Struct(
                    id.to(i),
                    path.clone(),
                    map!(debruijn_pattern(pat, ctx, env)),
                ),
                // might be variant pattern or variable
                Err(_) => {
                    if id.iter().count() > 1 {
                        let mut oid = id.clone().into();
                        let variant = oid.pop().unwrap();
                        let np = oid.untag();
                        debruijn_pattern(
                            &pat.to(ast::Pattern::Variant(id.to(oid), np, variant, pat.clone())),
                            ctx,
                            env,
                        )?
                        .into()
                    } else if let ast::Pattern::Unit = pat.as_ref() {
                        debruijn_pattern(
                            &pat.to(ast::Pattern::Var(id.clone().into_iter().next().unwrap())),
                            ctx,
                            env,
                        )?
                        .into()
                    } else {
                        return Err(Error::new("unresolved identifier").label(id, "not found"));
                    }
                }
            }
        }
        ast::Pattern::Const(t) => ast::Pattern::Const(debruijn_term(t, ctx, env)?),
        ast::Pattern::Unit => ast::Pattern::Unit,
        ast::Pattern::Tup(els) => ast::Pattern::Tup(map!(debruijn_pattern(els, ctx, env))),
        ast::Pattern::Rec(fields) => ast::Pattern::Rec(
            fields.to(fields
                .iter()
                .map(|(i, pat)| {
                    Ok((
                        i.clone(),
                        match pat {
                            Some(pat) => Some(map!(debruijn_pattern(pat, ctx, env))),
                            None => {
                                ctx.insert(path![i]);
                                None
                            }
                        },
                    ))
                })
                .collect::<Result<_>>()?),
        ),
        ast::Pattern::Variant(id, path, var, pat) => ast::Pattern::Variant(
            id.to(ctx_resolve(env, id)?),
            path.clone(),
            var.clone(),
            map!(debruijn_pattern(pat, ctx, env)),
        ),
    }))
}
