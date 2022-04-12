use ast::ctx::{self, Ctx};
use ast::err::{Error, GetCodeRef, Result};
use ast::tag::{MapTag, Tag, Untag};
use ast::{map, path};
use ast::{Constructor, Path, Pattern, Variant};
use ast::{Term, Top, Type};

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
        match ctx::resolve(ctx, u.clone()) {
            Some(s) => return Ok(s),
            None => {}
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
    for top in program.it() {
        match top.it() {
            Top::FFIFun(bind, _, _, _) => ctx.insert(path![bind]),
            Top::Fun(bind, _, _, _) => ctx.insert(path![bind]),
            Top::Alias(bind, _) => env.insert(path![bind]),
            Top::Struct(bind, _) => env.insert(path![bind]),
            Top::Enum(bind, _) => env.insert(path![bind]),
            _ => unreachable!(),
        }
    }

    Ok(map!(debruijn_top(program, ctx, env)))
}

fn debruijn_top(top: &FromTop, ctx: &Ctx<Path>, env: &Ctx<Path>) -> Result<ToTop> {
    Ok(top.set(match top.it() {
        Top::Fun(bind, args, ty, body) => {
            let mut _ctx = ctx.clone();
            Top::Fun(
                bind.clone(),
                args.set(
                    args.it()
                        .iter()
                        .map(|(pat, t)| {
                            Ok((
                                map!(debruijn_pattern(pat, &mut _ctx, env)),
                                map!(debruijn_type(t, env)),
                            ))
                        })
                        .collect::<Result<_>>()?,
                ),
                map!(debruijn_type(ty, env)),
                map!(debruijn_term(body, &_ctx, env)),
            )
        }
        Top::FFIFun(bind, args, ty, body) => Top::FFIFun(
            bind.clone(),
            map!(debruijn_type(args, env)),
            map!(debruijn_type(ty, env)),
            body.clone(),
        ),
        Top::Alias(bind, ty) => Top::Alias(bind.clone(), map!(debruijn_type(ty, env))),
        Top::Struct(bind, body) => Top::Struct(bind.clone(), map!(debruijn_variant(body, env))),
        Top::Enum(bind, body) => Top::Enum(bind.clone(), map!(debruijn_variant(body, env))),
        _ => unreachable!(),
    }))
}

fn debruijn_type(typ: &FromType, env: &Ctx<Path>) -> Result<ToType> {
    Ok(typ.set(match typ.it() {
        Type::Var(id) => Type::Var(id.set(ctx_resolve(env, id)?)),
        Type::Unit => Type::Unit,
        Type::Bool => Type::Bool,
        Type::Int => Type::Int,
        Type::Str => Type::Str,
        Type::Tup(els) => Type::Tup(map!(debruijn_type(els, env))),
        // Type::Sum(els) => Type::Sum(map!(debruijn_type(els, env))),
        Type::Rec(fields) => Type::Rec(map!(debruijn_type(fields, env))),
        Type::Fun(in_tys, out_ty) => Type::Fun(
            map!(debruijn_type(in_tys, env)),
            map!(debruijn_type(out_ty, env)),
        ),
        _ => unreachable!(),
    }))
}

fn debruijn_term(term: &FromTerm, ctx: &Ctx<Path>, env: &Ctx<Path>) -> Result<ToTerm> {
    Ok(term.set(match term.it() {
        Term::Unit => Term::Unit,
        Term::True => Term::True,
        Term::False => Term::False,
        Term::Int(i) => Term::Int(i.clone()),
        Term::Str(s) => Term::Str(s.clone()),
        Term::Tup(els) => Term::Tup(map!(debruijn_term(els, ctx, env))),
        Term::Rec(fields) => Term::Rec(map!(debruijn_term(fields, ctx, env))),
        Term::Var(id) => {
            match ctx_resolve(ctx, id) {
                Ok(i) => Term::Var(id.set(i)),
                // might be a enum / struct unit variant call (no syntactical difference)
                Err(_) => debruijn_term(
                    &term.set(Term::Struct(
                        id.clone(),
                        id.untag(),
                        Tag::new(id.it().last().unwrap().tag.clone(), Constructor::Unit),
                    )),
                    ctx,
                    env,
                )?
                .into_it(),
            }
        }
        Term::Lam(args, body) => {
            let mut _ctx = ctx.clone();
            Term::Lam(
                args.set(
                    args.it()
                        .iter()
                        .map(|(pat, t)| {
                            Ok((
                                map!(debruijn_pattern(pat, &mut _ctx, env)),
                                map!(debruijn_type(t, env)),
                            ))
                        })
                        .collect::<Result<_>>()?,
                ),
                map!(debruijn_term(body, &_ctx, env)),
            )
        }
        Term::Seq(left, right) => Term::Seq(
            map!(debruijn_term(left, ctx, env)),
            map!(debruijn_term(right, ctx, env)),
        ),
        Term::App(fun, args) => {
            let f = map!(debruijn_term(fun, ctx, env));
            let a = map!(debruijn_term(args, ctx, env));
            match f.it() {
                // might be a enum / struct tup variant call (no syntactical difference)
                Term::Enum(i, path, var, _) if args.it().len() > 0 => Term::Enum(
                    i.clone(),
                    path.clone(),
                    var.clone(),
                    a.set(Constructor::Tup(a.clone())),
                ),
                Term::Struct(i, path, _) if args.it().len() > 0 => {
                    Term::Struct(i.clone(), path.clone(), a.set(Constructor::Tup(a.clone())))
                }
                _ => Term::App(f, a),
            }
        }
        Term::Let(pat, body, cnt) => {
            let mut _ctx = ctx.clone();
            let pat = map!(debruijn_pattern(pat, &mut _ctx, env));
            match body.it() {
                Term::Seq(left, right) => Term::Let(
                    pat,
                    map!(debruijn_term(left, ctx, env)),
                    map!(debruijn_term(right, &_ctx, env)),
                ),
                _ => Term::Let(
                    pat,
                    map!(debruijn_term(body, ctx, env)),
                    map!(debruijn_term(cnt, &_ctx, env)),
                ),
            }
        }
        Term::Assign(var, t, cnt) => Term::Assign(
            var.set(ctx_resolve(ctx, var)?),
            map!(debruijn_term(t, ctx, env)),
            map!(debruijn_term(cnt, ctx, env)),
        ),
        Term::Fun(bind, args, ty, body, cnt) => {
            let mut _ctx = ctx.clone();
            Term::Fun(
                bind.clone(),
                args.set(
                    args.it()
                        .iter()
                        .map(|(pat, t)| {
                            Ok((
                                map!(debruijn_pattern(pat, &mut _ctx, env)),
                                map!(debruijn_type(t, env)),
                            ))
                        })
                        .collect::<Result<_>>()?,
                ),
                map!(debruijn_type(ty, env)),
                map!(debruijn_term(body, &_ctx, env)),
                map!(debruijn_term(cnt, &ctx.mutate(path![bind]), env)),
            )
        }
        Term::BinOp(left, op, right) => Term::BinOp(
            map!(debruijn_term(left, ctx, env)),
            op.clone(),
            map!(debruijn_term(right, ctx, env)),
        ),
        Term::UnOp(op, t) => Term::UnOp(op.clone(), map!(debruijn_term(t, ctx, env))),
        Term::Struct(id, path, constr) => {
            match ctx_resolve(env, id) {
                Ok(i) => Term::Struct(
                    id.set(i),
                    path.clone(),
                    map!(debruijn_constructor(constr, ctx, env)),
                ),
                // might be a enum rec variant call (no syntactical difference)
                Err(_) => {
                    if path.len() > 1 {
                        let mut oid = id.clone().into_it();
                        let variant = oid.pop().unwrap();
                        let path = oid.untag();
                        // let oid = oid.pop().unwrap();
                        debruijn_term(
                            &term.set(Term::Enum(id.set(oid), path, variant, constr.clone())),
                            ctx,
                            env,
                        )?
                        .into_it()
                    } else {
                        return Err(Error::new("unresolved identifier").label(id, "not found"));
                    }
                }
            }
        }
        Term::Enum(id, path, var, constr) => Term::Enum(
            id.set(ctx_resolve(env, id)?),
            path.clone(),
            var.clone(),
            map!(debruijn_constructor(constr, ctx, env)),
        ),
        Term::TupProj(t, i) => Term::TupProj(map!(debruijn_term(t, ctx, env)), i.clone()),
        Term::RecProj(t, s) => Term::RecProj(map!(debruijn_term(t, ctx, env)), s.clone()),
        Term::Match(t, arms) => Term::Match(
            map!(debruijn_term(t, ctx, env)),
            arms.set(
                arms.it()
                    .iter()
                    .map(|(pat, t)| {
                        let mut _ctx = ctx.clone();
                        Ok((
                            map!(debruijn_pattern(pat, &mut _ctx, env)),
                            map!(debruijn_term(t, &_ctx, env)),
                        ))
                    })
                    .collect::<Result<_>>()?,
            ),
        ),
    }))
}

fn debruijn_variant(var: &FromVariant, env: &Ctx<Path>) -> Result<ToVariant> {
    Ok(var.set(match var.it() {
        Variant::Unit => Variant::Unit,
        Variant::Tup(els) => Variant::Tup(map!(debruijn_type(els, env))),
        Variant::Rec(fields) => Variant::Rec(map!(debruijn_type(fields, env))),
    }))
}

fn debruijn_constructor(
    constr: &FromConstructor,
    ctx: &Ctx<Path>,
    env: &Ctx<Path>,
) -> Result<ToConstructor> {
    Ok(constr.set(match constr.it() {
        Constructor::Unit => Constructor::Unit,
        Constructor::Tup(els) => Constructor::Tup(map!(debruijn_term(els, ctx, env))),
        Constructor::Rec(fields) => Constructor::Rec(map!(debruijn_term(fields, ctx, env))),
    }))
}

fn debruijn_pattern(pat: &FromPattern, ctx: &mut Ctx<Path>, env: &Ctx<Path>) -> Result<ToPattern> {
    Ok(pat.set(match pat.it() {
        Pattern::Wildcard => Pattern::Wildcard,
        Pattern::Var(id) => {
            ctx.insert(path![id]);
            Pattern::Var(id.clone())
        }
        Pattern::Or(pats) => {
            let mut _ctx = Ctx::default();
            let mut iter = pats.it().clone().into_iter();
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
            Pattern::Or(map!(debruijn_pattern(pats, ctx, env)))
        }
        Pattern::Struct(id, path, pat) => {
            match ctx_resolve(env, id) {
                Ok(i) => Pattern::Struct(
                    id.set(i),
                    path.clone(),
                    map!(debruijn_pattern(pat, ctx, env)),
                ),
                // might be variant pattern or variable
                Err(_) => {
                    if id.it().len() > 1 {
                        let mut oid = id.clone().into_it();
                        let variant = oid.pop().unwrap();
                        let np = oid.untag();
                        // let oid = oid.pop().unwrap();
                        debruijn_pattern(
                            &pat.set(Pattern::Variant(id.set(oid), np, variant, pat.clone())),
                            ctx,
                            env,
                        )?
                        .into_it()
                    } else if let Pattern::Unit = pat.it() {
                        debruijn_pattern(
                            &pat.set(Pattern::Var(
                                id.clone().into_it().into_iter().next().unwrap(),
                            )),
                            ctx,
                            env,
                        )?
                        .into_it()
                    } else {
                        return Err(Error::new("unresolved identifier").label(id, "not found"));
                    }
                }
            }
        }
        Pattern::Const(t) => Pattern::Const(debruijn_term(t, ctx, env)?),
        Pattern::Unit => Pattern::Unit,
        Pattern::Tup(els) => Pattern::Tup(map!(debruijn_pattern(els, ctx, env))),
        Pattern::Rec(fields) => Pattern::Rec(
            fields.set(
                fields
                    .it()
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
                    .collect::<Result<_>>()?,
            ),
        ),
        Pattern::Variant(id, path, var, pat) => Pattern::Variant(
            id.set(ctx_resolve(env, id)?),
            path.clone(),
            var.clone(),
            map!(debruijn_pattern(pat, ctx, env)),
        ),
    }))
}