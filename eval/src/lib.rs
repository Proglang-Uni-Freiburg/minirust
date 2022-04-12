use ast::tag::MapTag;
use ast::tag::Split;
use ast::{
    ctx::Ctx,
    err::{Error, Result},
    map, Value,
};
use ast::{BinOp, Body, Constructor, Debruijn};
use ffi::FFI;

ast::def_from_to_ast_types! {
    from => Debruijn,
    to => Debruijn,
    prefix => ast
}

pub fn eval(program: &FromProgram, ffi: &Option<FFI>) -> Result<ToValue> {
    let mut ctx: Ctx<ToValue> = Ctx::default();
    let mut main: Option<&FromTerm> = None;
    for top in program.it() {
        match top.it() {
            ast::Top::Fun(id, args, _, body) => {
                ctx.insert(top.set(Value::TopClos(args.lefts(), body.clone())));
                if id.it() == "main" {
                    main = Some(body);
                }
            }
            ast::Top::FFIFun(id, args, ty, _) => {
                ctx.insert(top.set(Value::FFIClos(id.clone(), args.rights(), ty.clone())));
            }
            _ => {}
        }
    }
    // println!("CTX {}", ctx.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(","));
    let top_ctx = ctx.clone();
    eval_term(main.unwrap(), &mut ctx, &top_ctx, &ffi)
}

fn eval_term(
    t: &FromTerm,
    ctx: &mut Ctx<ToValue>,
    top_ctx: &Ctx<ToValue>,
    ffi: &Option<FFI>,
) -> Result<ToValue> {
    Ok(t.set(match t.it() {
        ast::Term::Var(i) => {
            // println!("CTX {} - {}", ctx.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(","), i);
            match ctx.lookup(i.it().clone()) {
                Some(s) => s.into_it(),
                None => return Err(Error::new("interpreter bug").label(t, "should be resolved")),
            }
        }
        ast::Term::Unit => Value::Unit,
        ast::Term::True => Value::Bool(true),
        ast::Term::False => Value::Bool(false),
        ast::Term::Int(i) => Value::Int(i.clone()),
        ast::Term::Str(s) => Value::Str(s.clone()),
        ast::Term::Seq(left, right) => {
            eval_term(left, ctx, top_ctx, ffi)?;
            eval_term(right, ctx, top_ctx, ffi)?.into_it()
        }
        ast::Term::Tup(els) => Value::Tup(map!(eval_term(els, ctx, top_ctx, ffi))),
        ast::Term::Rec(fields) => Value::Rec(map!(eval_term(fields, ctx, top_ctx, ffi))),
        ast::Term::UnOp(op, it) => match (op.it(), eval_term(it, ctx, top_ctx, ffi)?.it()) {
            (ast::UnOp::Not, Value::Bool(true)) => Value::Bool(false),
            (ast::UnOp::Not, Value::Bool(false)) => Value::Bool(true),
            (ast::UnOp::Neg, Value::Int(i)) => Value::Int(-i.clone()),
            _ => unimplemented!(),
        },
        ast::Term::BinOp(left, op, right) => {
            match (
                eval_term(left, ctx, top_ctx, ffi)?.it(),
                op.it(),
                eval_term(right, ctx, top_ctx, ffi)?.it(),
            ) {
                (
                    Value::Int(i1),
                    op @ (BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div),
                    Value::Int(i2),
                ) => match op {
                    BinOp::Add => Value::Int(i1 + i2),
                    BinOp::Sub => Value::Int(i1 - i2),
                    BinOp::Mul => Value::Int(i1 * i2),
                    BinOp::Div => Value::Int(i1 / i2),
                    _ => unreachable!(),
                },
                (Value::Str(s1), BinOp::Add, Value::Str(s2)) => Value::Str(s1.clone() + s2),
                (
                    Value::Int(i1),
                    op @ (BinOp::Gt | BinOp::Gte | BinOp::Lt | BinOp::Lte),
                    Value::Int(i2),
                ) => match op {
                    BinOp::Gt => Value::Bool(i1 > i2),
                    BinOp::Gte => Value::Bool(i1 >= i2),
                    BinOp::Lt => Value::Bool(i1 < i2),
                    BinOp::Lte => Value::Bool(i1 <= i2),
                    _ => unreachable!(),
                },
                (Value::Bool(b1), op @ (BinOp::And | BinOp::Or), Value::Bool(b2)) => match op {
                    BinOp::Add => Value::Bool(b1.clone() && b2.clone()),
                    BinOp::Or => Value::Bool(b1.clone() || b2.clone()),
                    _ => unreachable!(),
                },
                (v1, op @ (BinOp::Eq | BinOp::Neq), v2) => match op {
                    BinOp::Eq => Value::Bool(v1.eq(v2)),
                    BinOp::Neq => Value::Bool(!v1.eq(v2)),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
        ast::Term::Struct(_, id, var) => {
            Value::Struct(id.clone(), eval_variant(var, ctx, top_ctx, ffi)?)
        }
        ast::Term::Enum(_, id, var_name, var) => Value::Enum(
            id.clone(),
            var_name.clone(),
            eval_variant(var, ctx, top_ctx, ffi)?,
        ),
        ast::Term::App(lam, app) => match eval_term(lam, ctx, top_ctx, ffi)?.it() {
            Value::Clos(pats, body, _ctx) => {
                let mut _ctx = _ctx.clone();
                for (pat, val) in pats
                    .it()
                    .iter()
                    .zip(map!(eval_term(app, ctx, top_ctx, ffi)).into_it())
                {
                    eval_pattern(pat, val, &mut _ctx).unwrap()
                }
                eval_term(body, &mut _ctx, top_ctx, ffi)?.into_it()
            }
            Value::TopClos(pats, body) => {
                let mut _ctx = top_ctx.clone();
                for (pat, val) in pats
                    .it()
                    .iter()
                    .zip(map!(eval_term(app, ctx, top_ctx, ffi)).into_it())
                {
                    eval_pattern(pat, val, &mut _ctx).unwrap()
                }
                eval_term(body, &mut _ctx, top_ctx, ffi)?.into_it()
            }
            Value::FFIClos(id, tys, ty) => match ffi {
                Some(f) => {
                    let apps = map!(eval_term(app, ctx, top_ctx, ffi));
                    f.call(
                        id,
                        &app.set(
                            apps.into_it()
                                .into_iter()
                                .zip(tys.clone().into_it().into_iter())
                                .collect(),
                        ),
                        ty,
                    )?
                    .into_it()
                }
                None => unreachable!(),
            },
            _ => unreachable!(),
        },
        ast::Term::TupProj(t, i) => {
            let els = match eval_term(t, ctx, top_ctx, ffi)?.it() {
                Value::Tup(els) => els.clone(),
                Value::Struct(_, var) | Value::Enum(_, _, var) => match var.it() {
                    Body::Tup(els) => els.clone(),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            els.it()[(i.it().clone() as usize)].it().clone()
        }
        ast::Term::RecProj(t, s) => {
            let fields = match eval_term(t, ctx, top_ctx, ffi)?.it() {
                Value::Rec(fields) => fields.clone(),
                Value::Struct(_, var) | Value::Enum(_, _, var) => match var.it() {
                    Body::Rec(fields) => fields.clone(),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            fields
                .it()
                .iter()
                .filter(|(x, _)| x == s)
                .map(|(_, x)| x)
                .collect::<Vec<&ToValue>>()[0]
                .clone()
                .into_it()
        }
        ast::Term::Let(pat, body, cnt) => {
            let mut _ctx = ctx.clone();
            eval_pattern(pat, eval_term(body, ctx, top_ctx, ffi)?, &mut _ctx).unwrap();
            eval_term(cnt, &mut _ctx, top_ctx, ffi)?.into_it()
        }
        ast::Term::Assign(var, t, cnt) => {
            let idx = var.it().clone();
            let t = eval_term(t, ctx, top_ctx, ffi)?;
            ctx.set(idx, t);
            eval_term(cnt, ctx, top_ctx, ffi)?.into_it()
        }
        ast::Term::Lam(args, body) => Value::Clos(args.lefts(), body.clone(), ctx.clone()),
        ast::Term::Match(m, pats) => {
            let val = eval_term(m, ctx, top_ctx, ffi)?;
            for (pat, term) in pats.it() {
                let mut _ctx = ctx.clone();
                // one needs to match bc exhaustive
                match eval_pattern(pat, val.clone(), &mut _ctx) {
                    Ok(_) => return Ok(t.set(eval_term(term, &mut _ctx, top_ctx, ffi)?.into_it())),
                    Err(_) => continue,
                }
            }
            unreachable!()
        }
        ast::Term::Fun(_, args, _, body, cnt) => eval_term(
            cnt,
            &mut ctx.mutate(t.set(Value::Clos(args.lefts(), body.clone(), ctx.clone()))),
            top_ctx,
            ffi,
        )?
        .into_it(),
    }))
}

fn eval_variant(
    var: &FromConstructor,
    ctx: &mut Ctx<ToValue>,
    top_ctx: &Ctx<ToValue>,
    ffi: &Option<FFI>,
) -> Result<ToBody> {
    Ok(var.set(match var.it() {
        Constructor::Unit => Body::Unit,
        Constructor::Tup(els) => Body::Tup(map!(eval_term(els, ctx, top_ctx, ffi))),
        Constructor::Rec(fields) => Body::Rec(map!(eval_term(fields, ctx, top_ctx, ffi))),
    }))
}

fn eval_pattern_tup(
    els1: &FromVec<FromPattern>,
    els2: &FromVec<FromValue>,
    ctx: &mut Ctx<ToValue>,
) -> std::result::Result<(), ()> {
    let mut _ctx = ctx.clone();
    for (p, b) in els1.it().iter().zip(els2.it().iter()) {
        match eval_pattern(p, b.clone(), &mut _ctx) {
            Ok(_) => {}
            Err(_) => return Err(()),
        }
    }
    for (p, b) in els1.it().iter().zip(els2.it().iter()) {
        eval_pattern(p, b.clone(), ctx)?;
    }
    Ok(())
}

fn eval_pattern_rec(
    fields1: &FromVec<(FromIdent, Option<FromPattern>)>,
    fields2: &FromVec<(FromIdent, FromValue)>,
    ctx: &mut Ctx<ToValue>,
) -> std::result::Result<(), ()> {
    let mut _ctx = ctx.clone();
    for (label, field) in fields1.it().iter() {
        match fields2.it().iter().find(|(x, _)| x == label) {
            Some(v) => match field {
                Some(pat) => match eval_pattern(pat, v.1.clone(), &mut _ctx) {
                    Ok(_) => {}
                    Err(_) => return Err(()),
                },
                None => _ctx.insert(v.1.clone()),
            },
            None => return Err(()),
        }
    }
    for (label, field) in fields1.it().iter() {
        match fields2.it().iter().find(|(x, _)| x == label) {
            Some(v) => match field {
                Some(pat) => {
                    eval_pattern(pat, v.1.clone(), ctx)?;
                }
                None => ctx.insert(v.1.clone()),
            },
            None => unreachable!(),
        }
    }
    Ok(())
}

fn eval_pattern_variant(
    var: &FromPattern,
    body: FromBody,
    ctx: &mut Ctx<ToValue>,
) -> std::result::Result<(), ()> {
    match var.it() {
        ast::Pattern::Unit => {}
        ast::Pattern::Tup(els1) => match body.into_it() {
            Body::Tup(els2) => eval_pattern_tup(els1, &els2, ctx)?,
            _ => return Err(()),
        },
        ast::Pattern::Rec(fields1) => match body.into_it() {
            Body::Rec(fields2) => eval_pattern_rec(fields1, &fields2, ctx)?,
            _ => return Err(()),
        },
        _ => unreachable!(),
    };
    Ok(())
}

fn eval_pattern(
    pat: &FromPattern,
    val: FromValue,
    ctx: &mut Ctx<ToValue>,
) -> std::result::Result<(), ()> {
    match pat.it() {
        ast::Pattern::Var(_) => ctx.insert(val),
        ast::Pattern::Wildcard => return Ok(()),
        ast::Pattern::Const(c) => match c.it() {
            ast::Term::Unit if matches!(val.it(), Value::Unit) => {}
            ast::Term::True if matches!(val.it(), Value::Bool(true)) => {}
            ast::Term::False if matches!(val.it(), Value::Bool(false)) => {}
            ast::Term::Int(i1) => match val.it() {
                Value::Int(i2) if i1 == i2 => {}
                _ => return Err(()),
            },
            ast::Term::Str(s1) => match val.it() {
                Value::Str(s2) if s1 == s2 => {}
                _ => return Err(()),
            },
            _ => return Err(()),
        },
        ast::Pattern::Struct(_, p1, var) => match val.into_it() {
            Value::Struct(p2, body) if p1 == &p2 => eval_pattern_variant(var, body, ctx)?,
            _ => return Err(()),
        },
        ast::Pattern::Variant(_, p1, vn1, var) => match val.into_it() {
            Value::Enum(p2, vn2, body) if p1 == &p2 && vn1 == &vn2 => {
                eval_pattern_variant(var, body, ctx)?
            }
            _ => return Err(()),
        },
        ast::Pattern::Or(pats) => {
            let mut _ctx = ctx.clone();
            for pat in pats.it() {
                match eval_pattern(pat, val.clone(), &mut _ctx) {
                    Ok(_) => {
                        eval_pattern(pat, val.clone(), ctx)?;
                        return Ok(());
                    }
                    Err(_) => {}
                }
            }
            return Err(());
        }
        ast::Pattern::Tup(els1) => match val.into_it() {
            Value::Tup(els2) if els1.it().len() == els2.it().len() => {
                eval_pattern_tup(els1, &els2, ctx)?
            }
            _ => return Err(()),
        },
        ast::Pattern::Rec(fields1) => match val.into_it() {
            Value::Rec(fields2) if fields1.it().len() == fields2.it().len() => {
                eval_pattern_rec(fields1, &fields2, ctx)?
            }
            _ => return Err(()),
        },
        _ => return Err(()),
    };
    Ok(())
}

trait ValueEq {
    fn eq(&self, rhs: &Self) -> bool;
}

impl ValueEq for Body<Debruijn> {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Body::Unit, Body::Unit) => true,
            (Body::Tup(els1), Body::Tup(els2)) => els1
                .it()
                .iter()
                .zip(els2.it().iter())
                .all(|(x1, x2)| x1.eq(x2)),
            (Body::Rec(fields1), Body::Rec(fields2)) => fields1
                .it()
                .iter()
                .zip(fields2.it().iter())
                .all(|((n1, x1), (n2, x2))| n1 == n2 && x1.eq(x2)),
            _ => false,
        }
    }
}

impl ValueEq for Value<Debruijn> {
    fn eq(&self, rhs: &Self) -> bool {
        return match (self, rhs) {
            (Value::Unit, Value::Unit) => true,
            (Value::Bool(b1), Value::Bool(b2)) => b1.clone() == b2.clone(),
            (Value::Int(i1), Value::Int(i2)) => i1 == i2,
            (Value::Tup(els1), Value::Tup(els2)) => els1
                .it()
                .iter()
                .zip(els2.it().iter())
                .all(|(x1, x2)| x1.eq(x2)),
            (Value::Rec(fields1), Value::Rec(fields2)) => fields1
                .it()
                .iter()
                .zip(fields2.it().iter())
                .all(|((n1, x1), (n2, x2))| n1 == n2 && x1.eq(x2)),
            (Value::Struct(n1, v1), Value::Struct(n2, v2)) => n1 == n2 && v1.eq(v2),
            (Value::Enum(n1, vn1, v1), Value::Enum(n2, vn2, v2)) => {
                n1 == n2 && vn1 == vn2 && v1.eq(v2)
            }
            _ => false,
        };
    }
}
