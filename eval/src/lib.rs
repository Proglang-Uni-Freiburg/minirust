use ast::tag::MapTag;
use ast::tag::Split;
use ast::Debruijn;
use ast::{
    ctx::Ctx,
    err::Result,
    map,
};
use ffi::FFI;

ast::def_ast_types! {
    type => Debruijn,
    prefix => ast
}

pub fn eval(program: &Program, ffi: &Option<FFI>) -> Result<Value> {
    let mut ctx: Ctx<Value> = Ctx::default();
    let mut main: Option<&Term> = None;
    for top in program {
        match top.as_ref() {
            ast::Top::Fun(id, args, _, body) => {
                ctx.insert(top.to(ast::Value::TopClos(args.lefts(), body.clone())));
                if id.as_ref() == "main" {
                    main = Some(body);
                }
            }
            ast::Top::FFIFun(id, args, ty, _) => {
                ctx.insert(top.to(ast::Value::FFIClos(id.clone(), args.rights(), ty.clone())));
            }
            _ => {}
        }
    }
    let top_ctx = ctx.clone();
    eval_term(main.unwrap(), &mut ctx, &top_ctx, ffi)
}

fn eval_term(
    t: &Term,
    ctx: &mut Ctx<Value>,
    top_ctx: &Ctx<Value>,
    ffi: &Option<FFI>,
) -> Result<Value> {
    Ok(t.to(match t.as_ref() {
        ast::Term::Var(i) => {
            match ctx.lookup(i) {
                Some(s) => s.into(),
                None => unreachable!(),
            }
        }
        ast::Term::Unit => ast::Value::Unit,
        ast::Term::True => ast::Value::Bool(true),
        ast::Term::False => ast::Value::Bool(false),
        ast::Term::Int(i) => ast::Value::Int(*i),
        ast::Term::Str(s) => ast::Value::Str(s.clone()),
        ast::Term::Seq(left, right) => {
            eval_term(left, ctx, top_ctx, ffi)?;
            eval_term(right, ctx, top_ctx, ffi)?.into()
        }
        ast::Term::Tup(els) => ast::Value::Tup(map!(eval_term(els, ctx, top_ctx, ffi))),
        ast::Term::Rec(fields) => ast::Value::Rec(map!(eval_term(fields, ctx, top_ctx, ffi))),
        ast::Term::UnOp(op, it) => match (op.as_ref(), &*eval_term(it, ctx, top_ctx, ffi)?) {
            (ast::UnOp::Not, ast::Value::Bool(true)) => ast::Value::Bool(false),
            (ast::UnOp::Not, ast::Value::Bool(false)) => ast::Value::Bool(true),
            (ast::UnOp::Neg, ast::Value::Int(i)) => ast::Value::Int(-i),
            _ => unimplemented!(),
        },
        ast::Term::BinOp(left, op, right) => {
            match (
                &*eval_term(left, ctx, top_ctx, ffi)?,
                op.as_ref(),
                &*eval_term(right, ctx, top_ctx, ffi)?,
            ) {
                (
                    ast::Value::Int(i1),
                    op @ (ast::BinOp::Add | ast::BinOp::Sub | ast::BinOp::Mul | ast::BinOp::Div),
                    ast::Value::Int(i2),
                ) => match op {
                    ast::BinOp::Add => ast::Value::Int(i1 + i2),
                    ast::BinOp::Sub => ast::Value::Int(i1 - i2),
                    ast::BinOp::Mul => ast::Value::Int(i1 * i2),
                    ast::BinOp::Div => ast::Value::Int(i1 / i2),
                    _ => unreachable!(),
                },
                (ast::Value::Str(s1), ast::BinOp::Add, ast::Value::Str(s2)) => {
                    ast::Value::Str(format!("{}{}", s1, s2))
                }
                (
                    ast::Value::Int(i1),
                    op @ (ast::BinOp::Gt | ast::BinOp::Gte | ast::BinOp::Lt | ast::BinOp::Lte),
                    ast::Value::Int(i2),
                ) => match op {
                    ast::BinOp::Gt => ast::Value::Bool(i1 > i2),
                    ast::BinOp::Gte => ast::Value::Bool(i1 >= i2),
                    ast::BinOp::Lt => ast::Value::Bool(i1 < i2),
                    ast::BinOp::Lte => ast::Value::Bool(i1 <= i2),
                    _ => unreachable!(),
                },
                (
                    ast::Value::Bool(b1),
                    op @ (ast::BinOp::And | ast::BinOp::Or),
                    ast::Value::Bool(b2),
                ) => match op {
                    ast::BinOp::Add => ast::Value::Bool(*b1 && *b2),
                    ast::BinOp::Or => ast::Value::Bool(*b1 || *b2),
                    _ => unreachable!(),
                },
                (v1, op @ (ast::BinOp::Eq | ast::BinOp::Neq), v2) => match op {
                    ast::BinOp::Eq => ast::Value::Bool(v1.eq(v2)),
                    ast::BinOp::Neq => ast::Value::Bool(!v1.eq(v2)),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
        ast::Term::Struct(_, id, var) => {
            ast::Value::Struct(id.clone(), eval_variant(var, ctx, top_ctx, ffi)?)
        }
        ast::Term::Enum(_, id, var_id, var) => ast::Value::Enum(
            id.clone(),
            var_id.clone(),
            eval_variant(var, ctx, top_ctx, ffi)?,
        ),
        ast::Term::App(lam, app) => match &*eval_term(lam, ctx, top_ctx, ffi)? {
            ast::Value::Clos(pats, body, _ctx) => {
                let mut _ctx = _ctx.clone();
                for (pat, val) in pats
                    .iter()
                    .zip(map!(eval_term(app, ctx, top_ctx, ffi)).iter())
                {
                    eval_pattern(pat, val, &mut _ctx).unwrap()
                }
                eval_term(body, &mut _ctx, top_ctx, ffi)?.into()
            }
            ast::Value::TopClos(pats, body) => {
                let mut _ctx = top_ctx.clone();
                for (pat, val) in pats
                    .iter()
                    .zip(map!(eval_term(app, ctx, top_ctx, ffi)).iter())
                {
                    eval_pattern(pat, val, &mut _ctx).unwrap()
                }
                eval_term(body, &mut _ctx, top_ctx, ffi)?.into()
            }
            ast::Value::FFIClos(id, tys, ret) => match ffi {
                Some(f) => {
                    let apps = map!(eval_term(app, ctx, top_ctx, ffi));
                    f.call(
                        id,
                        &(app.to(apps.into_iter().zip(tys.clone().into_iter()).collect())),
                        ret,
                    )?
                    .into()
                }
                None => unreachable!(),
            },
            _ => unreachable!(),
        },
        ast::Term::TupProj(t, i) => {
            let els = match &*eval_term(t, ctx, top_ctx, ffi)? {
                ast::Value::Tup(els) => els.clone(),
                ast::Value::Struct(_, var) | ast::Value::Enum(_, _, var) => match var.as_ref() {
                    ast::Body::Tup(els) => els.clone(),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            els.into_iter().nth(i.into()).unwrap().into()
        }
        ast::Term::RecProj(t, s) => {
            let fields = match &*eval_term(t, ctx, top_ctx, ffi)? {
                ast::Value::Rec(fields) => fields.clone(),
                ast::Value::Struct(_, var) | ast::Value::Enum(_, _, var) => match var.as_ref() {
                    ast::Body::Rec(fields) => fields.clone(),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            fields
                .into_iter()
                .filter(|(x, _)| x == s)
                .map(|(_, x)| x)
                .next()
                .unwrap()
                .into()
        }
        ast::Term::Let(pat, _, body, cnt) => {
            let mut _ctx = ctx.clone();
            eval_pattern(pat, &eval_term(body, ctx, top_ctx, ffi)?, &mut _ctx).unwrap();
            eval_term(cnt, &mut _ctx, top_ctx, ffi)?.into()
        }
        ast::Term::Lam(args, body) => ast::Value::Clos(args.lefts(), body.clone(), ctx.clone()),
        ast::Term::Match(m, pats) => {
            let val = eval_term(m, ctx, top_ctx, ffi)?;
            for (pat, term) in pats {
                let mut _ctx = ctx.clone();
                // one needs to match bc exhaustive
                match eval_pattern(pat, &val, &mut _ctx) {
                    Ok(_) => return Ok(t.to(eval_term(term, &mut _ctx, top_ctx, ffi)?.into())),
                    Err(_) => continue,
                }
            }
            unreachable!()
        }
        ast::Term::Fun(_, args, _, body, cnt) => eval_term(
            cnt,
            &mut ctx.mutate(t.to(ast::Value::Clos(args.lefts(), body.clone(), ctx.clone()))),
            top_ctx,
            ffi,
        )?
        .into(),
    }))
}

fn eval_variant(
    var: &Constructor,
    ctx: &mut Ctx<Value>,
    top_ctx: &Ctx<Value>,
    ffi: &Option<FFI>,
) -> Result<Body> {
    Ok(var.to(match var.as_ref() {
        ast::Constructor::Unit => ast::Body::Unit,
        ast::Constructor::Tup(els) => ast::Body::Tup(map!(eval_term(els, ctx, top_ctx, ffi))),
        ast::Constructor::Rec(fields) => ast::Body::Rec(map!(eval_term(fields, ctx, top_ctx, ffi))),
    }))
}

fn eval_pattern_tup(
    els1: &Vec<Pattern>,
    els2: &Vec<Value>,
    ctx: &mut Ctx<Value>,
) -> std::result::Result<(), ()> {
    let mut _ctx = ctx.clone();
    for (p, b) in els1.iter().zip(els2.iter()) {
        match eval_pattern(p, b, &mut _ctx) {
            Ok(_) => {}
            Err(_) => return Err(()),
        }
    }
    for (p, b) in els1.iter().zip(els2.iter()) {
        eval_pattern(p, b, ctx)?;
    }
    Ok(())
}

fn eval_pattern_rec(
    fields1: &Vec<(Ident, Option<Pattern>)>,
    fields2: &Vec<(Ident, Value)>,
    ctx: &mut Ctx<Value>,
) -> std::result::Result<(), ()> {
    let mut _ctx = ctx.clone();
    for (label, field) in fields1.iter() {
        match fields2.iter().find(|(x, _)| x == label) {
            Some(v) => match field {
                Some(pat) => match eval_pattern(pat, &v.1, &mut _ctx) {
                    Ok(_) => {}
                    Err(_) => return Err(()),
                },
                None => _ctx.insert(v.1.clone()),
            },
            None => return Err(()),
        }
    }
    for (label, field) in fields1.iter() {
        match fields2.iter().find(|(x, _)| x == label) {
            Some(v) => match field {
                Some(pat) => {
                    eval_pattern(pat, &v.1, ctx)?;
                }
                None => ctx.insert(v.1.clone()),
            },
            None => unreachable!(),
        }
    }
    Ok(())
}

fn eval_pattern_variant(
    var: &Pattern,
    body: &Body,
    ctx: &mut Ctx<Value>,
) -> std::result::Result<(), ()> {
    match var.as_ref() {
        ast::Pattern::Unit => {}
        ast::Pattern::Tup(els1) => match body.as_ref() {
            ast::Body::Tup(els2) => eval_pattern_tup(els1, els2, ctx)?,
            _ => return Err(()),
        },
        ast::Pattern::Rec(fields1) => match body.as_ref() {
            ast::Body::Rec(fields2) => eval_pattern_rec(fields1, fields2, ctx)?,
            _ => return Err(()),
        },
        _ => unreachable!(),
    };
    Ok(())
}

fn eval_pattern(pat: &Pattern, val: &Value, ctx: &mut Ctx<Value>) -> std::result::Result<(), ()> {
    match pat.as_ref() {
        ast::Pattern::Var(_) => ctx.insert(val.clone()),
        ast::Pattern::Wildcard => return Ok(()),
        ast::Pattern::Const(c) => match c.as_ref() {
            ast::Term::Unit if matches!(val.as_ref(), ast::Value::Unit) => {}
            ast::Term::True if matches!(val.as_ref(), ast::Value::Bool(true)) => {}
            ast::Term::False if matches!(val.as_ref(), ast::Value::Bool(false)) => {}
            ast::Term::Int(i1) => match val.as_ref() {
                ast::Value::Int(i2) if i1 == i2 => {}
                _ => return Err(()),
            },
            ast::Term::Str(s1) => match val.as_ref() {
                ast::Value::Str(s2) if s1 == s2 => {}
                _ => return Err(()),
            },
            _ => return Err(()),
        },
        ast::Pattern::Struct(_, p1, var) => match val.as_ref() {
            ast::Value::Struct(p2, body) if p1 == p2 => eval_pattern_variant(var, body, ctx)?,
            _ => return Err(()),
        },
        ast::Pattern::Variant(_, p1, vn1, var) => match val.as_ref() {
            ast::Value::Enum(p2, vn2, body) if p1 == p2 && vn1 == vn2 => {
                eval_pattern_variant(var, body, ctx)?
            }
            _ => return Err(()),
        },
        ast::Pattern::Or(pats) => {
            let mut _ctx = ctx.clone();
            for pat in pats.as_ref() {
                if eval_pattern(pat, val, &mut _ctx).is_ok() {
                    eval_pattern(pat, val, ctx)?;
                    return Ok(());
                }
            }
            return Err(());
        }
        ast::Pattern::Tup(els1) => match val.as_ref() {
            ast::Value::Tup(els2) if els1.iter().count() == els2.iter().count() => {
                eval_pattern_tup(els1, els2, ctx)?
            }
            _ => return Err(()),
        },
        ast::Pattern::Rec(fields1) => match val.as_ref() {
            ast::Value::Rec(fields2) if fields1.iter().count() == fields2.iter().count() => {
                eval_pattern_rec(fields1, fields2, ctx)?
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

impl ValueEq for ast::Body<Debruijn> {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (ast::Body::Unit, ast::Body::Unit) => true,
            (ast::Body::Tup(els1), ast::Body::Tup(els2)) => {
                els1.iter().zip(els2.iter()).all(|(x1, x2)| x1.eq(x2))
            }
            (ast::Body::Rec(fields1), ast::Body::Rec(fields2)) => fields1
                .iter()
                .zip(fields2.iter())
                .all(|((n1, x1), (n2, x2))| n1 == n2 && x1.eq(x2)),
            _ => false,
        }
    }
}

impl ValueEq for ast::Value<Debruijn> {
    fn eq(&self, rhs: &Self) -> bool {
        return match (self, rhs) {
            (ast::Value::Unit, ast::Value::Unit) => true,
            (ast::Value::Bool(b1), ast::Value::Bool(b2)) => b1 == b2,
            (ast::Value::Int(i1), ast::Value::Int(i2)) => i1 == i2,
            (ast::Value::Tup(els1), ast::Value::Tup(els2)) => {
                els1.iter().zip(els2.iter()).all(|(x1, x2)| x1.eq(x2))
            }
            (ast::Value::Rec(fields1), ast::Value::Rec(fields2)) => fields1
                .iter()
                .zip(fields2.iter())
                .all(|((n1, x1), (n2, x2))| n1 == n2 && x1.eq(x2)),
            (ast::Value::Struct(n1, v1), ast::Value::Struct(n2, v2)) => n1 == n2 && v1.eq(v2),
            (ast::Value::Enum(n1, vn1, v1), ast::Value::Enum(n2, vn2, v2)) => {
                n1 == n2 && vn1 == vn2 && v1.eq(v2)
            }
            _ => false,
        };
    }
}
