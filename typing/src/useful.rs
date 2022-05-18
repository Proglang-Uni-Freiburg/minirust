use ast::err::{Error, GetCodeRef, Result};
use ast::tag::{Item, Tag};
use std::cell::Cell;
use std::iter::once;

use crate::{resolve, Env, Pattern, Type, Variant};

// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html

#[derive(Debug, Clone)]
pub enum PatternConstructor {
    Single,
    Variant(String),
    IntRange(i128, i128),
    Wildcard,
    NonExhaustive,
    Or,
    Missing(Vec<PatternConstructor>),
}

#[derive(Debug, Clone)]
pub struct _DeconstructedPattern {
    pub constr: PatternConstructor,
    pub fields: Vec<DeconstructedPattern>,
    pub ty: Type,
    pub reachable: Cell<bool>,
}
type DeconstructedPattern = Tag<<ast::Debruijn as ast::Repr>::Ann, _DeconstructedPattern>;

fn deconstruct_variant(pat: &Pattern, ty: &Variant, env: &Env) -> Vec<DeconstructedPattern> {
    match pat.as_ref() {
        ast::Pattern::Unit => vec![],
        ast::Pattern::Tup(pats) => match ty.as_ref() {
            ast::Variant::Tup(els) => pats
                .iter()
                .enumerate()
                .map(|(i, pat)| deconstruct(pat, &resolve(&els.it[i], env).unwrap(), env))
                .collect(),
            _ => unreachable!(),
        },
        ast::Pattern::Rec(pats) => match ty.as_ref() {
            ast::Variant::Rec(fields) => pats
                .iter()
                .map(|(x, pat)| {
                    let f_ty = resolve(
                        fields.iter().find(|(y, _)| x == y).map(|(_, t)| t).unwrap(),
                        env,
                    )
                    .unwrap();
                    match pat {
                        Some(pat) => deconstruct(pat, &f_ty, env),
                        None => deconstruct(&x.to(ast::Pattern::Wildcard), &f_ty, env),
                    }
                })
                .collect(),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}
fn deconstruct(p: &Pattern, ty: &Type, env: &Env) -> DeconstructedPattern {
    let ty = resolve(ty, env).unwrap();
    let (constr, fields) = match p.as_ref() {
        ast::Pattern::Or(_) => {
            fn expand(pat: &Pattern, vec: &mut Vec<Pattern>) {
                match pat.as_ref() {
                    ast::Pattern::Or(pats) => {
                        for pat in pats.as_ref() {
                            expand(pat, vec);
                        }
                    }
                    _ => vec.push(pat.clone()),
                }
            }
            let mut pats = vec![];
            expand(p, &mut pats);
            (
                PatternConstructor::Or,
                pats.iter().map(|x| deconstruct(x, &ty, env)).collect(),
            )
        }
        ast::Pattern::Wildcard | ast::Pattern::Var(_) => (PatternConstructor::Wildcard, vec![]),
        ast::Pattern::Const(c) => match ty.as_ref() {
            ast::Type::Unit => (PatternConstructor::Single, vec![]),
            ast::Type::Bool => match c.as_ref() {
                ast::Term::False => (PatternConstructor::IntRange(0, 0), vec![]),
                ast::Term::True => (PatternConstructor::IntRange(1, 1), vec![]),
                _ => unreachable!(),
            },
            ast::Type::Int => match c.as_ref() {
                ast::Term::Int(i) => (
                    PatternConstructor::IntRange((*i).into(), (*i).into()),
                    vec![],
                ),
                _ => unreachable!(),
            },
            ast::Type::Str => (PatternConstructor::NonExhaustive, vec![]),
            _ => unreachable!(),
        },
        ast::Pattern::Struct(_, _, pat) => match ty.as_ref() {
            ast::Type::Struct(_, var) => (
                PatternConstructor::Single,
                deconstruct_variant(pat, var, env),
            ),
            _ => unreachable!(),
        },
        ast::Pattern::Variant(_, _, var_id, pat) => match ty.as_ref() {
            ast::Type::Enum(_, vars) => {
                let var = vars
                    .iter()
                    .find(|(x, _)| x == var_id)
                    .map(|(_, y)| y)
                    .unwrap();
                (
                    PatternConstructor::Variant(var_id.clone().into()),
                    deconstruct_variant(pat, var, env),
                )
            }
            _ => unreachable!(),
        },
        ast::Pattern::Unit => (PatternConstructor::Single, vec![]),
        ast::Pattern::Tup(pats) => match ty.as_ref() {
            ast::Type::Tup(els) => (
                PatternConstructor::Single,
                pats.iter()
                    .enumerate()
                    .map(|(i, pat)| deconstruct(pat, &els.it[i].clone(), env))
                    .collect(),
            ),
            _ => unreachable!(),
        },
        ast::Pattern::Rec(pats) => match ty.as_ref() {
            ast::Type::Rec(fields) => (
                PatternConstructor::Single,
                pats.iter()
                    .map(|(x, pat)| {
                        let f_ty = fields.iter().find(|(y, _)| x == y).map(|(_, t)| t).unwrap();
                        match pat {
                            Some(pat) => deconstruct(pat, f_ty, env),
                            None => deconstruct(&x.to(ast::Pattern::Wildcard), f_ty, env),
                        }
                    })
                    .collect(),
            ),
            _ => unreachable!(),
        },
    };
    p.to(_DeconstructedPattern {
        constr,
        fields,
        ty,
        reachable: Cell::new(false),
    })
}

impl PatternConstructor {
    fn is_covered_by(&self, other: &Self) -> bool {
        match (self, other) {
            (_, PatternConstructor::Wildcard) => true,
            (PatternConstructor::Missing(_) | PatternConstructor::Wildcard, _) => false,
            (PatternConstructor::Single, PatternConstructor::Single) => true,
            (PatternConstructor::Variant(v1), PatternConstructor::Variant(v2)) => v1 == v2,
            (PatternConstructor::IntRange(lo, hi), PatternConstructor::IntRange(olo, ohi)) => {
                lo >= olo && hi <= ohi
            }
            (PatternConstructor::NonExhaustive, _) => false,
            _ => unimplemented!(
                "matched pattern constructor {:#?} with invalid {:#?}",
                self,
                other
            ),
        }
    }
    fn all_constructors(&self, ty: &Type, env: &Env) -> Vec<PatternConstructor> {
        let ty = resolve(ty, env).unwrap();
        match ty.as_ref() {
            ast::Type::Unit => vec![PatternConstructor::Single],
            ast::Type::Bool => vec![PatternConstructor::IntRange(0, 1)],
            ast::Type::Int => vec![PatternConstructor::IntRange(
                (-i64::MAX).into(),
                i64::MAX.into(),
            )],
            ast::Type::Str => vec![PatternConstructor::NonExhaustive],
            ast::Type::Tup(_) => vec![PatternConstructor::Single],
            ast::Type::Rec(_) => vec![PatternConstructor::Single],
            ast::Type::Struct(_, _) => vec![PatternConstructor::Single],
            ast::Type::Enum(_, vars) => vars
                .as_ref()
                .iter()
                .map(|(x, _)| PatternConstructor::Variant(x.as_ref().clone()))
                .collect(),
            _ => vec![PatternConstructor::NonExhaustive],
        }
    }
    fn split(
        &self,
        others: &[PatternConstructor],
        ty: &Type,
        env: &Env,
    ) -> Vec<PatternConstructor> {
        match self {
            PatternConstructor::IntRange(lo, hi) if lo != hi => {
                let included = others.iter().filter_map(|x| match x {
                    PatternConstructor::IntRange(olo, ohi) => {
                        if lo <= ohi && olo <= hi {
                            Some(PatternConstructor::IntRange(
                                std::cmp::max(*lo, *olo),
                                std::cmp::min(*hi, *hi),
                            ))
                        } else {
                            None
                        }
                    }
                    _ => None,
                });
                let mut borders = included
                    .flat_map(|range| match range {
                        PatternConstructor::IntRange(lo, hi) => once(lo).chain(once(hi + 1)),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<i128>>();
                borders.sort_unstable();
                let mut prev = lo;
                let ranges = borders
                    .iter()
                    .chain(once(&(hi + 1)))
                    .map(move |border| {
                        let r = (prev, border);
                        prev = border;
                        r
                    })
                    .filter(|(prev, current)| prev != current)
                    .map(move |(prev, current)| match (prev, current) {
                        (lo, hi) if lo < hi => PatternConstructor::IntRange(*lo, *hi - 1),
                        _ => unreachable!(),
                    })
                    .collect();
                ranges
            }
            PatternConstructor::Wildcard => {
                let all = self.all_constructors(ty, env);
                let all = all
                    .iter()
                    .map(|c| c.split(others, ty, env))
                    .collect::<Vec<Vec<PatternConstructor>>>()
                    .into_iter()
                    .flatten()
                    .collect::<Vec<PatternConstructor>>();

                let m_constructors: Vec<PatternConstructor> = others
                    .iter()
                    .filter(|c| !matches!(c, PatternConstructor::Wildcard))
                    .cloned()
                    .collect();

                let missing = all
                    .iter()
                    .filter(|x| !m_constructors.iter().any(|o| x.is_covered_by(o)))
                    .collect::<Vec<&PatternConstructor>>();

                if !missing.is_empty() {
                    if !m_constructors.is_empty() {
                        vec![PatternConstructor::Missing(
                            missing.into_iter().cloned().collect(),
                        )]
                    } else {
                        vec![PatternConstructor::Wildcard]
                    }
                } else {
                    all
                }
            }
            _ => vec![self.clone()],
        }
    }
}

impl _DeconstructedPattern {
    fn wildcard(ty: &Type) -> Self {
        _DeconstructedPattern {
            constr: PatternConstructor::Wildcard,
            fields: vec![],
            ty: ty.clone(),
            reachable: Cell::new(false),
        }
    }
}

fn specialize_variant_wildcard(var: &Variant) -> Vec<DeconstructedPattern> {
    match var.as_ref() {
        ast::Variant::Unit => vec![],
        ast::Variant::Tup(els) => els
            .iter()
            .map(|x| x.to(_DeconstructedPattern::wildcard(x)))
            .collect(),
        ast::Variant::Rec(fields) => fields
            .iter()
            .map(|(_, x)| x.to(_DeconstructedPattern::wildcard(x)))
            .collect(),
    }
}

pub fn specialize(
    pat: &DeconstructedPattern,
    other: &PatternConstructor,
    env: &Env,
) -> Vec<DeconstructedPattern> {
    let pat_constr = pat.as_ref();
    let ty = resolve(&pat_constr.ty, env).unwrap();
    match (&pat_constr.constr, other) {
        (PatternConstructor::Wildcard, _) => match other {
            PatternConstructor::Single => match ty.as_ref() {
                ast::Type::Tup(els) => els
                    .iter()
                    .map(|x| x.to(_DeconstructedPattern::wildcard(x)))
                    .collect(),
                ast::Type::Rec(fields) => fields
                    .iter()
                    .map(|(_, x)| x.to(_DeconstructedPattern::wildcard(x)))
                    .collect(),
                ast::Type::Struct(_, var) => specialize_variant_wildcard(var),
                ast::Type::Unit => vec![],
                _ => unreachable!(),
            },
            PatternConstructor::Variant(var) => match ty.as_ref() {
                ast::Type::Enum(_, vars) => specialize_variant_wildcard(
                    vars.iter()
                        .find(|(x, _)| x.as_ref() == var)
                        .map(|(_, y)| y)
                        .unwrap(),
                ),
                _ => unreachable!(),
            },
            _ => vec![],
        },
        _ => pat_constr.fields.clone(),
    }
}

type Matrix<T> = Vec<Vec<T>>;
fn is_useful(matrix: &Matrix<DeconstructedPattern>, v: &[DeconstructedPattern], env: &Env) -> bool {
    let row_lens = matrix.iter().map(|row| row.len()).collect::<Vec<usize>>();
    assert!(row_lens.iter().min() == row_lens.iter().max());

    // base case
    if v.is_empty() {
        return matrix.is_empty();
    }

    let _DeconstructedPattern {
        constr,
        ty,
        reachable: reachable_cell,
        ..
    } = v[0].as_ref();

    let mut reachable = false;

    // expands an or pattern
    fn expand(row: &[DeconstructedPattern]) -> Matrix<DeconstructedPattern> {
        row[0]
            .as_ref()
            .fields
            .iter()
            .map(|p| {
                let mut new_row = vec![p.clone()];
                let mut row_without_head = row.iter();
                row_without_head.next();
                new_row.extend(row_without_head.cloned());
                new_row
            })
            .collect::<Matrix<DeconstructedPattern>>()
    }

    // expand or patterns in matrix
    let matrix = matrix
        .iter()
        .flat_map(|row| match row[0].as_ref().constr {
            PatternConstructor::Or => expand(row),
            _ => vec![row.clone()],
        })
        .collect::<Matrix<DeconstructedPattern>>();

    // specializes head of row
    fn specialize_row(
        c: &PatternConstructor,
        row: &[DeconstructedPattern],
        env: &Env,
    ) -> Vec<DeconstructedPattern> {
        let mut new_row = specialize(&row[0], c, env);
        let mut row_without_head = row.iter();
        row_without_head.next();
        new_row.extend(row_without_head.cloned());
        new_row
    }

    // specialize all heads of all rows
    fn specialize_matrix(
        c: &PatternConstructor,
        matrix: &Matrix<DeconstructedPattern>,
        env: &Env,
    ) -> Matrix<DeconstructedPattern> {
        let mut mat = vec![];
        for row in matrix {
            if c.is_covered_by(&row[0].as_ref().constr) {
                mat.push(specialize_row(c, row, env))
            }
        }
        mat
    }

    if matches!(constr, PatternConstructor::Or) {
        // expand or pattern
        let mut all_reachable = true;
        let mut matrix = matrix;
        for v in expand(v) {
            all_reachable &= is_useful(&matrix, &v, env);
            matrix.push(v)
        }
        reachable |= all_reachable
    } else {
        let heads = &matrix
            .iter()
            .map(|row| row[0].as_ref().constr.clone())
            .collect::<Vec<PatternConstructor>>();
        let splitted = constr.split(heads, ty, env);
        for c in splitted {
            let spec = specialize_matrix(&c, &matrix, env);
            let v = specialize_row(&c, v, env);
            reachable |= is_useful(&spec, &v, env);
        }
    }

    if reachable {
        reachable_cell.set(true)
    }
    reachable
}

pub fn is_exhaustive<T: GetCodeRef>(
    pats: &crate::Vec<Pattern>,
    ty: &Type,
    m: &T,
    env: &Env,
) -> Result<()> {
    let mut matrix = vec![];
    let ty = resolve(ty, env).unwrap();

    let deconstructed_patterns = pats.iter().map(|x| deconstruct(x, &ty, env));
    let pat_reachable: Vec<(DeconstructedPattern, bool)> = deconstructed_patterns
        .map(|x| {
            let v = vec![x];
            is_useful(&matrix, &v, env);
            let (reachable, pat) = (v[0].as_ref().reachable.get(), v[0].clone());
            matrix.push(v);
            (pat, reachable)
        })
        .collect();

    let wildcard = vec![Tag::new(
        (vec![], (0, 0)),
        _DeconstructedPattern::wildcard(&ty),
    )];

    is_useful(&matrix, &wildcard, env);

    if wildcard[0].as_ref().reachable.get() {
        return Err(Error::new("non-exhaustive match")
            .label(m, format!("does not cover all constructors of {}", ty)));
    }

    for (pat, reachable) in pat_reachable {
        if !reachable {
            match pat.as_ref().constr {
                PatternConstructor::Or => {
                    return Err(
                        Error::new("unreachable branch").label(&pat, "has unreachable branch")
                    )
                }
                _ => return Err(Error::new("unreachable pattern").label(&pat, "is unreachable")),
            }
        }
    }

    Ok(())
}
