use ast::err::{CodeRef, Error, Result};
use ast::tag::{Item, Tag};

use ast::{Debruijn, Pattern, Repr, Variant, _Type};
use ast::{Term, Type};
use std::cell::Cell;
use std::iter::once;

use crate::{resolve, Env};

// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html

ast::def_from_to_ast_types! {
    from => Debruijn,
    to => Debruijn,
    prefix => ast
}

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
pub struct DeconstructedPattern {
    pub constr: PatternConstructor,
    pub fields: Vec<_DeconstructedPattern>,
    pub ty: _Type<Debruijn>,
    pub reachable: Cell<bool>,
}
type _DeconstructedPattern = Tag<<Debruijn as Repr>::Ann, DeconstructedPattern>;

fn deconstruct_variant(
    pat: &FromPattern,
    ty: &FromVariant,
    env: &Env,
) -> Vec<_DeconstructedPattern> {
    match pat.it() {
        Pattern::Unit => vec![],
        Pattern::Tup(pats) => match ty.it() {
            Variant::Tup(els) => pats
                .it()
                .iter()
                .enumerate()
                .map(|(i, pat)| deconstruct(pat, &resolve(&els.it[i], env).unwrap(), env))
                .collect(),
            _ => unreachable!(),
        },
        Pattern::Rec(pats) => match ty.it() {
            Variant::Rec(fields) => pats
                .it()
                .iter()
                .map(|(x, pat)| {
                    let f_ty = resolve(
                        fields.iter().find(|(y, _)| x == y).map(|(_, t)| t).unwrap(),
                        env,
                    )
                    .unwrap();
                    match pat {
                        Some(pat) => deconstruct(pat, &f_ty, env),
                        None => deconstruct(&x.set(Pattern::Wildcard), &f_ty, env),
                    }
                })
                .collect(),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}
fn deconstruct(p: &FromPattern, ty: &FromType, env: &Env) -> _DeconstructedPattern {
    let ty = resolve(ty, env).unwrap();
    let (constr, fields) = match p.it() {
        Pattern::Or(_) => {
            fn expand(pat: &FromPattern, vec: &mut Vec<FromPattern>) {
                match pat.it() {
                    Pattern::Or(pats) => {
                        for pat in pats.it() {
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
        Pattern::Wildcard | Pattern::Var(_) => (PatternConstructor::Wildcard, vec![]),
        Pattern::Const(c) => match ty.it() {
            Type::Unit => (PatternConstructor::Single, vec![]),
            Type::Bool => match c.it() {
                Term::False => (PatternConstructor::IntRange(0, 0), vec![]),
                Term::True => (PatternConstructor::IntRange(1, 1), vec![]),
                _ => unreachable!(),
            },
            Type::Int => match c.it() {
                Term::Int(i) => (
                    PatternConstructor::IntRange(i.clone().into(), i.clone().into()),
                    vec![],
                ),
                _ => unreachable!(),
            },
            Type::Str => (PatternConstructor::NonExhaustive, vec![]),
            _ => unreachable!(),
        },
        Pattern::Struct(_, _, pat) => match ty.it() {
            Type::Struct(_, var) => (
                PatternConstructor::Single,
                deconstruct_variant(pat, var, env),
            ),
            _ => unreachable!(),
        },
        Pattern::Variant(_, _, var_id, pat) => match ty.it() {
            Type::Enum(_, vars) => {
                let var = vars
                    .it()
                    .iter()
                    .find(|(x, _)| x == var_id)
                    .map(|(_, y)| y)
                    .unwrap();
                (
                    PatternConstructor::Variant(var_id.it().clone()),
                    deconstruct_variant(pat, var, env),
                )
            }
            _ => unreachable!(),
        },
        Pattern::Unit => (PatternConstructor::Single, vec![]),
        Pattern::Tup(pats) => match ty.it() {
            Type::Tup(els) => (
                PatternConstructor::Single,
                pats.it()
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| deconstruct(pat, &els.it[i].clone(), env))
                    .collect(),
            ),
            _ => unreachable!(),
        },
        Pattern::Rec(pats) => match ty.it() {
            Type::Rec(fields) => (
                PatternConstructor::Single,
                pats.it()
                    .iter()
                    .map(|(x, pat)| {
                        let f_ty = fields.iter().find(|(y, _)| x == y).map(|(_, t)| t).unwrap();
                        match pat {
                            Some(pat) => deconstruct(pat, &f_ty, env),
                            None => deconstruct(&x.set(Pattern::Wildcard), &f_ty, env),
                        }
                    })
                    .collect(),
            ),
            _ => unreachable!(),
        },
    };
    p.set(DeconstructedPattern {
        constr,
        fields,
        ty: ty.clone(),
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
    fn all_constructors(&self, ty: &FromType, env: &Env) -> Vec<PatternConstructor> {
        let ty = resolve(ty, env).unwrap();
        match ty.it() {
            Type::Unit => vec![PatternConstructor::Single],
            Type::Bool => vec![PatternConstructor::IntRange(0, 1)],
            Type::Int => vec![PatternConstructor::IntRange(
                (-i64::MAX).into(),
                i64::MAX.into(),
            )],
            Type::Str => vec![PatternConstructor::NonExhaustive],
            Type::Tup(_) => vec![PatternConstructor::Single],
            Type::Rec(_) => vec![PatternConstructor::Single],
            Type::Struct(_, _) => vec![PatternConstructor::Single],
            Type::Enum(_, vars) => vars
                .it()
                .iter()
                .map(|(x, _)| PatternConstructor::Variant(x.it().clone()))
                .collect(),
            _ => vec![PatternConstructor::NonExhaustive]
        }
    }
    fn split(
        &self,
        others: &Vec<PatternConstructor>,
        ty: &FromType,
        env: &Env,
    ) -> Vec<PatternConstructor> {
        match self {
            PatternConstructor::IntRange(lo, hi) if lo != hi => {
                let included = others
                    .iter()
                    .filter_map(|x| match x {
                        PatternConstructor::IntRange(olo, ohi) => {
                            if lo <= ohi && olo <= hi {
                                Some(PatternConstructor::IntRange(
                                    std::cmp::max(lo.clone(), olo.clone()),
                                    std::cmp::min(hi.clone(), hi.clone()),
                                ))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    })
                    .collect::<Vec<PatternConstructor>>();
                let mut borders = included
                    .into_iter()
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
                        (lo, hi) if lo < hi => {
                            PatternConstructor::IntRange(lo.clone(), hi.clone() - 1)
                        }
                        _ => unreachable!(),
                    })
                    .collect();
                ranges
            }
            PatternConstructor::Wildcard => {
                let all = self.all_constructors(ty, env);
                let all = all
                    .iter()
                    .map(|c| c.split(&others.clone(), ty, env))
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

impl DeconstructedPattern {
    fn wildcard(ty: &FromType) -> Self {
        DeconstructedPattern {
            constr: PatternConstructor::Wildcard,
            fields: vec![],
            ty: ty.clone(),
            reachable: Cell::new(false),
        }
    }
}

fn specialize_variant_wildcard(var: &FromVariant) -> Vec<_DeconstructedPattern> {
    match var.it() {
        Variant::Unit => vec![],
        Variant::Tup(els) => els
            .it()
            .iter()
            .map(|x| x.set(DeconstructedPattern::wildcard(x)))
            .collect(),
        Variant::Rec(fields) => fields
            .it()
            .iter()
            .map(|(_, x)| x.set(DeconstructedPattern::wildcard(x)))
            .collect(),
    }
}

pub fn specialize(
    pat: &_DeconstructedPattern,
    other: &PatternConstructor,
    env: &Env,
) -> Vec<_DeconstructedPattern> {
    let pat_constr = pat.it();
    let ty = resolve(&pat_constr.ty, env).unwrap();
    match (&pat_constr.constr, other) {
        (PatternConstructor::Wildcard, _) => match other {
            PatternConstructor::Single => match ty.it() {
                Type::Tup(els) => els
                    .it()
                    .iter()
                    .map(|x| x.set(DeconstructedPattern::wildcard(x)))
                    .collect(),
                Type::Rec(fields) => fields
                    .it()
                    .iter()
                    .map(|(_, x)| x.set(DeconstructedPattern::wildcard(x)))
                    .collect(),
                Type::Struct(_, var) => specialize_variant_wildcard(var),
                _ => unreachable!(),
            },
            PatternConstructor::Variant(var) => match ty.it() {
                Type::Enum(_, vars) => specialize_variant_wildcard(
                    &vars
                        .it()
                        .iter()
                        .find(|(x, _)| x.it() == var)
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

fn is_useful(
    matrix: &Vec<Vec<_DeconstructedPattern>>,
    v: &Vec<_DeconstructedPattern>,
    env: &Env,
) -> bool {
    let row_lens = matrix.iter().map(|row| row.len()).collect::<Vec<usize>>();
    assert!(row_lens.iter().min() == row_lens.iter().max());

    // base case
    if v.is_empty() {
        return matrix.is_empty();
    }

    let DeconstructedPattern {
        constr,
        ty,
        reachable: reachable_cell,
        ..
    } = v[0].it();

    let mut reachable = false;

    // expands an or pattern
    fn expand(row: &Vec<_DeconstructedPattern>) -> Vec<Vec<_DeconstructedPattern>> {
        row[0]
            .it()
            .fields
            .iter()
            .map(|p| {
                let mut new_row = vec![p.clone()];
                let mut row_without_head = row.clone().into_iter();
                row_without_head.next();
                new_row.extend(row_without_head);
                new_row
            })
            .collect::<Vec<Vec<_DeconstructedPattern>>>()
    }

    // expand or patterns in matrix
    let matrix = matrix
        .iter()
        .flat_map(|row| match row[0].it().constr {
            PatternConstructor::Or => expand(row),
            _ => vec![row.clone()],
        })
        .collect::<Vec<Vec<_DeconstructedPattern>>>();

    // specializes head of row
    fn specialize_row(
        c: &PatternConstructor,
        row: &Vec<_DeconstructedPattern>,
        env: &Env,
    ) -> Vec<_DeconstructedPattern> {
        let mut new_row = specialize(&row[0], c, env);
        let mut row_without_head = row.clone().into_iter();
        row_without_head.next();
        new_row.extend(row_without_head);
        new_row
    }

    // specialize all heads of all rows
    fn specialize_matrix(
        c: &PatternConstructor,
        matrix: &Vec<Vec<_DeconstructedPattern>>,
        env: &Env,
    ) -> Vec<Vec<_DeconstructedPattern>> {
        let mut mat = vec![];
        for row in matrix {
            if c.is_covered_by(&row[0].it().constr) {
                mat.push(specialize_row(c, row, env))
            }
        }
        mat
    }

    if matches!(constr, PatternConstructor::Or) {
        // expand or pattern
        let mut all_reachable = true;
        let mut matrix = matrix.clone();
        for v in expand(v) {
            all_reachable &= is_useful(&matrix, &v, env);
            matrix.push(v)
        }
        reachable |= all_reachable
    } else {
        let heads = &matrix
            .iter()
            .map(|row| row[0].it().constr.clone())
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

pub fn is_exhaustive<T: Item>(
    pats: &FromVec<FromPattern>,
    ty: &FromType,
    m: &Tag<CodeRef, T>,
    env: &Env,
) -> Result<()> {
    let mut matrix = vec![];
    let ty = resolve(ty, env).unwrap();

    let deconstructed_patterns: Vec<_DeconstructedPattern> =
        pats.it().iter().map(|x| deconstruct(x, &ty, env)).collect();

    let pat_reachable: Vec<(_DeconstructedPattern, bool)> = deconstructed_patterns
        .into_iter()
        .map(|x| {
            let v = vec![x];
            is_useful(&matrix, &v, env);
            let (reachable, pat) = (v[0].it().reachable.get(), v[0].clone());
            matrix.push(v);
            (pat, reachable)
        })
        .collect();

    let wildcard = vec![Tag::new(
        (vec![], (0, 0)),
        DeconstructedPattern::wildcard(&ty),
    )];

    is_useful(&matrix, &wildcard, env);

    if wildcard[0].it().reachable.get() {
        return Err(Error::new("non-exhaustive match")
            .label(m, format!("does not cover all constructors of {}", ty)));
    }

    for (pat, reachable) in pat_reachable {
        if !reachable {
            match pat.it().constr {
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
