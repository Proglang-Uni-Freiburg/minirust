use ast::err::{Error, Result};
use ast::tag::{Item, Tag};

use ast::{Debruijn, Pattern, Top, Variant, _Ident, _Pattern, _Program, _Top, _Variant, _Vec};

pub trait NoDups {
    fn no_dups(&self) -> Result<()>;
}

impl<T: Item> NoDups for Tag<T, Vec<_Ident<Debruijn>>> {
    fn no_dups(&self) -> Result<()> {
        for a in self.it() {
            if self.it().iter().filter(|b| &a == b).count() > 1 {
                return Err(Error::new("duplicated struct field").label(a, "duplicated"));
            }
        }
        Ok(())
    }
}

impl<T: Item, I: Item> NoDups for Tag<T, Vec<(_Ident<Debruijn>, I)>> {
    fn no_dups(&self) -> Result<()> {
        for (a, _) in self.it() {
            if self.it().iter().filter(|(b, _)| a == b).count() > 1 {
                return Err(Error::new("duplicated struct field").label(a, "is duplicated"));
            }
        }
        Ok(())
    }
}

impl NoDups for _Variant<Debruijn> {
    fn no_dups(&self) -> Result<()> {
        match self.it() {
            Variant::Unit => Ok(()),
            Variant::Tup(_) => Ok(()),
            Variant::Rec(rec) => rec.no_dups(),
        }
    }
}

fn _pat_dups(p: &_Pattern<Debruijn>) -> Result<Vec<String>> {
    Ok(match p.it() {
        Pattern::Var(v) => {
            vec![v.clone().into()]
        }
        Pattern::Or(pats) => {
            let mut dups = pats
                .it()
                .iter()
                .map(|p| Ok(_pat_dups(p)?))
                .collect::<Result<Vec<Vec<String>>>>()?
                .into_iter();
            let first = dups.next().unwrap();
            for other in dups {
                if first != other {
                    return Err(Error::new("or pattern variables mismatch")
                        .label(p, "does not have same binders in each branch"));
                }
            }
            first.clone()
        }
        Pattern::Unit | Pattern::Const(_) | Pattern::Wildcard => vec![],
        Pattern::Tup(pats) => pats
            .it()
            .iter()
            .map(|pat| Ok(_pat_dups(pat)?))
            .collect::<Result<Vec<Vec<String>>>>()?
            .into_iter()
            .flatten()
            .collect(),
        Pattern::Rec(pats) => pats
            .it()
            .iter()
            .map(|(i, pat)| {
                Ok(match pat {
                    Some(pat) => _pat_dups(pat)?,
                    None => vec![i.clone().into()],
                })
            })
            .collect::<Result<Vec<Vec<String>>>>()?
            .into_iter()
            .flatten()
            .collect(),
        Pattern::Struct(_, _, pat) | Pattern::Variant(_, _, _, pat) => _pat_dups(pat)?,
    })
}

impl NoDups for _Pattern<Debruijn> {
    fn no_dups(&self) -> Result<()> {
        let vars = _pat_dups(self)?;
        for var in &vars {
            if vars.iter().filter(|x| x == &var).count() > 1 {
                return Err(Error::new("duplicated binder")
                    .label(self, format!("has duplicated binder {}", var)));
            }
        }
        Ok(())
    }
}

impl NoDups for _Vec<Debruijn, _Pattern<Debruijn>> {
    fn no_dups(&self) -> Result<()> {
        let mut vars = vec![];
        for pat in self.it() {
            vars.extend(_pat_dups(pat)?);
        }
        for var in &vars {
            if vars.iter().filter(|x| x == &var).count() > 1 {
                return Err(Error::new("duplicated binder")
                    .label(self, format!("has duplicated binder {}", var)));
            }
        }
        Ok(())
    }
}

impl NoDups for _Program<Debruijn> {
    fn no_dups(&self) -> Result<()> {
        for a in self.it() {
            if self
                .it()
                .iter()
                .filter(|b| top_id(a) == top_id(b) && a.tag.0 == b.tag.0)
                .count()
                > 1
            {
                return Err(
                    Error::new("duplicated definition").label(a, "is already defined in this file")
                );
            }
        }
        Ok(())
    }
}

fn top_id(t: &_Top<Debruijn>) -> _Ident<Debruijn> {
    match &t.it() {
        Top::Fun(id, _, _, _) => id.clone(),
        Top::FFIFun(id, _, _, _) => id.clone(),
        Top::Alias(id, _) => id.clone(),
        Top::Struct(id, _) => id.clone(),
        Top::Enum(id, _) => id.clone(),
        Top::Use(_) => unimplemented!(),
    }
}