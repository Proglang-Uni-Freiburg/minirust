use ast::err::{Error, Result};
use ast::tag::Item;

use crate::{Ident, Pattern, Program, Top, Variant, Vec};

pub trait NoDups {
    fn no_dups(&self) -> Result<()>;
}

impl NoDups for Vec<Ident> {
    fn no_dups(&self) -> Result<()> {
        for a in self.as_ref() {
            if self.iter().filter(|b| &a == b).count() > 1 {
                return Err(Error::new("duplicated struct field").label(a, "duplicated"));
            }
        }
        Ok(())
    }
}

impl<T: Item> NoDups for Vec<(Ident, T)> {
    fn no_dups(&self) -> Result<()> {
        for (a, _) in self.as_ref() {
            if self.iter().filter(|(b, _)| a == b).count() > 1 {
                return Err(Error::new("duplicated struct field").label(a, "is duplicated"));
            }
        }
        Ok(())
    }
}

impl NoDups for Variant {
    fn no_dups(&self) -> Result<()> {
        match self.as_ref() {
            ast::Variant::Unit => Ok(()),
            ast::Variant::Tup(_) => Ok(()),
            ast::Variant::Rec(rec) => rec.no_dups(),
        }
    }
}

fn _pat_dups(p: &Pattern) -> Result<std::vec::Vec<String>> {
    Ok(match p.as_ref() {
        ast::Pattern::Var(v) => {
            vec![v.clone().into()]
        }
        ast::Pattern::Or(pats) => {
            let mut dups = pats
                .iter()
                .map(_pat_dups)
                .collect::<Result<std::vec::Vec<std::vec::Vec<String>>>>()?
                .into_iter();
            let first = dups.next().unwrap();
            for other in dups {
                if first != other {
                    return Err(Error::new("or pattern variables mismatch")
                        .label(p, "does not have same binders in each branch"));
                }
            }
            first
        }
        ast::Pattern::Unit | ast::Pattern::Const(_) | ast::Pattern::Wildcard => vec![],
        ast::Pattern::Tup(pats) => pats
            .iter()
            .map(_pat_dups)
            .collect::<Result<std::vec::Vec<std::vec::Vec<String>>>>()?
            .into_iter()
            .flatten()
            .collect(),
        ast::Pattern::Rec(pats) => pats
            .as_ref()
            .iter()
            .map(|(i, pat)| {
                Ok(match pat {
                    Some(pat) => _pat_dups(pat)?,
                    None => vec![i.clone().into()],
                })
            })
            .collect::<Result<std::vec::Vec<std::vec::Vec<String>>>>()?
            .into_iter()
            .flatten()
            .collect(),
        ast::Pattern::Struct(_, _, pat) | ast::Pattern::Variant(_, _, _, pat) => _pat_dups(pat)?,
    })
}

impl NoDups for Pattern {
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

impl NoDups for Vec<Pattern> {
    fn no_dups(&self) -> Result<()> {
        let mut vars = vec![];
        for pat in self.as_ref() {
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

impl NoDups for Program {
    fn no_dups(&self) -> Result<()> {
        for a in self.as_ref() {
            if self
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

fn top_id(t: &Top) -> &Ident {
    match &t.as_ref() {
        ast::Top::Fun(id, _, _, _) => id,
        ast::Top::FFIFun(id, _, _, _) => id,
        ast::Top::Alias(id, _) => id,
        ast::Top::Struct(id, _) => id,
        ast::Top::Enum(id, _) => id,
        ast::Top::Use(_) => unimplemented!(),
    }
}
