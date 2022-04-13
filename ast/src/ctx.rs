#[derive(Clone)]
pub struct Ctx<T>(Vec<T>);

impl<T: Clone> Ctx<T> {
    pub fn insert<I: Into<T>>(&mut self, t: I) {
        self.0.insert(0, t.into())
    }

    pub fn mutate<I: Into<T>>(&self, t: I) -> Self {
        let mut clone = self.0.clone();
        clone.insert(0, t.into());
        Ctx(clone)
    }

    pub fn mutates<I: Into<Vec<T>>>(&self, ts: I) -> Self {
        let mut clone = self.0.clone();
        for t in ts.into() {
            clone.insert(0, t);
        }
        Ctx(clone)
    }
    pub fn lookup<I: Into<usize>>(&self, u: I) -> Option<T> {
        Some(self.0.get(u.into())?.clone())
    }
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }
    pub fn from(iter: impl Iterator<Item = T>) -> Self {
        Ctx(iter.collect())
    }
    pub fn update(&mut self, idx: usize, t: T) {
        self.0[idx] = t; 
    }
}

// outsourced because of partial eq trait bound (not necessary for all ctxs used)
pub fn resolve<V: Clone + std::cmp::PartialEq, T: Into<V>>(ctx: &Ctx<V>, t: T) -> Option<usize> {
    let t_prime = t.into();
    ctx.0.iter().position(|v| v == &t_prime)
}

impl<T: Clone> Default for Ctx<T> {
    fn default() -> Self {
        Ctx(vec![])
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Ctx<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.0)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Ctx<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|it| format!("{}", it))
                .collect::<Vec<String>>()
                .join(",")
        )
    }
}
