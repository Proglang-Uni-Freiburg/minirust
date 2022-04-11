use crate::err::Result;
use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

pub trait Item = Clone + Debug;

#[derive(Clone)]
pub struct Tag<T: Item, I: Item> {
    pub tag: T,
    pub it: Box<I>,
}

impl<T: Item, I: Item> Tag<T, I> {
    pub fn new(tag: T, it: I) -> Self {
        Tag {
            tag,
            it: Box::new(it),
        }
    }

    pub fn into_tag(self) -> T {
        self.tag.clone()
    }
    pub fn into_it(self) -> I {
        *self.it
    }
    pub fn tag(&self) -> &T {
        &self.tag
    }
    pub fn it(&self) -> &I {
        &*self.it
    }

    pub fn set<J: Item>(&self, j: J) -> Tag<T, J> {
        Tag::new(self.tag.clone(), j)
    }
}

// helper macro

#[macro_export]
macro_rules! map {
    ( $fun:ident ( $mapped:expr ) ) => {
        $mapped.map_tag(&mut |x| $fun(x))?
    };
    ( $fun:ident ( $mapped:expr , $($args:expr),* ) ) => {
        $mapped.map_tag(&mut |x| $fun(x, $($args),*))?
    };
}

// some useful trait impls

impl<T: Item, I: Item> Debug for Tag<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.it())
    }
}

impl<T: Item, I: Item + Display> Display for Tag<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.it())
    }
}

impl<T: Item, I: Item> Deref for Tag<T, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        self.it()
    }
}

impl<T: Item> Into<String> for Tag<T, String> {
    fn into(self) -> String {
        self.into_it()
    }
}

impl<T: Item> Into<usize> for Tag<T, usize> {
    fn into(self) -> usize {
        self.into_it()
    }
}

impl<T: Item, I: Item + PartialEq> PartialEq for Tag<T, I> {
    fn eq(&self, other: &Self) -> bool {
        self.it() == other.it()
    }
}

// some generic mapping of tags

pub trait MapTag<T, V, O> {
    fn map_tag<F: FnMut(&T) -> V>(&self, f: &mut F) -> O;
}

impl<T: Item, I: Item, V: Item, J: Item> MapTag<Tag<T, I>, Result<Tag<V, J>>, Result<Tag<V, J>>>
    for Tag<T, I>
{
    fn map_tag<F: FnMut(&Tag<T, I>) -> Result<Tag<V, J>>>(&self, f: &mut F) -> Result<Tag<V, J>> {
        f(self)
    }
}

impl<T: Item, J: Item, V: Item, K: Item, L: Item>
    MapTag<Tag<V, J>, Result<Tag<K, L>>, Result<Tag<T, Tag<K, L>>>> for Tag<T, Tag<V, J>>
{
    fn map_tag<F: FnMut(&Tag<V, J>) -> Result<Tag<K, L>>>(
        &self,
        f: &mut F,
    ) -> Result<Tag<T, Tag<K, L>>> {
        Ok(Tag::new(self.tag.clone(), self.it().map_tag(f)?))
    }
}

impl<T: Item, J: Item, V: Item, K: Item, L: Item>
    MapTag<Tag<V, J>, Result<Tag<K, L>>, Result<Tag<T, Vec<Tag<K, L>>>>>
    for Tag<T, Vec<Tag<V, J>>>
{
    fn map_tag<F: FnMut(&Tag<V, J>) -> Result<Tag<K, L>>>(
        &self,
        f: &mut F,
    ) -> Result<Tag<T, Vec<Tag<K, L>>>> {
        Ok(Tag::new(self.tag.clone(), self.it().map_tag(f)?))
    }
}

impl<T: Item, J: Item, V: Item, K: Item, L: Item, A: Item>
    MapTag<Tag<V, J>, Result<Tag<K, L>>, Result<Tag<T, Vec<(A, Tag<K, L>)>>>>
    for Tag<T, Vec<(A, Tag<V, J>)>>
{
    fn map_tag<F: FnMut(&Tag<V, J>) -> Result<Tag<K, L>>>(
        &self,
        f: &mut F,
    ) -> Result<Tag<T, Vec<(A, Tag<K, L>)>>> {
        Ok(Tag::new(self.tag.clone(), self.it().map_tag(f)?))
    }
}

impl<T: Item, J: Item, V: Item, K: Item, L: Item, A: Item>
    MapTag<Tag<V, J>, Result<Tag<K, L>>, Result<Tag<T, Vec<(A, Option<Tag<K, L>>)>>>>
    for Tag<T, Vec<(A, Option<Tag<V, J>>)>>
{
    fn map_tag<F: FnMut(&Tag<V, J>) -> Result<Tag<K, L>>>(
        &self,
        f: &mut F,
    ) -> Result<Tag<T, Vec<(A, Option<Tag<K, L>>)>>> {
        Ok(Tag::new(self.tag.clone(), self.it().map_tag(f)?))
    }
}

impl<T: Item, I: Item, V: Item, J: Item>
    MapTag<Tag<T, I>, Result<Tag<V, J>>, Result<Option<Tag<V, J>>>> for Option<Tag<T, I>>
{
    fn map_tag<F: FnMut(&Tag<T, I>) -> Result<Tag<V, J>>>(
        &self,
        f: &mut F,
    ) -> Result<Option<Tag<V, J>>> {
        match self {
            Some(x) => Ok(Some(x.map_tag(f)?)),
            None => Ok(None),
        }
    }
}

impl<T: Item, I: Item, V: Item, J: Item>
    MapTag<Tag<T, I>, Result<Tag<V, J>>, Result<Vec<Tag<V, J>>>> for Vec<Tag<T, I>>
{
    fn map_tag<F: FnMut(&Tag<T, I>) -> Result<Tag<V, J>>>(
        &self,
        f: &mut F,
    ) -> Result<Vec<Tag<V, J>>> {
        self.iter()
            .map(|x| x.map_tag(f))
            .collect::<Result<Vec<Tag<V, J>>>>()
    }
}

impl<T: Item, I: Item, V: Item, J: Item, A: Clone>
    MapTag<Tag<T, I>, Result<Tag<V, J>>, Result<Vec<(A, Tag<V, J>)>>> for Vec<(A, Tag<T, I>)>
{
    fn map_tag<F: FnMut(&Tag<T, I>) -> Result<Tag<V, J>>>(
        &self,
        f: &mut F,
    ) -> Result<Vec<(A, Tag<V, J>)>> {
        self.iter()
            .map(|(a, x)| Ok((a.clone(), x.map_tag(f)?)))
            .collect::<Result<Vec<(A, Tag<V, J>)>>>()
    }
}

impl<T: Item, I: Item, V: Item, J: Item, A: Clone>
    MapTag<Tag<T, I>, Result<Tag<V, J>>, Result<Vec<(A, Option<Tag<V, J>>)>>>
    for Vec<(A, Option<Tag<T, I>>)>
{
    fn map_tag<F: FnMut(&Tag<T, I>) -> Result<Tag<V, J>>>(
        &self,
        f: &mut F,
    ) -> Result<Vec<(A, Option<Tag<V, J>>)>> {
        self.iter()
            .map(|(a, x)| Ok((a.clone(), x.map_tag(f)?)))
            .collect::<Result<Vec<(A, Option<Tag<V, J>>)>>>()
    }
}


// un tagging nested tags

pub trait Untag<T> {
    fn untag(&self) -> T;
}

impl<T: Item, V: Item, J: Item> Untag<J> for Tag<T, Tag<V, J>> {
    fn untag(&self) -> J {
        self.it().it().clone()
    }
}

impl<T: Item, I: Item> Untag<Vec<I>> for Vec<Tag<T, I>> {
    fn untag(&self) -> Vec<I> {
        self.iter().map(|t| t.it().clone()).collect()
    }
}

impl<T: Item, V: Item, J: Item> Untag<Vec<J>> for Tag<T, Vec<Tag<V, J>>> {
    fn untag(&self) -> Vec<J> {
        self.it().iter().map(|t| t.it().clone()).collect()
    }
}

impl<T: Item, I: Item, V: Item, J: Item> Untag<Vec<(I, J)>> for Vec<(Tag<T, I>, Tag<V, J>)> {
    fn untag(&self) -> Vec<(I, J)> {
        self.iter()
            .map(|(l, r)| (l.it().clone(), r.it().clone()))
            .collect()
    }
}

// tuple vec helpers

pub trait Split<L, R> {
    fn lefts(&self) -> L;
    fn rights(&self) -> R;
}

impl<L: Item, R: Item> Split<Vec<L>, Vec<R>> for Vec<(L, R)> {
    fn lefts(&self) -> Vec<L> {
        self.iter().map(|(t, _)| t.clone()).collect()
    }
    fn rights(&self) -> Vec<R> {
        self.iter().map(|(_, v)| v.clone()).collect()
    }
}

impl<T: Item, L: Item, R: Item> Split<Tag<T, Vec<L>>, Tag<T, Vec<R>>> for Tag<T, Vec<(L, R)>> {
    fn lefts(&self) -> Tag<T, Vec<L>> {
        self.set(self.it().iter().map(|(t, _)| t.clone()).collect())
    }
    fn rights(&self) -> Tag<T, Vec<R>> {
        self.set(self.it().iter().map(|(_, v)| v.clone()).collect())
    }
}
