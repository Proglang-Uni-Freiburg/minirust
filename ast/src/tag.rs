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
    pub fn into(self) -> I {
        *self.it
    }
    pub fn as_ref(&self) -> &I {
        &*self.it
    }
    pub fn to<J: Item>(&self, j: J) -> Tag<T, J> {
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
        write!(f, "{:#?}", self.as_ref())
    }
}

impl<T: Item, I: Item + Display> Display for Tag<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl<T: Item, I: Item + IntoIterator> IntoIterator for Tag<T, I> {
    type Item = I::Item;

    type IntoIter = I::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.it.into_iter()
    }
}

pub struct Iter<'a, I>(std::slice::Iter<'a, I>);
impl<'a, I> Iterator for Iter<'a, I> {
    type Item = &'a I;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a, T: Item, I: Item> IntoIterator for &'a Tag<T, std::vec::Vec<I>> {
    type Item = &'a I;

    type IntoIter = Iter<'a, I>;

    fn into_iter(self) -> Self::IntoIter {
        Iter(self.as_ref().iter())
    }
}

impl<T: Item, I: Item> Deref for Tag<T, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &*self.it
    }
}

impl<T: Item, I: Item + Into<String>> Into<String> for Tag<T, I> {
    fn into(self) -> String {
        self.into().into()
    }
}

impl<T: Item, I: Item + Into<usize>> Into<usize> for Tag<T, I> {
    fn into(self) -> usize {
        self.into().into()
    }
}

impl<'a, T: Item> Into<usize> for &'a Tag<T, usize> {
    fn into(self) -> usize {
        *self.it.clone()
    }
}

impl<'a, T: Item> Into<usize> for &'a Tag<T, i64> {
    fn into(self) -> usize {
        usize::try_from(&*self).unwrap()
    }
}

impl<T: Item, I: Item + PartialEq> PartialEq for Tag<T, I> {
    fn eq(&self, other: &Self) -> bool {
        self.it == other.it
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
        Ok(Tag::new(self.tag.clone(), self.as_ref().map_tag(f)?))
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
        Ok(Tag::new(self.tag.clone(), self.as_ref().map_tag(f)?))
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
        Ok(Tag::new(self.tag.clone(), self.as_ref().map_tag(f)?))
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
        Ok(Tag::new(self.tag.clone(), self.as_ref().map_tag(f)?))
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
        self.as_ref().as_ref().clone()
    }
}

impl<T: Item, I: Item> Untag<Vec<I>> for Vec<Tag<T, I>> {
    fn untag(&self) -> Vec<I> {
        self.iter().map(|t| t.as_ref().clone()).collect()
    }
}

impl<T: Item, V: Item, J: Item> Untag<Vec<J>> for Tag<T, Vec<Tag<V, J>>> {
    fn untag(&self) -> Vec<J> {
        self.as_ref().iter().map(|t| t.as_ref().clone()).collect()
    }
}

impl<T: Item, I: Item, V: Item, J: Item> Untag<Vec<(I, J)>> for Vec<(Tag<T, I>, Tag<V, J>)> {
    fn untag(&self) -> Vec<(I, J)> {
        self.iter()
            .map(|(l, r)| (l.as_ref().clone(), r.as_ref().clone()))
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
        self.to(self.as_ref().iter().map(|(t, _)| t.clone()).collect())
    }
    fn rights(&self) -> Tag<T, Vec<R>> {
        self.to(self.as_ref().iter().map(|(_, v)| v.clone()).collect())
    }
}
