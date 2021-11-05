use crate::store::Store;
use crate::thing::Thing;

// * The Query trait

/// A Query provides a function that determines if a thing should be
/// selected or not.
pub trait Query<'a> {
    /// Return a filter that can be used to filter an iterator over
    /// the Store's entities.
    fn matches(&self, store: &Store, thing: &'_ Thing) -> bool;
}

//  ** Booleans

//  *** Not

pub struct NotQuery<'a> {
    other: dyn Query<'a>,
}

impl<'a> Query<'a> for NotQuery<'a> {
    fn matches(&self, store: &Store, thing: &'_ Thing) -> bool {
        !self.other.matches(store, thing)
    }
}

//  *** And

pub struct AndQuery<'a> {
    left: Box<dyn Query<'a>>,
    right: Box<dyn Query<'a>>,
}

impl<'a> Query<'a> for AndQuery<'a> {
    fn matches(&self, store: &Store, thing: &'_ Thing) -> bool {
        self.left.matches(store, thing) && self.right.matches(store, thing)
    }
}

//  *** Or

pub struct OrQuery<'a> {
    left: Box<dyn Query<'a>>,
    right: Box<dyn Query<'a>>,
}

impl<'a> Query<'a> for OrQuery<'a> {
    fn matches(&self, store: &Store, thing: &'_ Thing) -> bool {
        self.left.matches(store, thing) || self.right.matches(store, thing)
    }
}

//  *** Xor

pub struct XorQuery<'a> {
    left: Box<dyn Query<'a>>,
    right: Box<dyn Query<'a>>,
}

impl<'a> Query<'a> for XorQuery<'a> {
    fn matches(&self, store: &Store, thing: &'_ Thing) -> bool {
        self.left.matches(store, thing) != self.right.matches(store, thing)
    }
}

// * The Query<'a> parser

/**
Queries in Tkacz are made by combining Query<'a>s.  The standard
format is Query<'a>:argument, for example:

authors:Foucault
:MyCollection
context:SomeContext
title:"*lambda*"

Some Query<'a>s allow for a more complex structures, with (eg) comparison operators:

date>2020-01-01
in:authors:Foucault

The Query<'a> parser has macros. Some non-alphabetic characters expand
to Query<'a>s.  The most simple example is tag selection with #some-tag,
which expands to

in:(tag)some-tag

The parser also recognizes the classic boolean infix operators
`and`, `or`, and `xor` (aliased to &, |, !=) and the prefix not, !.
 */

pub struct QueryParser {}
