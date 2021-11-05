/*!
Tkacz classification infrastructure.

Taxons comes in many flavors.  Fundamentally, they have a name and
hold a set of [Things](crate::thing::Thing).  Some behave like tags,
other like a structured classification.

Taxons can obtain their contents from multiple sources.  The most
basic
 */

use crate::*;
use std::slice::Iter;
use std::iter::Iterator;

// * Types

/// A classification entry.
pub struct Taxon<'a> {
    /// A name for this taxon, to use in queries.
    name: &'a str,
    /// The "type"of this taxon.  This is mostly for UI.
    pub class: TaxonClass,
    /// Where the things in this taxon come from.
    pub content_source: ContentSource<'a>,
    /// Contents cache.
    pub contents: Vec<&'a Thing>,
    /// Children taxons
    pub children: Vec<Taxon<'a>>
}

impl<'a> Taxon<'a> {
    /// Refresh the contents of a Taxon, given a Store.
    pub fn refresh_contents(&mut self, store: &Store) {
        self.contents = self.content_source.gather(store).collect();
    }

    pub fn get_name(&'a self) -> &'a str{
        self.name
}

pub fn set_name(&mut self, name: &'a str) {
        self.name = name;
    }
}

/// How this taxon behaves, UI-wise
pub enum TaxonClass {
    /// Category is the most basic.
    Category,
    /// Tags have unique names among Tag taxons, and have priority
    /// over non-tag taxons for their names.  They can be selected
    /// with the pound-sign reader macro, #tag_name.
    Tag,
    /// Not exposed in regular UI. Used for "pure" contexts.
    Internal,
}

/// How a [Taxon] receives its contents.
pub enum ContentSource<'a> {
    /// All contents are added by hand.
    Manual { elements: Iter<'a, Thing> },
    /// Collects the contents from its children.
    Meta,
    /// Contains the results of Query.
    Dynamic { query: Box<dyn Query<'a>> },
    /// Externally controlled.
    Driven,
}

/// How a [Taxon] gets children.
pub enum TaxonChildren<'a> {
    /// No children.
    None,
    ///
    Manual { children: Vec<Taxon<'a>> },
}

impl<'a> ContentSource<'a> {
    fn gather(&self, store: &Store) -> impl Iterator<Item = &'a Thing> {
        match self {
            ContentSource::Manual { elements } => elements.clone(),
            ContentSource::Meta => todo!(),
            ContentSource::Dynamic { query } => todo!(),
            ContentSource::Driven => todo!(),
        }
    }
}
