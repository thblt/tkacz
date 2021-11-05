use crate::*;

/// A Thing in Tkacz is something and anything that can be part of a
/// Store.  Things are further characterized by their type.  They have
/// categories, links and backlinks.
pub struct Thing {
    /// The id of this Thing in its store.
    pub(crate) id: Id,
    /// The name of this Thing.
    pub(crate) name: String,
}

impl Thing {
    pub(crate) fn new<'a> (store: &'a mut Store, name: String) -> &'a Thing {
        store.new_thing(name)
    }
}
