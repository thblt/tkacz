//! Tkacz, a library

pub mod config;
pub mod query;
pub mod store;
pub mod taxonomy;
pub mod thing;
pub mod types;

pub use crate::query::{Query, QueryParser};
pub use crate::store::Store;
pub use crate::taxonomy::{ContentSource, Taxon};
pub use crate::thing::Thing;
pub use crate::types::{Class, Named, NamedType, NamedValue, Object, Type, Value};

pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub type Id = i64;
pub const TKACZ_DIR_NAME: &str = "tkacz";
