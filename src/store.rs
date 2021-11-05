use crate::*;
use std::path::{Path, PathBuf};
use std::{fmt, fs, io};

// * The Store type

/// A Store
pub struct Store {
    path: PathBuf,
    writable: bool,
    pub(crate) things: std::vec::Vec<Thing>,
}

// * Implementation

impl<'a> Store {
    /// Open an existing Tkacz store.
    pub fn open(path: &Path, read_only: bool) -> Result<Store, StoreAccessError> {
        let metadata = fs::metadata(path)?;
        if metadata.is_dir() {
            let ret = Store {
                path: path.to_path_buf(),
                writable: !(read_only || metadata.permissions().readonly()),
                things: std::vec!(),
            };
            return Ok(ret);
        } else {
            Err(StoreAccessError::new("Not a directory.".to_string()))
        }
    }

    /// Initialize a new store into an existing, empty directory.
    pub fn init(path: &Path) -> Result<(), StoreAccessError> {
        let metadata = fs::metadata(path)?;
        if !metadata.is_dir() {
            Err(StoreAccessError::new("Not a directory.".to_string()))
        } else if metadata.permissions().readonly() {
            Err(StoreAccessError::new("Not writable.".to_string()))
        } else if fs::read_dir(path)?.next().is_some() {
            Err(StoreAccessError::new("Not empty.".to_string()))
        } else {
            // Todo create store.
            Ok(())
        }
    }
    /// Add a [Thing] to that store.
    pub fn new_thing(&mut self, name: String) -> &mut Thing {
        let id = self.things.len();
        self.things.push(Thing { id: id as Id, name });
        &mut self.things[id]
    }
}

// * Errors

#[derive(Debug, Clone)]
/// Errors raised when accessing stores.
pub struct StoreAccessError {
    msg: String,
}

impl StoreAccessError {
    fn new(msg: String) -> StoreAccessError {
        StoreAccessError { msg }
    }
}

impl fmt::Display for StoreAccessError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[StoreAccessError] {}", self.msg)
    }
}

impl From<io::Error> for StoreAccessError {
    fn from(error: io::Error) -> Self {
        StoreAccessError {
            msg: format!("IO Error: {}", error).to_string(),
        }
    }
}
