use std::fmt;

use lazy_static::lazy_static;
use regex::Regex;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ColumnName(String);

impl ColumnName {
    pub fn new(value: String) -> Self {
        ColumnName(value)
    }
}

impl fmt::Display for ColumnName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(value: String) -> Self {
        Identifier(value)
    }

    pub fn as_column_name(self) -> ColumnName {
        ColumnName(self.0)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn ident<S: Into<String>>(value_s: S) -> Identifier {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-zA-Z0-9_\-]+$").unwrap();
    }
    let value = value_s.into();
    assert!(RE.is_match(&value), "Invalid identifier");
    Identifier::new(value)
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeName(String);

impl TypeName {
    pub fn new(value: String) -> Self {
        TypeName(value)
    }
}
