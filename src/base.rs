use std::collections::HashMap;
use std::fmt;

use chrono::naive::NaiveDate;
use lazy_static::lazy_static;
use regex::Regex;
use rust_decimal::Decimal;

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

    pub fn into_column_name(self) -> ColumnName {
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
        static ref RE: Regex = Regex::new(r"^[a-z_][a-zA-Z0-9_\-]*$").unwrap();
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

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn type_name<S: Into<String>>(value_s: S) -> TypeName {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[A-Z][a-zA-Z0-9]*(<[A-Z][a-zA-Z0-9]*>)?$").unwrap();
    }
    let value = value_s.into();
    assert!(RE.is_match(&value), "Invalid type name");
    TypeName::new(value)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Kind {
    Primitive,
    Row,
}

impl Kind {
    pub fn from_str(value: &str) -> Option<Kind> {
        match value {
            "Primitive" => Some(Kind::Primitive),
            "Row" => Some(Kind::Row),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Kinds(HashMap<TypeName, Kind>);

impl Kinds {
    pub fn new(values: Vec<(TypeName, Kind)>) -> Self {
        Kinds(values.into_iter().collect())
    }

    pub fn empty() -> Self {
        Kinds(HashMap::new())
    }

    pub fn get(&self, name: &TypeName) -> Option<Kind> {
        self.0.get(name).copied()
    }
}

pub fn kinds<S: Into<String>>(values: Vec<(S, Kind)>) -> Kinds {
    Kinds::new(
        values
            .into_iter()
            .map(|(name, kind)| (type_name(name), kind))
            .collect(),
    )
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constant {
    Bool(bool),
    Int(usize),
    Float(Decimal),
    String(String),
    Date(NaiveDate),
}

#[derive(Clone, Debug)]
pub struct Scope<'a, T> {
    parent: Option<&'a Scope<'a, T>>,
    assignments: HashMap<Identifier, T>,
}

impl<'a, T> Scope<'a, T> {
    pub fn root() -> Self {
        Scope {
            parent: None,
            assignments: HashMap::new(),
        }
    }

    pub fn get(&self, ident: &Identifier) -> Option<&T> {
        self.assignments.get(ident).or_else(|| match &self.parent {
            Some(parent) => parent.get(ident),
            None => None,
        })
    }

    pub fn descend(&'a self, assignments: HashMap<Identifier, T>) -> Self {
        Scope {
            parent: Some(self),
            assignments,
        }
    }

    pub fn insert(&mut self, ident: Identifier, value: T) -> Option<T> {
        self.assignments.insert(ident, value)
    }

    pub fn remove(&mut self, ident: &Identifier) -> Option<T> {
        self.assignments.remove(ident)
    }
}
