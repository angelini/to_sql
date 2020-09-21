use std::fmt;

use lazy_static::lazy_static;
use regex::Regex;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(String);

impl Identifier {
    pub fn check(value: &str) -> bool {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^[a-zA-Z0-9_\-]+$").unwrap();
        }
        return RE.is_match(value)
    }

    pub fn from_string(value: String) -> Option<Identifier> {
        match Self::check(&value) {
            true => Some(Identifier(value)),
            false => None
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

pub fn ident<S: Into<String>>(value_s: S) -> Identifier {
    Identifier::from_string(value_s.into()).unwrap()
}
