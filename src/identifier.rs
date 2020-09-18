use std::fmt;

use regex::Regex;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(String);

impl Identifier {
    fn from_string(value: String) -> Option<Identifier> {
        let re = Regex::new(r"^[a-zA-Z0-9_\-]+$").unwrap();
        match re.is_match(&value) {
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
