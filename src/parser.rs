use std::str::FromStr;

use chrono::naive::NaiveDate;
use lazy_static::lazy_static;
use regex::{Regex, RegexSet};
use rust_decimal::Decimal;

use crate::ast::Constant;
use crate::identifier::Identifier;

enum Token {
    Constant(Constant),
    Variable(Identifier),
    EOF,
}

trait Parser {
    fn check(input: &str) -> bool;
    fn take(input: &str) -> (Token, &str);
}

struct ConstantParser;

impl Parser for ConstantParser {
    fn check(input: &str) -> bool {
        lazy_static! {
            static ref RE: RegexSet = RegexSet::new(&[
                r"^true|false",
                r"^\d{4}-[0-1][0-9]-[0-3][0-9]",
                r"^\d+.\d+",
                r"^\d+",
                r"^'[0-9a-zA-Z _-]+'",
            ])
            .unwrap();
        }
        RE.is_match(input)
    }

    fn take(input: &str) -> (Token, &str) {
        lazy_static! {
            static ref BOOL_RE: Regex = Regex::new(r"^true|false").unwrap();
            static ref DATE_RE: Regex = Regex::new(r"^\d{4}-[0-1][0-9]-[0-3][0-9]").unwrap();
            static ref FLOAT_RE: Regex = Regex::new(r"^\d+.\d+").unwrap();
            static ref INT_RE: Regex = Regex::new(r"^\d+").unwrap();
            static ref STRING_RE: Regex = Regex::new(r"^'[0-9a-zA-Z _-]+'").unwrap();
        }
        if let Some(m) = BOOL_RE.find(input) {
            let value = &input[0..m.end()];
            return (Token::Constant(Constant::Bool(value == "true")), &input[m.end()..])
        }
        if let Some(m) = FLOAT_RE.find(input) {
            let value = &input[0..m.end()];
            let year = value[0..4].parse::<i32>().unwrap();
            let month = value[5..6].parse::<u32>().unwrap();
            let day = value[7..8].parse::<u32>().unwrap();
            return (
                Token::Constant(Constant::Date(NaiveDate::from_ymd(year, month, day))),
                &input[m.end()..]
            )
        }
        if let Some(m) = FLOAT_RE.find(input) {
            let value = &input[0..m.end()];
            return (
                Token::Constant(Constant::Float(Decimal::from_str(value).unwrap())),
                &input[m.end()..],
            )
        }
        if let Some(m) = INT_RE.find(input) {
            let value = &input[0..m.end()];
            return (
                Token::Constant(Constant::Int(value.parse::<usize>().unwrap())),
                &input[m.end()..],
            )
        }
        if let Some(m) = STRING_RE.find(input) {
            let value = &input[1..m.end() - 1];
            return (
                Token::Constant(Constant::String(value.to_string())),
                &input[m.end()..],
            )
        }
        unreachable!()
    }
}

struct VariableParser;

impl Parser for VariableParser {
    fn check(input: &str) -> bool {
        // FIXME: check looks for a complete string `$`
        Identifier::check(input)
    }

    fn take(input: &str) -> (Token, &str) {
        unimplemented!()
    }
}
