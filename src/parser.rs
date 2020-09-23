use std::str::FromStr;

use chrono::naive::NaiveDate;
use lazy_static::lazy_static;
use regex::Regex;
use rust_decimal::Decimal;

use crate::ast::Constant;
use crate::identifier::Identifier;

#[derive(Debug)]
enum Token {
    Constant(Constant),
    Variable(Identifier),
    Assignment(Box<Token>, Box<Token>),
    Application(Box<Token>, Vec<Token>),
}

type Tokens = Option<Vec<Token>>;

fn fail(input: &str) -> (Tokens, &str) {
    (None, input)
}

fn constant(c: Constant) -> Tokens {
    Some(vec![Token::Constant(c)])
}

trait Parser: std::fmt::Debug + ParserClone {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str);
}

trait ParserClone {
    fn clone_box(&self) -> Box<dyn Parser>;
}

impl<T> ParserClone for T
where
    T: 'static + Parser + Clone,
{
    fn clone_box(&self) -> Box<dyn Parser> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Parser> {
    fn clone(&self) -> Box<dyn Parser> {
        self.clone_box()
    }
}

impl<T> From<T> for Box<dyn Parser>
where
    T: 'static + Parser
{
    fn from(parser: T) -> Self {
        Box::new(parser)
    }
}

#[derive(Clone, Debug)]
struct WsParser;

impl Parser for WsParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\s+").unwrap();
        }
        match RE.find(input) {
            Some(m) => (Some(vec![]), &input[m.end()..]),
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct ChooseParser(Vec<Box<dyn Parser>>);

impl Parser for ChooseParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        for parser in &self.0 {
            let (tokens, rest) = parser.take(input);
            if tokens.is_some() {
                return (tokens, rest);
            }
        }
        fail(input)
    }
}

macro_rules! choose {
    ($($typ:expr),+) => {
        ChooseParser(vec![
            $(
                $typ.into(),
            )*
        ])
    }
}

#[derive(Clone, Debug)]
struct ChainParser(Vec<Box<dyn Parser>>);

impl Parser for ChainParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let mut all_tokens = vec![];
        let mut current_input = input;
        for parser in &self.0 {
            let (tokens, rest) = parser.take(current_input);
            match tokens {
                Some(mut ts) => {
                    all_tokens.append(&mut ts);
                    current_input = rest;
                }
                None => return fail(input),
            }
        }
        (Some(all_tokens), current_input)
    }
}

macro_rules! chain {
    ($($typ:expr),+) => {
        ChainParser(vec![
            $(
                $typ.into(),
            )*
        ])
    }
}

#[derive(Clone, Debug)]
struct RepeatParser {
    parser: Box<dyn Parser>,
    delimiter: Box<dyn Parser>,
}

impl Parser for RepeatParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let mut all_tokens = vec![];
        let mut remaining = input;

        loop {
            let (tokens, rest) = chain!(self.parser.clone(), WsParser, self.delimiter.clone()).take(remaining);
            remaining = rest;

            match tokens {
                Some(mut ts) => {
                    all_tokens.push(ts.pop().unwrap());
                }
                None => {
                    let (tokens, rest) = chain!(WsParser, self.parser.clone()).take(remaining);
                    if let Some(mut ts) = tokens {
                        all_tokens.push(ts.pop().unwrap())
                    }
                    return (Some(all_tokens), rest)
                }
            }
        }
    }
}

macro_rules! repeat {
    ($parser:expr, $delimiter:expr) => {
        RepeatParser{ parser: $parser.into(), delimiter: $delimiter.into() }
    }
}

#[derive(Clone, Debug)]
struct FixedParser(String);

impl Parser for FixedParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        if input.starts_with(&self.0) {
            (Some(vec![]), &input[1..])
        } else {
            fail(input)
        }
    }
}

fn fixed<S: Into<String>>(value: S) -> FixedParser {
    FixedParser(value.into())
}

#[derive(Clone, Debug)]
struct BoolParser;

impl Parser for BoolParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^true|false").unwrap();
        }
        match RE.find(input) {
            Some(m) => (
                constant(Constant::Bool(&input[0..m.end()] == "true")),
                &input[m.end()..],
            ),
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct IntParser;

impl Parser for IntParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\d+").unwrap();
        }
        match RE.find(input) {
            Some(m) => (
                constant(Constant::Int(input[0..m.end()].parse::<usize>().unwrap())),
                &input[m.end()..],
            ),
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct FloatParser;

impl Parser for FloatParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\d+.\d+").unwrap();
        }
        match RE.find(input) {
            Some(m) => (
                constant(Constant::Float(
                    Decimal::from_str(&input[0..m.end()]).unwrap(),
                )),
                &input[m.end()..],
            ),
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct StringParser;

impl Parser for StringParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^'[0-9a-zA-Z _-]+'").unwrap();
        }
        match RE.find(input) {
            Some(m) => (
                constant(Constant::String(input[1..m.end() - 1].to_string())),
                &input[m.end()..],
            ),
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct DateParser;

impl Parser for DateParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\d{4}-[0-1][0-9]-[0-3][0-9]").unwrap();
        }
        match RE.find(input) {
            Some(m) => {
                let value = &input[0..m.end()];
                let year = value[0..4].parse::<i32>().unwrap();
                let month = value[5..6].parse::<u32>().unwrap();
                let day = value[7..8].parse::<u32>().unwrap();
                (
                    constant(Constant::Date(NaiveDate::from_ymd(year, month, day))),
                    &input[m.end()..],
                )
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct ConstantParser;

impl Parser for ConstantParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        choose!(BoolParser, DateParser, FloatParser, IntParser, StringParser).take(input)
    }
}

#[derive(Clone, Debug)]
struct VariableParser;

impl Parser for VariableParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^[a-zA-Z0-9_\-]+").unwrap();
        }
        match RE.find(input) {
            Some(m) => {
                let ident = Identifier::new(input[0..m.end()].to_string());
                (Some(vec![Token::Variable(ident)]), &input[m.end()..])
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct AssignmentParser;

impl Parser for AssignmentParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        // FIXME: Must end with an ExpressionParser
        let (tokens, rest) = chain!(
            VariableParser,
            WsParser,
            fixed("="),
            WsParser,
            ConstantParser
        )
        .take(input);
        match tokens {
            Some(mut ts) => {
                let expression = Box::new(ts.pop().unwrap());
                let name = Box::new(ts.pop().unwrap());
                (Some(vec![Token::Assignment(name, expression)]), rest)
            }
            None => fail(input),
        }
    }
}

pub fn example(input: &str) {
    let parser = chain!(
        ConstantParser,
        WsParser,
        ConstantParser,
        WsParser,
        VariableParser
    );
    println!("{:?}", parser.take(input))
}

pub fn example_assignment(input: &str) {
    let parser = AssignmentParser;
    println!("{:?}", parser.take(input))
}

pub fn example_repeat(input: &str) {
    let parser = repeat!(VariableParser, fixed(","));
    println!("{:?}", parser.take(input))
}
