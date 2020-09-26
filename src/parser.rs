use std::collections::BTreeMap;
use std::str::FromStr;

use chrono::naive::NaiveDate;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use rust_decimal::Decimal;

use crate::ast::Constant;
use crate::base::{ColumnName, Identifier, TypeName};

pub type RowType = BTreeMap<ColumnName, TypeName>;

pub type Kind = (TypeName, TypeName);

#[derive(Clone, Debug)]
pub enum Token {
    Constant(Constant),
    Identifier(Identifier),
    Kind(Kind),
    Block(Vec<Token>),
    Access(Identifier, ColumnName),
    Assignment(Identifier, Box<Token>),
    Application(Identifier, Vec<Token>),
    Function(Vec<Identifier>, Box<Token>),

    TypeName(TypeName),
    RowType(RowType),
    TypeAlias(TypeName, Box<Token>),
    FunctionType(Vec<Token>, Box<Token>),
    TypeAssignment(Identifier, Box<Token>),
    GenericTypeAssignment(Identifier, Vec<Kind>, Box<Token>)
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
    T: 'static + Parser,
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
struct OptionalParser(Box<dyn Parser>);

impl Parser for OptionalParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = self.0.take(input);
        match tokens {
            Some(_) => (tokens, rest),
            None => (Some(vec![]), rest),
        }
    }
}

fn optional<P: Into<Box<dyn Parser>>>(parser: P) -> OptionalParser {
    OptionalParser(parser.into())
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
            let (tokens, rest) = chain!(
                optional(WsParser),
                self.parser.clone(),
                optional(WsParser),
                self.delimiter.clone()
            )
            .take(remaining);
            remaining = rest;

            match tokens {
                Some(mut ts) => {
                    all_tokens.append(&mut ts);
                }
                None => {
                    let (tokens, rest) =
                        chain!(optional(WsParser), self.parser.clone(), optional(WsParser))
                            .take(remaining);
                    if let Some(mut ts) = tokens {
                        all_tokens.append(&mut ts)
                    }
                    return (Some(all_tokens), rest);
                }
            }
        }
    }
}

macro_rules! repeat {
    ($parser:expr, $delimiter:expr) => {
        RepeatParser {
            parser: $parser.into(),
            delimiter: $delimiter.into(),
        }
    };
}

#[derive(Clone, Debug)]
struct FixedParser(String);

impl Parser for FixedParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        if input.starts_with(&self.0) {
            (Some(vec![]), &input[(self.0.len())..])
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
            static ref RE: Regex = Regex::new(r"^'[^'\\]*(\\.[^'\\]*)*'").unwrap();
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
struct IdentifierParser;

impl Parser for IdentifierParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^[a-zA-Z0-9_\-]+").unwrap();
        }
        match RE.find(input) {
            Some(m) => {
                let ident = Identifier::new(input[0..m.end()].to_string());
                (Some(vec![Token::Identifier(ident)]), &input[m.end()..])
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct TypeNameParser;

impl Parser for TypeNameParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^[A-Z][a-zA-Z0-9]*(<[A-Z][a-zA-Z0-9]*>)?").unwrap();
        }
        match RE.find(input) {
            Some(m) => {
                let type_name = TypeName::new(input[0..m.end()].to_string());
                (Some(vec![Token::TypeName(type_name)]), &input[m.end()..])
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct RowTypeParser;

impl Parser for RowTypeParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            fixed("{"),
            repeat!(
                chain!(
                    choose!(IdentifierParser, StringParser),
                    optional(WsParser),
                    fixed(":"),
                    optional(WsParser),
                    TypeNameParser
                ),
                fixed(",")
            ),
            fixed("}")
        )
        .take(input);

        let mut row_type = BTreeMap::new();

        match tokens {
            Some(ts) => {
                for mut chunk in ts.into_iter().chunks(2).into_iter() {
                    let column_name = match chunk.next().unwrap() {
                        Token::Constant(Constant::String(s)) => ColumnName::new(s),
                        Token::Identifier(ident) => ident.into_column_name(),
                        _ => unreachable!(),
                    };
                    let type_name = match chunk.next() {
                        Some(Token::TypeName(t)) => t,
                        _ => unreachable!(),
                    };
                    row_type.insert(column_name, type_name);
                }
                (Some(vec![Token::RowType(row_type)]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct KindParser;

impl Parser for KindParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            TypeNameParser,
            optional(WsParser),
            fixed(":"),
            optional(WsParser),
            TypeNameParser
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let kind = match (ts.remove(0), ts.remove(0)) {
                    (Token::TypeName(left), Token::TypeName(right)) => Token::Kind((left, right)),
                    _ => unreachable!(),
                };
                (Some(vec![kind]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct AccessParser;

impl Parser for AccessParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            IdentifierParser,
            optional(WsParser),
            fixed("."),
            optional(WsParser),
            choose!(IdentifierParser, StringParser)
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let column_name = match ts.pop() {
                    Some(Token::Identifier(ident)) => ident.into_column_name(),
                    Some(Token::Constant(Constant::String(s))) => ColumnName::new(s),
                    _ => unreachable!(),
                };
                let ident = match ts.pop() {
                    Some(Token::Identifier(ident)) => ident,
                    _ => unreachable!(),
                };
                (Some(vec![Token::Access(ident, column_name)]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct AssignmentParser;

impl Parser for AssignmentParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            IdentifierParser,
            optional(WsParser),
            fixed("="),
            optional(WsParser),
            ExpressionParser
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let expression = Box::new(ts.pop().unwrap());
                let ident = match ts.pop() {
                    Some(Token::Identifier(ident)) => ident,
                    _ => unreachable!(),
                };
                (Some(vec![Token::Assignment(ident, expression)]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct ApplicationParser;

impl Parser for ApplicationParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            IdentifierParser,
            fixed("("),
            optional(WsParser),
            repeat!(ExpressionParser, fixed(",")),
            optional(WsParser),
            fixed(")")
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let ident = match ts.remove(0) {
                    Token::Identifier(ident) => ident,
                    _ => unreachable!(),
                };
                (Some(vec![Token::Application(ident, ts)]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct BlockParser;

impl Parser for BlockParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            fixed("{"),
            optional(WsParser),
            repeat!(ExpressionParser, fixed(";")),
            optional(WsParser),
            fixed("}")
        )
        .take(input);

        match tokens {
            Some(ts) => (Some(vec![Token::Block(ts)]), rest),
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct FunctionParser;

impl Parser for FunctionParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            fixed("("),
            optional(WsParser),
            repeat!(IdentifierParser, fixed(",")),
            optional(WsParser),
            fixed(")"),
            optional(WsParser),
            fixed("->"),
            optional(WsParser),
            ExpressionParser
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let body = ts.pop().unwrap();
                let arguments = ts
                    .into_iter()
                    .map(|ident_token| match ident_token {
                        Token::Identifier(ident) => ident,
                        _ => unreachable!(),
                    })
                    .collect();
                (Some(vec![Token::Function(arguments, Box::new(body))]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct ExpressionParser;

impl Parser for ExpressionParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        choose!(
            BlockParser,
            AssignmentParser,
            ApplicationParser,
            AccessParser,
            FunctionParser,
            ConstantParser,
            IdentifierParser
        )
        .take(input)
    }
}

#[derive(Clone, Debug)]
struct TypeAliasParser;

impl Parser for TypeAliasParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            fixed("type"),
            optional(WsParser),
            TypeNameParser,
            optional(WsParser),
            fixed("="),
            optional(WsParser),
            TypeParser
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let typ = ts.pop().unwrap();
                let name = match ts.pop() {
                    Some(Token::TypeName(type_name)) => type_name,
                    _ => unreachable!(),
                };
                (Some(vec![Token::TypeAlias(name, Box::new(typ))]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct FunctionTypeParser;

impl Parser for FunctionTypeParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            fixed("("),
            optional(WsParser),
            repeat!(TypeParser, fixed(",")),
            optional(WsParser),
            fixed(")"),
            optional(WsParser),
            fixed("->"),
            optional(WsParser),
            TypeParser
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let return_type = ts.pop().unwrap();
                (
                    Some(vec![Token::FunctionType(ts, Box::new(return_type))]),
                    rest,
                )
            }
            None => fail(input),
        }
    }
}


#[derive(Clone, Debug)]
struct TypeAssignmentParser;

impl Parser for TypeAssignmentParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            IdentifierParser,
            optional(WsParser),
            fixed("::"),
            optional(WsParser),
            TypeParser
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let typ = Box::new(ts.pop().unwrap());
                let ident = match ts.pop() {
                    Some(Token::Identifier(ident)) => ident,
                    _ => unreachable!(),
                };
                (Some(vec![Token::TypeAssignment(ident, typ)]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct GenericTypeAssignmentParser;

impl Parser for GenericTypeAssignmentParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let (tokens, rest) = chain!(
            IdentifierParser,
            optional(WsParser),
            fixed("::"),
            optional(WsParser),
            repeat!(KindParser, fixed(",")),
            optional(WsParser),
            fixed("::"),
            optional(WsParser),
            TypeParser
        )
        .take(input);

        match tokens {
            Some(mut ts) => {
                let typ = Box::new(ts.pop().unwrap());
                let ident = match ts.remove(0) {
                    Token::Identifier(ident) => ident,
                    _ => unreachable!(),
                };

                let kinds = ts.into_iter().map(|token| match token {
                    Token::Kind(kind) => kind,
                    _ => unreachable!()
                }).collect();

                (Some(vec![Token::GenericTypeAssignment(ident, kinds, typ)]), rest)
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct TypeParser;

impl Parser for TypeParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        choose!(
            RowTypeParser,
            TypeAliasParser,
            FunctionTypeParser,
            GenericTypeAssignmentParser,
            TypeAssignmentParser,
            TypeNameParser
        )
        .take(input)
    }
}

pub fn parse_expression(input: &str) -> Option<(Token, &str)> {
    let (tokens, rest) = ExpressionParser.take(input);
    tokens.map(|mut ts| (ts.pop().unwrap(), rest))
}

pub fn parse_type(input: &str) -> Option<(Token, &str)> {
    let (tokens, rest) = TypeParser.take(input);
    tokens.map(|mut ts| (ts.pop().unwrap(), rest))
}

pub fn parse_ws(input: &str) -> &str {
    optional(WsParser).take(input).1
}
