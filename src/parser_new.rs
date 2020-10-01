use std::collections::BTreeMap;
use std::str::FromStr;

use chrono::naive::NaiveDate;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use rust_decimal::Decimal;

use crate::ast::Constant;
use crate::base::{ColumnName, Identifier, TypeName};

pub type RowValue = BTreeMap<ColumnName, ExprToken>;
pub type RowType = BTreeMap<ColumnName, TypeName>;

pub type Kind = (TypeName, TypeName);

trait Token: core::clone::Clone + core::fmt::Debug {}

#[derive(Clone, Debug)]
pub enum TypeToken {
    TypeName(TypeName),
    GenericTypeName(Vec<TypeName>),
    Union(Vec<TypeToken>),
    RowType(RowType),
    TypeAlias(TypeName, Box<TypeToken>),
    FunctionType(Vec<TypeToken>, Box<TypeToken>),
}

impl Token for TypeToken {}

type TypeResult<'a> = (Option<Vec<TypeToken>>, &'a str);

#[derive(Clone, Debug)]
pub enum ExprToken {
    Constant(Constant),
    Identifier(Identifier),
    Kind(Kind),
    RowValue(RowValue),
    Block(Vec<ExprToken>),
    Access(Identifier, ColumnName),
    Assignment(Identifier, Box<ExprToken>),
    Let(Identifier, Box<ExprToken>),
    Application(Identifier, Vec<ExprToken>),
    Function(Vec<Identifier>, Box<ExprToken>),
}

impl Token for ExprToken {}

type ExprResult<'a> = (Option<Vec<ExprToken>>, &'a str);

#[derive(Clone, Debug)]
pub enum RootToken {
    TypeAlias(TypeName, Box<TypeToken>),
    TypeAssignment(Identifier, Box<TypeToken>),
    GenericTypeAssignment(Identifier, Vec<Kind>, Box<TypeToken>),
}

impl Token for RootToken {}

fn fail<T>(input: &str) -> (Option<Vec<T>>, &str) {
    (None, input)
}

trait Parser<T>: std::fmt::Debug + ParserClone<T> {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Option<Vec<T>>, &'b str);
}

trait ParserClone<T> {
    fn clone_box(&self) -> Box<dyn Parser<T>>;
}

impl<P, T> ParserClone<T> for P
where
    P: 'static + Parser<T> + Clone,
{
    fn clone_box(&self) -> Box<dyn Parser<T>> {
        Box::new(self.clone())
    }
}

impl<T> Clone for Box<dyn Parser<T>> {
    fn clone(&self) -> Box<dyn Parser<T>> {
        self.clone_box()
    }
}

impl<T> From<T> for Box<dyn Parser<TypeToken>>
where
    T: 'static + Parser<TypeToken>
{
    fn from(parser: T) -> Self {
        Box::new(parser)
    }
}

impl<T> From<T> for Box<dyn Parser<ExprToken>>
where
    T: 'static + Parser<ExprToken>
{
    fn from(parser: T) -> Self {
        Box::new(parser)
    }
}

fn map_tokens<'a, F, T>(
    input: &'a str,
    parser: Box<dyn Parser<T>>,
    func: F,
) -> (Option<Vec<T>>, &'a str)
where
    F: FnOnce(Vec<T>) -> Vec<T>,
{
    let (tokens, rest) = parser.take(input);
    match tokens {
        Some(ts) => (Some(func(ts)), rest),
        None => fail(input),
    }
}

#[derive(Clone, Debug)]
struct WsParser;

impl Parser<TypeToken> for WsParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> TypeResult<'b> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^\s+").unwrap();
        }
        match RE.find(input) {
            Some(m) => (Some(vec![]), &input[m.end()..]),
            None => fail(input),
        }
    }
}

impl Parser<ExprToken> for WsParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> ExprResult<'b> {
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
struct OptionalParser<T>(Box<dyn Parser<T>>);

impl<T> Parser<T> for OptionalParser<T>
where
    T: 'static + Token,
{
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Option<Vec<T>>, &'b str) {
        let (tokens, rest) = self.0.take(input);
        match tokens {
            Some(_) => (tokens, rest),
            None => (Some(vec![]), input),
        }
    }
}

fn optional<T: Token, P: Into<Box<dyn Parser<T>>>>(parser: P) -> OptionalParser<T> {
    OptionalParser(parser.into())
}

#[derive(Clone, Debug)]
struct ChooseParser<T>(Vec<Box<dyn Parser<T>>>);

impl<T> Parser<T> for ChooseParser<T>
where
    T: 'static + Token,
{
    fn take<'b, 'c>(&'c self, input: &'b str) -> (Option<Vec<T>>, &'b str) {
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
struct ChainParser<T>(Vec<Box<dyn Parser<T>>>);

impl<T> Parser<T> for ChainParser<T>
where
    T: 'static + Token,
{
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Option<Vec<T>>, &'b str) {
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
struct BlockParser;

impl Parser<ExprToken> for BlockParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> ExprResult<'b> {
        map_tokens(
            input,
            Box::new(chain!(optional(WsParser), BlockParser)),
            |tokens| vec![ExprToken::Block(tokens)],
        )
    }
}

#[derive(Clone, Debug)]
struct ExpressionParser;

impl Parser<ExprToken> for ExpressionParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> ExprResult<'b> {
        choose!(BlockParser).take(input)
    }
}
