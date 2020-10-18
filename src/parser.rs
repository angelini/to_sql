use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;

use chrono::naive::NaiveDate;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use rust_decimal::Decimal;

use crate::base::{ColumnName, Constant, Identifier, Kind, Kinds, TypeName};

pub type RowValue = BTreeMap<ColumnName, Token>;
pub type RowType = BTreeMap<ColumnName, TypeName>;

#[derive(Clone, Debug)]
pub enum Token {
    Constant(Constant),
    Identifier(Identifier),
    RowValue(RowValue),
    Block(Vec<Token>),
    Access(Identifier, ColumnName),
    Let(Identifier, TypeName, Box<Token>),
    Application(Identifier, Vec<Token>),
    Function(Vec<Identifier>, Box<Token>),

    TypeName(TypeName),
    GenericTypeName(Vec<TypeName>),
    Union(Vec<Token>),
    RowType(RowType),
    TypeAlias(TypeName, Box<Token>),
    NamedKind(TypeName, Kind),
    FunctionType(Vec<Token>, Box<Token>),

    Assignment(Identifier, Box<Token>),
    TypeAssignment(Identifier, Box<Token>),
    GenericTypeAssignment(Identifier, Kinds, Box<Token>),
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

fn map_tokens<'a, F>(input: &'a str, parser: Box<dyn Parser>, func: F) -> (Tokens, &'a str)
where
    F: FnOnce(Vec<Token>) -> Vec<Token>,
{
    let (tokens, rest) = parser.take(input);
    match tokens {
        Some(ts) => (Some(func(ts)), rest),
        None => fail(input),
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
struct RepeatParser(Box<dyn Parser>);

impl Parser for RepeatParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        let mut all_tokens = vec![];
        let mut remaining = input;

        loop {
            let (tokens, rest) =
                chain!(optional(WsParser), self.0.clone(), optional(WsParser)).take(remaining);
            remaining = rest;

            match tokens {
                Some(mut ts) => {
                    all_tokens.append(&mut ts);
                }
                None => return (Some(all_tokens), rest),
            }
        }
    }
}

macro_rules! repeat {
    ($parser:expr) => {
        RepeatParser($parser.into())
    };
}

#[derive(Clone, Debug)]
struct DelimitedParser {
    parser: Box<dyn Parser>,
    delimiter: Box<dyn Parser>,
}

impl Parser for DelimitedParser {
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

macro_rules! delimited {
    ($parser:expr, $delimiter:expr) => {
        DelimitedParser {
            parser: $parser.into(),
            delimiter: $delimiter.into(),
        }
    };
}

#[derive(Clone, Debug)]
struct FixedParser {
    pattern: String,
    capture: bool,
}

impl Parser for FixedParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        if input.starts_with(&self.pattern) {
            let tokens = if self.capture {
                constant(Constant::String(self.pattern.clone()))
            } else {
                Some(vec![])
            };
            (tokens, &input[(self.pattern.len())..])
        } else {
            fail(input)
        }
    }
}

fn fixed<S: Into<String>>(value: S) -> FixedParser {
    FixedParser {
        pattern: value.into(),
        capture: false,
    }
}

fn capture<S: Into<String>>(value: S) -> FixedParser {
    FixedParser {
        pattern: value.into(),
        capture: true,
    }
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
            static ref RE: Regex = Regex::new(r"^[a-z_][a-zA-Z0-9_\-]*").unwrap();
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
struct NamedKindParser;

impl Parser for NamedKindParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                TypeNameParser,
                optional(WsParser),
                fixed(":"),
                optional(WsParser),
                choose!(capture("Row"), capture("Primitive"))
            )),
            |mut tokens| match (tokens.remove(0), tokens.remove(0)) {
                (Token::TypeName(name), Token::Constant(Constant::String(kind_str))) => {
                    vec![Token::NamedKind(name, Kind::from_str(&kind_str).unwrap())]
                }
                _ => unreachable!(),
            },
        )
    }
}

#[derive(Clone, Debug)]
struct RowValueParser;

impl Parser for RowValueParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                fixed("{"),
                delimited!(
                    chain!(
                        choose!(IdentifierParser, StringParser),
                        optional(WsParser),
                        fixed(":"),
                        optional(WsParser),
                        ExpressionParser
                    ),
                    fixed(",")
                ),
                fixed("}")
            )),
            |tokens| {
                let mut row_value = BTreeMap::new();

                for mut chunk in tokens.into_iter().chunks(2).into_iter() {
                    let column_name = match chunk.next().unwrap() {
                        Token::Constant(Constant::String(s)) => ColumnName::new(s),
                        Token::Identifier(ident) => ident.into_column_name(),
                        _ => unreachable!(),
                    };
                    row_value.insert(column_name, chunk.next().unwrap());
                }
                vec![Token::RowValue(row_value)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct BlockParser;

impl Parser for BlockParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                fixed("{"),
                optional(WsParser),
                delimited!(ExpressionParser, fixed(";")),
                optional(WsParser),
                fixed("}")
            )),
            |tokens| vec![Token::Block(tokens)],
        )
    }
}

#[derive(Clone, Debug)]
struct AccessParser;

impl Parser for AccessParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                IdentifierParser,
                optional(WsParser),
                fixed("."),
                optional(WsParser),
                choose!(IdentifierParser, StringParser)
            )),
            |mut tokens| {
                let column_name = match tokens.pop() {
                    Some(Token::Identifier(ident)) => ident.into_column_name(),
                    Some(Token::Constant(Constant::String(s))) => ColumnName::new(s),
                    _ => unreachable!(),
                };
                let ident = match tokens.pop() {
                    Some(Token::Identifier(ident)) => ident,
                    _ => unreachable!(),
                };
                vec![Token::Access(ident, column_name)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct AssignmentParser;

impl Parser for AssignmentParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                IdentifierParser,
                optional(WsParser),
                fixed("="),
                optional(WsParser),
                ExpressionParser
            )),
            |mut tokens| {
                let expression = Box::new(tokens.pop().unwrap());
                let ident = match tokens.pop() {
                    Some(Token::Identifier(ident)) => ident,
                    _ => unreachable!(),
                };
                vec![Token::Assignment(ident, expression)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct LetParser;

impl Parser for LetParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                fixed("let"),
                WsParser,
                IdentifierParser,
                optional(WsParser),
                fixed("::"),
                optional(WsParser),
                TypeNameParser,
                optional(WsParser),
                fixed("="),
                optional(WsParser),
                ExpressionParser
            )),
            |mut tokens| {
                let expression = Box::new(tokens.pop().unwrap());
                let (ident, type_name) = match (tokens.remove(0), tokens.remove(0)) {
                    (Token::Identifier(ident), Token::TypeName(type_name)) => (ident, type_name),
                    _ => unreachable!(),
                };
                vec![Token::Let(ident, type_name, expression)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct ApplicationParser;

impl Parser for ApplicationParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                IdentifierParser,
                fixed("("),
                optional(WsParser),
                delimited!(ExpressionParser, fixed(",")),
                optional(WsParser),
                fixed(")")
            )),
            |mut tokens| {
                let ident = match tokens.remove(0) {
                    Token::Identifier(ident) => ident,
                    _ => unreachable!(),
                };
                vec![Token::Application(ident, tokens)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct ComparatorParser;

impl Parser for ComparatorParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^(==|!=|>=|<=|>|<)").unwrap();
        }
        match RE.find(input) {
            Some(m) => {
                let cmp_fn = format!(
                    "__{}__",
                    match m.as_str() {
                        "==" => "eq",
                        "!=" => "ne",
                        ">=" => "gte",
                        "<=" => "lte",
                        ">" => "gt",
                        "<" => "lt",
                        _ => unreachable!(),
                    }
                );
                (
                    Some(vec![Token::Identifier(Identifier::new(cmp_fn))]),
                    &input[m.end()..],
                )
            }
            None => fail(input),
        }
    }
}

#[derive(Clone, Debug)]
struct InfixParser;

impl Parser for InfixParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                choose!(
                    RowValueParser,
                    ApplicationParser,
                    AccessParser,
                    ConstantParser,
                    IdentifierParser
                ),
                optional(WsParser),
                ComparatorParser,
                optional(WsParser),
                choose!(
                    InfixParser,
                    RowValueParser,
                    ApplicationParser,
                    AccessParser,
                    ConstantParser,
                    IdentifierParser
                )
            )),
            |mut tokens| {
                let right = tokens.pop().unwrap();
                let cmp = match tokens.pop() {
                    Some(Token::Identifier(ident)) => ident,
                    _ => unreachable!(),
                };
                let left = tokens.pop().unwrap();
                vec![Token::Application(cmp, vec![left, right])]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct FunctionParser;

impl Parser for FunctionParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                fixed("("),
                optional(WsParser),
                delimited!(IdentifierParser, fixed(",")),
                optional(WsParser),
                fixed(")"),
                optional(WsParser),
                fixed("->"),
                optional(WsParser),
                ExpressionParser
            )),
            |mut tokens| {
                let body = tokens.pop().unwrap();
                let arguments = tokens
                    .into_iter()
                    .map(|ident_token| match ident_token {
                        Token::Identifier(ident) => ident,
                        _ => unreachable!(),
                    })
                    .collect();
                vec![Token::Function(arguments, Box::new(body))]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct ExpressionParser;

impl Parser for ExpressionParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        choose!(
            InfixParser,
            RowValueParser,
            BlockParser,
            LetParser,
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
struct TypeNameParser;

impl Parser for TypeNameParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^[A-Z][a-zA-Z0-9]*").unwrap();
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
struct GenericTypeNameParser;

impl Parser for GenericTypeNameParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                TypeNameParser,
                fixed("<"),
                choose!(GenericTypeNameParser, TypeNameParser),
                fixed(">")
            )),
            |tokens| {
                let type_names = tokens
                    .into_iter()
                    .map(|token| match token {
                        Token::TypeName(name) => vec![name],
                        Token::GenericTypeName(names) => names,
                        _ => unreachable!(),
                    })
                    .flatten()
                    .collect();
                vec![Token::GenericTypeName(type_names)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct UnionParser;

impl Parser for UnionParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                choose!(GenericTypeNameParser, TypeNameParser),
                optional(WsParser),
                fixed("|"),
                optional(WsParser),
                delimited!(choose!(GenericTypeNameParser, TypeNameParser), fixed("|"))
            )),
            |tokens| vec![Token::Union(tokens)],
        )
    }
}

#[derive(Clone, Debug)]
struct RowTypeParser;

impl Parser for RowTypeParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                fixed("{"),
                delimited!(
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
            )),
            |tokens| {
                let mut row_type = BTreeMap::new();

                for mut chunk in tokens.into_iter().chunks(2).into_iter() {
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
                vec![Token::RowType(row_type)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct TypeAliasParser;

impl Parser for TypeAliasParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                fixed("type"),
                optional(WsParser),
                TypeNameParser,
                optional(WsParser),
                fixed("="),
                optional(WsParser),
                TypeParser
            )),
            |mut tokens| {
                let typ = tokens.pop().unwrap();
                let name = match tokens.pop() {
                    Some(Token::TypeName(type_name)) => type_name,
                    _ => unreachable!(),
                };
                vec![Token::TypeAlias(name, Box::new(typ))]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct FunctionTypeParser;

impl Parser for FunctionTypeParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                fixed("("),
                optional(WsParser),
                delimited!(TypeParser, fixed(",")),
                optional(WsParser),
                fixed(")"),
                optional(WsParser),
                fixed("->"),
                optional(WsParser),
                TypeParser
            )),
            |mut tokens| {
                let return_type = tokens.pop().unwrap();
                vec![Token::FunctionType(tokens, Box::new(return_type))]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct TypeAssignmentParser;

impl Parser for TypeAssignmentParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                IdentifierParser,
                optional(WsParser),
                fixed("::"),
                optional(WsParser),
                TypeParser
            )),
            |mut tokens| {
                let typ = Box::new(tokens.pop().unwrap());
                let ident = match tokens.pop() {
                    Some(Token::Identifier(ident)) => ident,
                    _ => unreachable!(),
                };
                vec![Token::TypeAssignment(ident, typ)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct GenericTypeAssignmentParser;

impl Parser for GenericTypeAssignmentParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        map_tokens(
            input,
            Box::new(chain!(
                IdentifierParser,
                optional(WsParser),
                fixed("::"),
                optional(WsParser),
                delimited!(NamedKindParser, fixed(",")),
                optional(WsParser),
                fixed("::"),
                optional(WsParser),
                TypeParser
            )),
            |mut tokens| {
                let typ = Box::new(tokens.pop().unwrap());
                let ident = match tokens.remove(0) {
                    Token::Identifier(ident) => ident,
                    _ => unreachable!(),
                };

                let kinds = tokens
                    .into_iter()
                    .map(|token| match token {
                        Token::NamedKind(name, kind) => (name, kind),
                        _ => unreachable!(),
                    })
                    .collect();
                vec![Token::GenericTypeAssignment(ident, Kinds::new(kinds), typ)]
            },
        )
    }
}

#[derive(Clone, Debug)]
struct TypeParser;

impl Parser for TypeParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        choose!(
            RowTypeParser,
            UnionParser,
            GenericTypeNameParser,
            TypeAliasParser,
            FunctionTypeParser,
            GenericTypeAssignmentParser,
            TypeAssignmentParser,
            TypeNameParser
        )
        .take(input)
    }
}

#[derive(Clone, Debug)]
struct RootTokenParser;

impl Parser for RootTokenParser {
    fn take<'a, 'b>(&'a self, input: &'b str) -> (Tokens, &'b str) {
        repeat!(choose!(
            TypeAliasParser,
            GenericTypeAssignmentParser,
            TypeAssignmentParser,
            AssignmentParser
        ))
        .take(input)
    }
}

#[derive(Debug)]
pub struct ParseError(String);

impl ParseError {
    pub fn from_str<S: Into<String>>(value: S) -> Self {
        ParseError(value.into())
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.0)
    }
}

pub fn parse(input: &str) -> Result<Vec<Token>, ParseError> {
    match RootTokenParser.take(input) {
        (Some(tokens), rest) => {
            if rest.is_empty() {
                Ok(tokens)
            } else {
                Err(ParseError::from_str(rest))
            }
        }
        (None, _) => Err(ParseError::from_str(input)),
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
