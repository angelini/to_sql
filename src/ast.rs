use std::collections::BTreeMap;

use chrono::naive::NaiveDate;
use rust_decimal::Decimal;

use crate::types::{Base, Type};

#[derive(Debug, Eq, PartialEq)]
enum Constant {
    Bool(bool),
    Int(usize),
    Float(Decimal),
    String(String),
    Date(NaiveDate),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Identifier(String);

#[derive(Debug, Eq, PartialEq)]
struct Assignment(Identifier, Expression);

// Row: { a: A, b: B }
// Alias: type A = B
// Function: f :: P: Primitive :: P? -> P

#[derive(Debug, Eq, PartialEq)]
pub enum TypeExpression {
    Row(Type),
    Alias(Identifier, Type),
    Function(Identifier, Type)
}

// Constant: 5
// Variable: a
// Application: test(a, 5)
// Block: { let a = 1; a }
// Function: a, b -> a

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Constant(Constant),
    Variable(Identifier),
    Assignment(Box<Assignment>),
    Application(Identifier, Vec<Expression>),
    Block(Vec<Assignment>, Box<Expression>),
    Function(Vec<Identifier>, Box<Expression>),
    Type(TypeExpression),
}
