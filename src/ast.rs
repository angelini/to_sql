use chrono::naive::NaiveDate;
use rust_decimal::Decimal;

use crate::identifier::Identifier;
use crate::types::{Base, Primitive, Type, TypeContext, TypeError};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constant {
    Bool(bool),
    Int(usize),
    Float(Decimal),
    String(String),
    Date(NaiveDate),
}

impl Constant {
    fn get_type(&self) -> Type {
        let base = match self {
            Constant::Bool(_) => Base::Bool(false),
            Constant::Int(_) => Base::Int(false),
            Constant::Float(_) => Base::Float(false),
            Constant::String(_) => Base::String(false),
            Constant::Date(_) => Base::Date(false),
        };
        Type::Value(Primitive::Known(base))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Constant(Constant),
    Variable(Identifier),
    Assignment(Identifier, Box<Expression>),
    Application(Identifier, Vec<Expression>),
    Block(Vec<(Identifier, Expression)>, Box<Expression>),
    Function(Vec<Identifier>, Box<Expression>),
    Type(Identifier, Type),
}

impl Expression {
    pub fn check_type(&self, ctx: &TypeContext, expected: &Type) -> Result<(), TypeError> {
        match self {
            Expression::Constant(constant) => Self::compare_types(expected, &constant.get_type()),
            Expression::Variable(ident) => Self::compare_types(expected, ctx.get(ident)?),
            Expression::Assignment(ident, expression) => expression.check_type(ctx, ctx.get(ident)?),
            _ => unimplemented!()
        }
    }

    fn compare_types(expected: &Type, actual: &Type) -> Result<(), TypeError> {
        if actual == expected {
            Ok(())
        } else {
            Err(TypeError::NotAsExpected(expected.clone(), actual.clone()))
        }
    }
}

pub fn example_constant() -> Expression {
    Expression::Constant(Constant::Int(5))
}
