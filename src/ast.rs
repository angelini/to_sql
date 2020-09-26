use chrono::naive::NaiveDate;
use rust_decimal::Decimal;

use crate::base::Identifier;
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
    Block(Vec<Expression>, Box<Expression>),
    Function(Vec<Identifier>, Box<Expression>),
}

impl Expression {
    pub fn check_type(&self, ctx: &TypeContext, expected: &Type) -> Result<(), TypeError> {
        match self {
            Expression::Constant(constant) => Self::compare_types(expected, &constant.get_type()),
            Expression::Variable(ident) => Self::compare_types(expected, ctx.get(ident)?),
            Expression::Assignment(ident, expression) => {
                expression.check_type(ctx, ctx.get(ident)?)
            }
            Expression::Application(ident, arg_expressions) => {
                if let Type::Function(_, expected_args, actual) = ctx.get(ident)? {
                    for (expression, expected_arg) in
                        arg_expressions.iter().zip(expected_args.iter())
                    {
                        expression.check_type(ctx, expected_arg)?;
                    }
                    Self::compare_types(expected, actual)
                } else {
                    Err(TypeError::NotAFunction(ident.clone()))
                }
            }
            Expression::Block(assignments, last_expression) => {
                let nested_ctx = ctx.clone();
                for assignment in assignments {
                    match assignment {
                        Expression::Assignment(ident, expression) => {
                            expression.check_type(&nested_ctx, nested_ctx.get(ident)?)?
                        }
                        _ => unimplemented!(),
                    }
                }
                last_expression.check_type(&nested_ctx, expected)
            }
            Expression::Function(argument_idents, expression) => {
                let mut nested_ctx = ctx.clone();
                let (argument_types, body_type) = match expected {
                    Type::Function(_, args, body) => (args, body),
                    _ => return Err(TypeError::DidNotExpectFunction(expected.clone())),
                };

                if argument_types.len() != argument_idents.len() {
                    return Err(TypeError::MistmatchArgumentCount(
                        argument_types.len(),
                        argument_idents.len(),
                    ));
                }

                for (idx, arg_ident) in argument_idents.iter().enumerate() {
                    nested_ctx.add(arg_ident.clone(), argument_types[idx].clone())
                }
                expression.check_type(&nested_ctx, body_type)
            }
        }
    }

    fn compare_types(expected: &Type, actual: &Type) -> Result<(), TypeError> {
        if expected.captures(actual) {
            Ok(())
        } else {
            Err(TypeError::NotAsExpected(expected.clone(), actual.clone()))
        }
    }
}
