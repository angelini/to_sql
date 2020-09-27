use std::collections::BTreeMap;

use chrono::naive::NaiveDate;
use rust_decimal::Decimal;

use crate::base::{ColumnName, Identifier};
use crate::types::{Base, Column, Primitive, Row, Type, TypeContext, TypeError};

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

pub type RowValue = BTreeMap<ColumnName, Expression>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Constant(Constant),
    Variable(Identifier),
    Row(RowValue),
    Assignment(Identifier, Box<Expression>),
    Application(Identifier, Vec<Expression>),
    Block(Vec<Expression>, Box<Expression>),
    Function(Vec<Identifier>, Box<Expression>),
}

impl Expression {
    pub fn check_type(&self, ctx: &TypeContext, expected: &Type) -> Result<(), TypeError> {
        match (expected, self) {
            (Type::Value(_), Expression::Constant(constant)) => {
                Self::compare_types(expected, &constant.get_type())
            }
            (Type::Union(variants), _) => {
                for variant in variants {
                    if self.check_type(ctx, variant).is_ok() {
                        return Ok(());
                    }
                }
                Err(TypeError::UnexpectedType(
                    expected.clone(),
                    format!("{:?}", self),
                ))
            }
            (Type::Row(Row::Known(row_type)), Expression::Row(row_value)) => {
                if row_type.len() != row_value.len() {
                    return Err(TypeError::MistmatchRow(
                        row_type.clone(),
                        format!("{:?}", row_value),
                    ));
                }

                for ((exp_name, exp_type), (act_name, act_expr)) in
                    row_type.iter().zip(row_value.into_iter())
                {
                    if exp_name != act_name {
                        return Err(TypeError::MistmatchRow(
                            row_type.clone(),
                            format!("{:?}", row_value),
                        ));
                    }
                    act_expr.check_type(ctx, &Type::Column(Column::Known(exp_type.clone())))?
                }

                Ok(())
            }
            (Type::Function(_, arg_types, body_type), Expression::Function(arg_idents, body)) => {
                let mut nested_ctx = ctx.clone();

                if arg_types.len() != arg_idents.len() {
                    return Err(TypeError::MistmatchArgumentCount(
                        arg_types.len(),
                        arg_idents.len(),
                    ));
                }

                for (idx, arg_ident) in arg_idents.iter().enumerate() {
                    nested_ctx.add(arg_ident.clone(), arg_types[idx].clone())
                }
                body.check_type(&nested_ctx, body_type)
            }

            (_, Expression::Variable(ident)) => Self::compare_types(expected, ctx.get(ident)?),
            (_, Expression::Application(ident, arg_expressions)) => {
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
            (_, Expression::Block(assignments, last_expression)) => {
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
            (_, _) => Err(TypeError::UnexpectedType(
                expected.clone(),
                format!("{:?}", self),
            )),
        }
    }

    fn compare_types(expected: &Type, actual: &Type) -> Result<(), TypeError> {
        if expected.captures(actual) {
            Ok(())
        } else {
            Err(TypeError::UnexpectedType(
                expected.clone(),
                format!("{:?}", actual),
            ))
        }
    }
}
