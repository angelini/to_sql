use std::collections::BTreeMap;
use std::fmt;

use crate::base::{ColumnName, Constant, Identifier, Kind, Kinds};
use crate::parser::{ParseError, Token};
use crate::types::{self, Column, Primitive, Row, Type, TypeContext, TypeError};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Constant(Constant),
    Variable(Identifier),
    Row(BTreeMap<ColumnName, Expression>),
    Application(Identifier, Vec<Expression>),
    Block(Vec<(Identifier, Type, Expression)>, Box<Expression>),
    Function(Vec<Identifier>, Box<Expression>),
}

impl Expression {
    pub fn check_type(
        &self,
        ctx: &TypeContext,
        expected: &Type,
    ) -> std::result::Result<(), TypeError> {
        match (expected, self) {
            (Type::Value(_), Expression::Constant(constant)) => {
                Self::compare_types(expected, &Type::from_constant(constant))
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
                    row_type.iter().zip(row_value.iter())
                {
                    if exp_name != act_name {
                        return Err(TypeError::MistmatchRow(
                            row_type.clone(),
                            format!("{:?}", row_value),
                        ));
                    }
                    act_expr.check_type(ctx, &Type::Column(exp_type.clone()))?
                }

                Ok(())
            }
            (Type::Function(arg_types, body_type), Expression::Function(arg_idents, body)) => {
                let mut nested_ctx = ctx.clone();

                if arg_types.len() != arg_idents.len() {
                    return Err(TypeError::MistmatchArgumentCount(
                        arg_types.len(),
                        arg_idents.len(),
                    ));
                }

                for (idx, arg_ident) in arg_idents.iter().enumerate() {
                    nested_ctx.add(arg_ident.clone(), Kinds::empty(), arg_types[idx].clone())
                }
                body.check_type(&nested_ctx, body_type)
            }

            (_, Expression::Variable(ident)) => Self::compare_types(expected, ctx.get(ident)?),
            (_, Expression::Application(ident, arg_expressions)) => {
                if let Type::Function(expected_args, actual) = ctx.get(ident)? {
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
                // FIXME: cloning the entire type context
                let mut nested_ctx = ctx.clone();
                for (ident, typ, expr) in assignments {
                    expr.check_type(&nested_ctx, typ)?;
                    nested_ctx.add(ident.clone(), Kinds::empty(), typ.clone());
                }
                last_expression.check_type(&nested_ctx, expected)
            }
            (_, _) => Err(TypeError::UnexpectedType(
                expected.clone(),
                format!("{:?}", self),
            )),
        }
    }

    fn compare_types(expected: &Type, actual: &Type) -> std::result::Result<(), TypeError> {
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

pub type Expressions = BTreeMap<Identifier, Expression>;

#[derive(Debug)]
pub enum InputError {
    Parse(ParseError),
    InvalidBlockAssignment(Token),
    Type(TypeError),
}

impl From<ParseError> for InputError {
    fn from(error: ParseError) -> Self {
        InputError::Parse(error)
    }
}

impl From<TypeError> for InputError {
    fn from(error: TypeError) -> Self {
        InputError::Type(error)
    }
}

impl fmt::Display for InputError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InputError::Parse(parse_error) => write!(f, "error parsing: '''\n{}\n'''", parse_error),
            InputError::InvalidBlockAssignment(token) => {
                write!(f, "invalid block assignment: {:?}", token)
            }
            InputError::Type(type_error) => write!(f, "type error: {}", type_error),
        }
    }
}

type Result<T> = std::result::Result<T, InputError>;

fn token_to_type(type_ctx: &TypeContext, kinds: &Kinds, token: Token) -> Result<Type> {
    Ok(match token {
        Token::TypeName(name) => match kinds.get(&name) {
            Some(Kind::Primitive) => Type::Value(Primitive::Unknown(name, false)),
            Some(Kind::Row) => Type::Row(Row::Unknown(name)),
            None => type_ctx.lookup_alias(&name)?.clone(),
        },
        Token::GenericTypeName(mut names) => {
            let root = names.remove(0);
            let nested = if names.len() > 1 {
                Token::GenericTypeName(names)
            } else {
                Token::TypeName(names.pop().unwrap())
            };

            match (root.as_str(), token_to_type(type_ctx, kinds, nested)?) {
                ("Col", Type::Value(Primitive::Known(base))) => Type::Column(Column::Known(base)),
                ("Col", Type::Value(Primitive::Unknown(name, is_null))) => {
                    Type::Column(Column::Unknown(name, is_null))
                }
                ("Table", Type::Row(row)) => Type::Table(row),
                _ => unimplemented!(),
            }
        }
        Token::Union(variants) => {
            let variant_types = variants
                .into_iter()
                .map(|variant| Ok(token_to_type(type_ctx, kinds, variant)?))
                .collect::<Result<Vec<Type>>>()?;
            Type::Union(variant_types)
        }
        Token::RowType(row_tokens) => {
            let row_types = row_tokens
                .into_iter()
                .map(|(col_name, type_name)| {
                    Ok((col_name, type_ctx.lookup_alias(&type_name)?.clone()))
                })
                .collect::<Result<BTreeMap<ColumnName, Type>>>()?;
            Type::Row(Row::from_value_types(row_types))
        }
        Token::FunctionType(argument_tokens, body_token) => {
            let argument_types = argument_tokens
                .into_iter()
                .map(|arg| Ok(token_to_type(type_ctx, kinds, arg)?))
                .collect::<Result<Vec<Type>>>()?;
            Type::Function(
                argument_types,
                Box::new(token_to_type(type_ctx, kinds, *body_token)?),
            )
        }
        _ => unimplemented!(),
    })
}

fn token_to_expression(type_ctx: &TypeContext, kinds: &Kinds, token: Token) -> Result<Expression> {
    Ok(match token {
        Token::Constant(constant) => Expression::Constant(constant),
        Token::Identifier(ident) => Expression::Variable(ident),
        Token::Application(ident, arguments) => Expression::Application(
            ident,
            arguments
                .into_iter()
                .map(|arg| Ok(token_to_expression(type_ctx, kinds, arg)?))
                .collect::<Result<Vec<Expression>>>()?,
        ),
        Token::RowValue(row_tokens) => {
            let row_expressions = row_tokens
                .into_iter()
                .map(|(col_name, expr_token)| {
                    Ok((col_name, token_to_expression(type_ctx, kinds, expr_token)?))
                })
                .collect::<Result<BTreeMap<ColumnName, Expression>>>()?;
            Expression::Row(row_expressions)
        }
        Token::Block(mut expressions) => {
            let last = token_to_expression(type_ctx, kinds, expressions.pop().unwrap())?;
            let assigments = expressions
                .into_iter()
                .map(|expr| match expr {
                    Token::Let(ident, type_name, body) => Ok((
                        ident,
                        token_to_type(type_ctx, kinds, Token::TypeName(type_name))?,
                        token_to_expression(type_ctx, kinds, *body)?,
                    )),
                    _ => return Err(InputError::InvalidBlockAssignment(expr)),
                })
                .collect::<Result<Vec<(Identifier, Type, Expression)>>>()?;

            Expression::Block(assigments, Box::new(last))
        }
        Token::Function(arguments, body) => Expression::Function(
            arguments,
            Box::new(token_to_expression(type_ctx, kinds, *body)?),
        ),
        _ => unimplemented!(),
    })
}

#[derive(Debug)]
pub struct Ast {
    pub type_ctx: TypeContext,
    pub expressions: Expressions,
}

impl Ast {
    pub fn from_tokens(tokens: Vec<Token>) -> Result<Self> {
        let mut type_ctx = types::std();
        let mut expressions = Expressions::new();

        for token in tokens {
            match token {
                Token::TypeAssignment(ident, type_token) => type_ctx.add(
                    ident,
                    Kinds::empty(),
                    token_to_type(&type_ctx, &Kinds::empty(), *type_token)?,
                ),
                Token::GenericTypeAssignment(ident, kinds, type_token) => {
                    let typ = token_to_type(&type_ctx, &kinds, *type_token)?;
                    type_ctx.add(ident, kinds, typ);
                }
                Token::TypeAlias(new_name, type_token) => {
                    type_ctx.alias(
                        new_name,
                        token_to_type(&type_ctx, &Kinds::empty(), *type_token)?,
                    );
                }
                Token::Assignment(ident, expression_token) => {
                    let expression = token_to_expression(&type_ctx, &Kinds::empty(), *expression_token)?;
                    expression.check_type(&type_ctx, type_ctx.get(&ident)?)?;
                    expressions.insert(ident, expression);
                }
                _ => unreachable!(),
            }
        }

        Ok(Ast {
            type_ctx,
            expressions,
        })
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.type_ctx)?;
        writeln!(f, "Implementations:")?;
        for (id, expression) in &self.expressions {
            writeln!(f, "  {}: {:?}", id, expression)?;
        }
        write!(f, "")
    }
}
