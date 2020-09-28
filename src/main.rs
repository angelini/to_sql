mod ast;
mod base;
mod parser;
mod sql;
mod types;

use std::collections::BTreeMap;
use std::fmt;

use ast::Expression;
use base::{ColumnName, Identifier};
use parser::Token;
use types::{Column, Primitive, Row, Type, TypeContext, TypeError};

#[derive(Debug)]
struct ParseError(String);

#[derive(Debug)]
enum InputError {
    InvalidRoot(Token),
    Parse(ParseError),
    Type(TypeError),
}

impl fmt::Display for InputError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InputError::InvalidRoot(expression) => {
                write!(f, "invalid root expression: {:?}", expression)
            }
            InputError::Parse(parse_error) => {
                write!(f, "error parsing: '''\n{}\n'''", parse_error.0)
            }
            InputError::Type(type_error) => write!(f, "type error: {}", type_error),
        }
    }
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

type Result<T> = std::result::Result<T, InputError>;

#[derive(Debug)]
struct Runtime {
    types: TypeContext,
    impls: BTreeMap<Identifier, Expression>,
}

impl Runtime {
    fn new() -> Self {
        Runtime {
            types: types::std(),
            impls: BTreeMap::new(),
        }
    }

    fn add_type_token(&mut self, token: Token) -> Result<()> {
        match token {
            Token::TypeAssignment(ident, type_token) => {
                self.types.add(ident, self.as_type(*type_token)?)
            }

            Token::TypeAlias(new_name, type_token) => {
                self.types.alias(new_name, self.as_type(*type_token)?)
            }
            _ => return Err(InputError::InvalidRoot(token)),
        };
        Ok(())
    }

    fn add_expression_token(&mut self, token: Token) -> Result<()> {
        match token {
            Token::Assignment(ident, expression_token) => {
                let expression = self.as_expression(*expression_token)?;
                expression.check_type(&self.types, self.types.get(&ident)?)?;
                self.impls.insert(ident, expression)
            }
            _ => return Err(InputError::InvalidRoot(token)),
        };
        Ok(())
    }

    fn as_type(&self, token: Token) -> Result<Type> {
        Ok(match token {
            Token::TypeName(name) => self.types.lookup_alias(&name)?.clone(),
            Token::GenericTypeName(mut names) => {
                let root = names.remove(0);
                let nested = if names.len() > 1 {
                    Token::GenericTypeName(names)
                } else {
                    Token::TypeName(names.pop().unwrap())
                };

                match (root.as_str(), self.as_type(nested)?) {
                    ("Col", Type::Value(Primitive::Known(base))) => {
                        Type::Column(Column::Known(base))
                    }
                    ("Table", Type::Row(row)) => Type::Table(row),
                    _ => unimplemented!(),
                }
            }
            Token::Union(variants) => {
                let variant_types = variants
                    .into_iter()
                    .map(|variant| Ok(self.as_type(variant)?))
                    .collect::<Result<Vec<Type>>>()?;
                Type::Union(variant_types)
            }
            Token::RowType(row_tokens) => {
                let row_types = row_tokens
                    .into_iter()
                    .map(|(col_name, type_name)| {
                        Ok((col_name, self.types.lookup_alias(&type_name)?.clone()))
                    })
                    .collect::<Result<BTreeMap<ColumnName, Type>>>()?;
                Type::Row(Row::from_value_types(row_types))
            }
            Token::FunctionType(argument_tokens, body_token) => {
                let argument_types = argument_tokens
                    .into_iter()
                    .map(|arg| Ok(self.as_type(arg)?))
                    .collect::<Result<Vec<Type>>>()?;
                Type::Function(vec![], argument_types, Box::new(self.as_type(*body_token)?))
            }
            _ => unimplemented!(),
        })
    }

    fn as_expression(&self, token: Token) -> Result<Expression> {
        Ok(match token {
            Token::Constant(constant) => Expression::Constant(constant),
            Token::Identifier(ident) => Expression::Variable(ident),
            Token::Assignment(ident, body) => {
                Expression::Assignment(ident, Box::new(self.as_expression(*body)?))
            }
            Token::Application(ident, arguments) => Expression::Application(
                ident,
                arguments
                    .into_iter()
                    .map(|arg| Ok(self.as_expression(arg)?))
                    .collect::<Result<Vec<Expression>>>()?,
            ),
            Token::RowValue(row_tokens) => {
                let row_expressions = row_tokens
                    .into_iter()
                    .map(|(col_name, expr_token)| Ok((col_name, self.as_expression(expr_token)?)))
                    .collect::<Result<BTreeMap<ColumnName, Expression>>>()?;
                Expression::Row(row_expressions)
            }
            Token::Block(mut expressions) => {
                let last = self.as_expression(expressions.pop().unwrap())?;
                Expression::Block(
                    expressions
                        .into_iter()
                        .map(|expr| Ok(self.as_expression(expr)?))
                        .collect::<Result<Vec<Expression>>>()?,
                    Box::new(last),
                )
            }
            Token::Function(arguments, body) => {
                Expression::Function(arguments, Box::new(self.as_expression(*body)?))
            }
            _ => unimplemented!(),
        })
    }
}

impl fmt::Display for Runtime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.types)?;
        writeln!(f, "Implementations:")?;
        for (id, expression) in &self.impls {
            writeln!(f, "  {}: {:?}", id, expression)?;
        }
        write!(f, "")
    }
}

fn parse_input(input: &str) -> Result<(Vec<Token>, Vec<Token>)> {
    let mut type_tokens = vec![];
    let mut expression_tokens = vec![];
    let mut remaining = input;

    loop {
        remaining = parser::parse_ws(remaining);

        if remaining.is_empty() {
            return Ok((type_tokens, expression_tokens));
        }

        if let Some((token, rest)) = parser::parse_type(remaining) {
            type_tokens.push(token);
            remaining = rest;
            continue;
        }

        if let Some((token, rest)) = parser::parse_expression(remaining) {
            expression_tokens.push(token);
            remaining = rest;
            continue;
        }

        return Err(InputError::Parse(ParseError(remaining.to_string())));
    }
}

fn main() -> Result<()> {
    for expression_input in &[
        "5",
        "hello_world",
        "true",
        "'true'",
        "b = 4",
        "foo(bar, baz, q)",
        "(foo, bar) -> 1",
        "{a}",
        "{'foo'; a}",
        "{a; 'foo'; 1}",
        "foo.bar",
        "foo.'baz'",
        "4.5",
        "4 == 4",
        "5 >= 3 == 2"
    ] {
        println!("Expression Input: {}", expression_input);
        match parser::parse_expression(expression_input) {
            Some((token, rest)) => {
                println!("Token:            {:?}", token);
                println!("Rest:             '{}'", rest);
            }
            None => println!("Error"),
        };
        println!();
    }

    for type_input in &[
        "{a: B}",
        "Foo",
        "Col<Int>",
        "List<Col<Bool>>",
        "type Foo = Bar",
        "type Foo = {a :B, cd: De}",
        "a :: Int",
        "foo :: (Int, Bool) -> Int",
        "bar :: R : Row :: (Int) -> Table<R>",
        "Foo | Bar",
        "Foo | Bar<Baz> | B",
    ] {
        println!("Type Input: {}", type_input);
        match parser::parse_type(type_input) {
            Some((token, rest)) => {
                println!("Token:      {:?}", token);
                println!("Rest:       '{}'", rest);
            }
            None => println!("Error"),
        };
        println!();
    }

    let input = "
        a :: Int
        a = 5

        foo :: (Int) -> Bool
        foo = (test) -> true

        result :: Bool
        result = foo(1)

        type A = Int
        type Bc = Bool

        colInt :: (Int) -> Col<Int>
        colBool :: (Bool) -> Col<Bool>

        row :: { a: A, 'b c': Bc }
        row = { a: colInt(1), 'b c': colBool(true) }

        union :: Int | Bool
        union = true
    ";

    // FIXME: Requires type checking generic args
    // result :: Bool
    // result = a == 5

    let (type_tokens, expression_tokens) = parse_input(input)?;
    let mut runtime = Runtime::new();

    for type_token in type_tokens {
        runtime.add_type_token(type_token)?;
    }

    for expression_token in expression_tokens {
        runtime.add_expression_token(expression_token)?;
    }

    println!("{}", runtime);

    Ok(())
}
