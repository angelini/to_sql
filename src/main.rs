mod ast;
mod base;
mod parser;
mod sql;
mod types;

use std::collections::BTreeMap;
use std::fmt;

use ast::Expression;
use base::{ident, Identifier};
use types::{Type, TypeContext, TypeError};

#[derive(Debug)]
enum InputError {
    InvalidRoot(Expression),
    Type(TypeError),
}

impl fmt::Display for InputError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InputError::InvalidRoot(expression) => {
                write!(f, "invalid root expression: {:?}", expression)
            }
            InputError::Type(type_error) => write!(f, "type error: {}", type_error),
        }
    }
}

impl From<TypeError> for InputError {
    fn from(error: TypeError) -> Self {
        InputError::Type(error)
    }
}

#[derive(Debug)]
struct Context {
    types: TypeContext,
    impls: BTreeMap<Identifier, Expression>,
}

impl Context {
    fn new() -> Context {
        Context {
            types: types::std(),
            impls: BTreeMap::new(),
        }
    }

    fn add_type(&mut self, ident: Identifier, typ: Type) {
        self.types.add(ident, typ)
    }

    fn add_impl(&mut self, ident: Identifier, expression: Expression) {
        self.impls.insert(ident, expression);
    }

    fn check(&self, ident: &Identifier, expression: &Expression) -> Result<(), TypeError> {
        expression.check_type(&self.types, self.types.get(ident)?)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.types)?;
        writeln!(f, "Implementations:")?;
        for (id, expression) in &self.impls {
            writeln!(f, "  {}: {:?}", id, expression)?;
        }
        write!(f, "")
    }
}

fn walk_input(input: Vec<Expression>) -> Result<Context, InputError> {
    let mut ctx = Context::new();

    for expression in input {
        match expression {
            Expression::Type(ident, typ) => ctx.add_type(ident, typ),
            Expression::Assignment(ident, expression) => {
                ctx.check(&ident, &expression)?;
                ctx.add_impl(ident, *expression)
            }
            _ => return Err(InputError::InvalidRoot(expression)),
        }
    }

    Ok(ctx)
}

fn main() -> Result<(), InputError> {
    let input = vec![
        Expression::Type(ident("i"), types::example_int()),
        Expression::Assignment(ident("i"), Box::new(ast::example_int())),
        Expression::Type(ident("func"), types::example_func()),
        Expression::Type(ident("value"), types::example_bool()),
        Expression::Assignment(
            ident("value"),
            Box::new(Expression::Application(
                ident("func"),
                vec![Expression::Variable(ident("i"))],
            )),
        ),
        Expression::Type(ident("block"), types::example_int()),
        Expression::Assignment(
            ident("block"),
            Box::new(Expression::Block(
                vec![
                    Expression::Type(ident("a"), types::example_int()),
                    Expression::Assignment(ident("a"), Box::new(ast::example_int())),
                ],
                Box::new(Expression::Variable(ident("a"))),
            )),
        ),
    ];

    let ctx = walk_input(input)?;
    println!("{}", ctx);

    for expression_input in &[
        "5",
        "hello_world",
        "true",
        "'true'",
        "foo(bar, baz, q)",
        "(foo, bar) -> 1",
        "{a}",
        "{'foo'; a}",
        "{a; 'foo'; 1}"
    ] {
        println!("Expression Input: {}", expression_input);
        match parser::parse_expression(expression_input) {
            Some((token, rest)) => {
                println!("Token:            {:?}", token);
                println!("Rest:             '{}'", rest);
            }
            None => println!("Error")
        };
        println!("");
    }

    for expression_input in &[
        "{a: B}",
        "Foo",
        "type Foo = Bar",
        "type Foo = {a :B, cd: De}",
        "foo :: Int, Bool -> Int",
        "bar :: R : Row :: Int -> Table<R>"
    ] {
        println!("Type Input: {}", expression_input);
        match parser::parse_type(expression_input) {
            Some((token, rest)) => {
                println!("Token:      {:?}", token);
                println!("Rest:       '{}'", rest);
            }
            None => println!("Error")
        };
        println!("");
    }

    Ok(())
}
