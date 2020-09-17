use std::collections::HashMap;

mod ast;
mod sql;
mod types;

use ast::{Expression, Identifier, TypeExpression};
use types::Type;

struct Context {
    types: HashMap<Identifier, Type>,
    expressions: HashMap<Identifier, Expression>,
}

impl Context {
    fn new() -> Context {
        Context {
            types: HashMap::new(),
            expressions: HashMap::new(),
        }
    }

    fn add_type(&mut self, ident: Identifier, typ: Type) {
        self.types.insert(ident, typ);
    }

    fn add_expression(&mut self, ident: Identifier, expression: Expression) {
        self.expressions.insert(ident, expression);
    }
}

fn type_check(input: Vec<Expression>) -> Result<Context, String> {
    let mut ctx = Context::new();

    for expression in input {
        match expression {
            Expression::Type(type_expression) => {
                match type_expression {
                    TypeExpression::Alias(ident, typ) => ctx.add_type(ident, typ),
                    TypeExpression::Function(ident, typ) => ctx.add_type(ident, typ),
                    _ => unimplemented!()
                };
                unimplemented!()
            },
            _ => unimplemented!()
        }
    }

    Ok(ctx)
}

fn main() {
    types::std();
}
