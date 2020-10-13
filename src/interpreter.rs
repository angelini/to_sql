use std::collections::{BTreeMap, HashMap};
use std::fmt;

use crate::ast::{Constant, Expression};
use crate::base::{self, ColumnName, Identifier};
use crate::types::{RowSchema, Type};

#[derive(Clone, Debug)]
pub struct TableName(String, String);

#[derive(Clone, Debug)]
pub struct Table {
    exists: bool,
    name: Option<String>,
    schema: RowSchema,
    select: BTreeMap<ColumnName, Expression>,
    from: Option<TableName>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Primitive(Constant),
    Row(BTreeMap<ColumnName, Value>),
    Column(TableName, ColumnName, Type),
    Table(Table),
}

#[derive(Clone, Debug)]
struct Scope {
    parent: Option<Box<Scope>>,
    values: HashMap<Identifier, Value>,
}

impl Scope {
    pub fn root() -> Self {
        Scope {
            parent: None,
            values: HashMap::new(),
        }
    }

    fn lookup(&self, ident: &Identifier) -> Option<&Value> {
        self.values.get(ident).or_else(|| match &self.parent {
            Some(parent) => parent.lookup(ident),
            None => None,
        })
    }
}

#[derive(Clone, Debug)]
pub enum ExecutionError {
    MissingIdentifier(Identifier),
}

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecutionError::MissingIdentifier(ident) => write!(f, "missing identifier: {}", ident),
        }
    }
}

type Result<T> = std::result::Result<T, ExecutionError>;

fn execute(scope: &Scope, expression: Expression) -> Result<Value> {
    match expression {
        Expression::Constant(constant) => Ok(Value::Primitive(constant)),
        Expression::Variable(ident) => scope
            .lookup(&ident)
            .cloned()
            .ok_or(ExecutionError::MissingIdentifier(ident)),
        Expression::Row(row_expressions) => Ok(Value::Row(
            row_expressions
                .into_iter()
                .map(|(column_name, expr)| Ok((column_name, execute(scope, expr)?)))
                .collect::<Result<BTreeMap<ColumnName, Value>>>()?,
        )),
        _ => unimplemented!(),
    }
}

pub fn run(expressions: BTreeMap<Identifier, Expression>) -> Result<Value> {
    let mut scope = Scope::root();

    for (ident, expression) in expressions {
        let value = execute(&scope, expression)?;
        scope.values.insert(ident, value);
    }

    Ok(scope.values.remove(&base::ident("main")).unwrap())
}
