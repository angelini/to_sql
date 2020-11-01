use std::collections::{BTreeMap, HashMap};
use std::fmt;

use crate::ast::{Ast, Expression};
use crate::base::{self, ColumnName, Constant, Identifier, Scope};
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
    ConstantColumn(Type, Vec<Constant>),
    Table(Table),
    Function(Vec<Identifier>, Expression),
    NativeFunction(fn(Vec<Value>) -> Value),
}

#[derive(Clone, Debug)]
pub enum ExecutionError {
    MissingIdentifier(Identifier),
    NotAFunction(Identifier),
    WrongNumberOfArguments(Identifier, usize, usize),
}

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecutionError::MissingIdentifier(ident) => write!(f, "missing identifier: {}", ident),
            ExecutionError::NotAFunction(ident) => write!(f, "not a function: {}", ident),
            ExecutionError::WrongNumberOfArguments(ident, expected, actual) => write!(
                f,
                "wrong number of arguments for: {}, expected: {}, found: {}",
                ident, expected, actual
            ),
        }
    }
}

type Result<T> = std::result::Result<T, ExecutionError>;

fn execute(scope: &Scope<Value>, expression: Expression) -> Result<Value> {
    match expression {
        Expression::Constant(constant) => Ok(Value::Primitive(constant)),
        Expression::Variable(ident) => scope
            .get(&ident)
            .cloned()
            .ok_or(ExecutionError::MissingIdentifier(ident)),
        Expression::Row(row_expressions) => Ok(Value::Row(
            row_expressions
                .into_iter()
                .map(|(column_name, expr)| Ok((column_name, execute(scope, expr)?)))
                .collect::<Result<BTreeMap<ColumnName, Value>>>()?,
        )),
        Expression::Application(ident, arguments) => {
            let (arg_names, func_body) = match scope.get(&ident) {
                Some(Value::Function(arg_names, expr)) => (arg_names, expr),
                Some(_) => return Err(ExecutionError::NotAFunction(ident)),
                _ => return Err(ExecutionError::MissingIdentifier(ident)),
            };

            if arg_names.len() != arguments.len() {
                return Err(ExecutionError::WrongNumberOfArguments(
                    ident,
                    arg_names.len(),
                    arguments.len(),
                ));
            }

            let arg_values = arg_names
                .into_iter()
                .zip(arguments.into_iter())
                .map(|(name, expr)| Ok((name.clone(), execute(scope, expr)?)))
                .collect::<Result<HashMap<Identifier, Value>>>()?;

            execute(&scope.descend(arg_values), func_body.clone())
        }
        Expression::Block(expressions, last_expr) => {
            let mut nested_scope = scope.descend(HashMap::new());

            for (ident, _, expr) in expressions {
                let value = execute(&nested_scope, expr)?;
                let mut assignments = HashMap::new();
                assignments.insert(ident, value);
                nested_scope = scope.descend(assignments);
            }

            execute(&nested_scope, *last_expr)
        }
        Expression::Function(idents, expr) => Ok(Value::Function(idents, *expr)),
    }
}

fn native_true(arguments: Vec<Value>) -> Value {
    Value::Primitive(Constant::Bool(true))
}

fn native_as_const(mut arguments: Vec<Value>) -> Value {
    let constant = match arguments.pop() {
        Some(Value::Primitive(c)) => c,
        _ => unreachable!()
    };
    Value::ConstantColumn(Type::from_constant(&constant), vec![constant])
}

fn std_column_functions<'a>(scope: &mut Scope<'a, Value>) {
    scope.insert(base::ident("as_const"), Value::NativeFunction(native_as_const));
}

pub fn run(ast: Ast) -> Result<Value> {
    let mut scope = Scope::root();

    std_column_functions(&mut scope);

    for (ident, expression) in ast.expressions {
        let value = execute(&scope, expression)?;
        scope.insert(ident, value);
    }

    Ok(scope.remove(&base::ident("main")).unwrap())
}
