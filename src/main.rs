use std::collections::{BTreeMap, HashMap};

mod types;

#[derive(Debug, Eq, PartialEq)]
enum ColumnType {
    Bool,
    Int,
    Float,
    String,
    Date,
}

#[derive(Debug)]
struct TableDefinition {
    exists: bool,
    name: Option<String>,
    schema: BTreeMap<String, ColumnType>,
}

#[derive(Debug, Eq, PartialEq)]
enum BoolOp {
    And,
    Or,
}

#[derive(Debug, Eq, PartialEq)]
enum Cmp {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
}

#[derive(Debug)]
enum SelectExpression {
    Name(String),
    Function(String, Vec<SelectExpression>),
    Cmp(Cmp, Box<(SelectExpression, SelectExpression)>),
    Bool(BoolOp, Box<(SelectExpression, SelectExpression)>),
}

#[derive(Debug)]
enum Filter {
    Cmp(Cmp, Box<(Filter, Filter)>),
    Bool(BoolOp, Box<(Filter, Filter)>),
}

#[derive(Debug)]
struct TableExpression {
    definition: TableDefinition,
    selects: BTreeMap<String, SelectExpression>,
    wheres: Vec<Filter>,
    limit: Option<usize>,
    order: Vec<String>,
}

#[derive(Debug)]
struct Context {
    tables: HashMap<String, TableDefinition>,
}

fn main() {
    types::std();
}
