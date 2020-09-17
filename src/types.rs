use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Eq, PartialEq)]
pub enum Base {
    Bool(bool),
    Int(bool),
    Float(bool),
    String(bool),
    Date(bool),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Kind {
    Primitive,
    Column,
    Row,
}

#[derive(Debug, Eq, PartialEq)]
enum Primitive {
    Known(Base),
    Unknown(usize, bool),
}

#[derive(Debug, Eq, PartialEq)]
enum Column {
    Known(Base),
    Unknown(usize, bool),
}

#[derive(Debug, Eq, PartialEq)]
enum Row {
    Known(BTreeMap<String, Base>),
    Unknown(usize),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    Value(Primitive),
    Column(Column),
    Row(Row),
    Table(Row),
    GroupedTable(Row, Row),
    Function(Vec<Kind>, Vec<Type>, Box<Type>),
    List(Box<Type>),
    Union(Vec<Type>),
}

fn unknown_value(id: usize, nullable: bool) -> Type {
    Type::Value(Primitive::Unknown(id, nullable))
}

fn col(base: Base) -> Type {
    Type::Column(Column::Known(base))
}

fn unknown_col(id: usize, nullable: bool) -> Type {
    Type::Column(Column::Unknown(id, nullable))
}

fn row<S: Into<String>>(columns: Vec<(S, Base)>) -> Type {
    Type::Row(Row::Known(
        columns.into_iter().map(|(k, v)| (k.into(), v)).collect(),
    ))
}

fn unknown_row(id: usize) -> Type {
    Type::Row(Row::Unknown(id))
}

fn func(kinds: Vec<Kind>, args: Vec<Type>, ret: Type) -> Type {
    Type::Function(kinds, args, Box::new(ret))
}

fn unknown_table(id: usize) -> Type {
    Type::Table(Row::Unknown(id))
}

pub struct TypeContext {
    definitions: HashMap<String, Type>,
}

impl TypeContext {
    fn new() -> TypeContext {
        TypeContext {
            definitions: HashMap::new(),
        }
    }

    fn add<S: Into<String>>(&mut self, key: S, typ: Type) {
        self.definitions.insert(key.into(), typ);
    }
}

fn std_column_functions(ctx: &mut TypeContext) {
    // col :: P : Primitive :: P -> Col<P>
    {
        ctx.add(
            "col",
            func(
                vec![Kind::Primitive],
                vec![unknown_value(0, false)],
                unknown_col(0, false),
            ),
        )
    }

    // sum :: N : Int | Float :: Col<N?> -> Value<N>
    // FIXME: Support unions

    // avg :: N : Int | Float :: Col<N?> -> Value<N>
    // FIXME: Support unions

    // default :: P : Primitive :: Col<P?>, P -> Col<P>
    {
        ctx.add(
            "default",
            func(
                vec![Kind::Primitive],
                vec![unknown_col(0, true), unknown_value(0, false)],
                col(Base::Int(false)),
            ),
        )
    }
}

fn std_table_functions(ctx: &mut TypeContext) {
    // select :: R1, R2 : Row :: Table<R1>, (R1 -> R2) -> Table<R2>
    {
        ctx.add(
            "select",
            func(
                vec![Kind::Row, Kind::Row],
                vec![
                    unknown_table(0),
                    func(
                        vec![Kind::Row, Kind::Row],
                        vec![unknown_row(0)],
                        unknown_row(1),
                    ),
                ],
                unknown_table(1),
            ),
        );
    }

    // filter :: R : Row :: Table<R>, (R -> Col<Bool>) -> Table<R>
    {
        ctx.add(
            "filter",
            func(
                vec![Kind::Row],
                vec![
                    unknown_table(0),
                    func(
                        vec![Kind::Row],
                        vec![unknown_row(0)],
                        col(Base::Bool(false)),
                    ),
                ],
                unknown_table(0),
            ),
        )
    }
}

pub fn std() -> TypeContext {
    let mut ctx = TypeContext::new();
    std_column_functions(&mut ctx);
    std_table_functions(&mut ctx);
    ctx
}
