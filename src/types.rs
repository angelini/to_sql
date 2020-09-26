use std::collections::{BTreeMap, HashMap};
use std::fmt;

use crate::base;
use crate::base::{ColumnName, Identifier, TypeName};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Base {
    Bool(bool),
    Int(bool),
    Float(bool),
    String(bool),
    Date(bool),
}

impl Base {
    fn captures(&self, other: &Self) -> bool {
        match (self, other) {
            (Base::Bool(expected), Base::Bool(actual))
            | (Base::Int(expected), Base::Int(actual))
            | (Base::Float(expected), Base::Float(actual))
            | (Base::String(expected), Base::String(actual))
            | (Base::Date(expected), Base::Date(actual)) => *expected || !actual,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Kind {
    Primitive,
    Column,
    Row,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive {
    Known(Base),
    Unknown(usize, bool),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Column {
    Known(Base),
    Unknown(usize, bool),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RowSchema(BTreeMap<String, Base>);

impl RowSchema {
    fn from_vec<S: Into<String>>(columns: Vec<(S, Base)>) -> RowSchema {
        RowSchema(columns.into_iter().map(|(k, v)| (k.into(), v)).collect())
    }

    fn captures(&self, other: &Self) -> bool {
        for (col_name, expected) in &self.0 {
            match other.0.get(col_name) {
                Some(actual) => {
                    if !expected.captures(actual) {
                        return false;
                    }
                }
                None => return false,
            }
        }
        true
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Row {
    Known(RowSchema),
    Unknown(usize),
}

impl Row {
    pub fn from_value_types(types: BTreeMap<ColumnName, Type>) -> Row {
        unimplemented!()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

impl Type {
    pub fn captures(&self, actual: &Self) -> bool {
        match (self, actual) {
            (Type::Value(Primitive::Known(expected)), Type::Value(Primitive::Known(actual)))
            | (Type::Column(Column::Known(expected)), Type::Column(Column::Known(actual))) => {
                expected.captures(actual)
            }
            (Type::Row(Row::Known(expected)), Type::Row(Row::Known(actual)))
            | (Type::Table(Row::Known(expected)), Type::Table(Row::Known(actual))) => {
                expected.captures(actual)
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    DidNotExpectFunction(Type),
    MistmatchArgumentCount(usize, usize),
    MissingAlias(TypeName),
    MissingAssignment(Identifier),
    NotAFunction(Identifier),
    NotAsExpected(Type, Type),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::DidNotExpectFunction(expected) => {
                write!(f, "expected {:?}, not a function", expected)
            }
            TypeError::MistmatchArgumentCount(expected, actual) => write!(
                f,
                "expected {} function arguments, has {}",
                expected, actual
            ),
            TypeError::MissingAlias(name) => write!(f, "missing type alias for: {}", name),
            TypeError::MissingAssignment(ident) => {
                write!(f, "missing type assignment for: {}", ident)
            }
            TypeError::NotAFunction(ident) => {
                write!(f, "cannot call {}, it is not a function", ident)
            }
            TypeError::NotAsExpected(expected, actual) => {
                write!(f, "expected: {:?}, actual: {:?}", expected, actual)
            }
        }
    }
}

fn value(base: Base) -> Type {
    Type::Value(Primitive::Known(base))
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
    Type::Row(Row::Known(RowSchema::from_vec(columns)))
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

#[derive(Clone, Debug)]
pub struct TypeContext {
    aliases: HashMap<TypeName, Type>,
    assignments: HashMap<Identifier, Type>,
}

impl TypeContext {
    pub fn new() -> TypeContext {
        TypeContext {
            aliases: HashMap::new(),
            assignments: HashMap::new(),
        }
    }

    pub fn alias(&mut self, name: TypeName, typ: Type) {
        self.aliases.insert(name, typ);
    }

    pub fn lookup_alias(&self, name: &TypeName) -> Result<&Type, TypeError> {
        self.aliases
            .get(name)
            .ok_or_else(|| TypeError::MissingAlias(name.clone()))
    }

    pub fn add(&mut self, id: Identifier, typ: Type) {
        self.assignments.insert(id, typ);
    }

    pub fn get(&self, id: &Identifier) -> Result<&Type, TypeError> {
        self.assignments
            .get(id)
            .ok_or_else(|| TypeError::MissingAssignment(id.clone()))
    }
}

impl fmt::Display for TypeContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Type Context:")?;
        for (name, typ) in &self.aliases {
            writeln!(f, "  {}: {:?}", name, typ)?;
        }
        for (id, typ) in &self.assignments {
            writeln!(f, "  {}: {:?}", id, typ)?;
        }
        write!(f, "")
    }
}

fn std_base_types(ctx: &mut TypeContext) {
    ctx.alias(base::type_name("Bool"), value(Base::Bool(false)));
    ctx.alias(base::type_name("Int"), value(Base::Int(false)));
    ctx.alias(base::type_name("Float"), value(Base::Float(false)));
    ctx.alias(base::type_name("String"), value(Base::String(false)));
    ctx.alias(base::type_name("Date"), value(Base::Date(false)));
}

fn std_column_functions(ctx: &mut TypeContext) {
    // col :: P : Primitive :: P -> Col<P>
    {
        ctx.add(
            base::ident("col"),
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
            base::ident("default"),
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
            base::ident("select"),
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
            base::ident("filter"),
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
    std_base_types(&mut ctx);
    std_column_functions(&mut ctx);
    std_table_functions(&mut ctx);
    ctx
}
