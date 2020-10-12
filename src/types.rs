use std::collections::{BTreeMap, HashMap};
use std::fmt;

use crate::base;
use crate::base::{ColumnName, Identifier, Kind, Kinds, TypeName};

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Primitive {
    Known(Base),
    Unknown(TypeName, bool),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Column {
    Known(Base),
    Unknown(TypeName, bool),
}

impl Column {
    fn captures(&self, other: &Self) -> bool {
        match (self, other) {
            (Column::Known(expected), Column::Known(actual)) => expected.captures(actual),
            (Column::Unknown(_, _), _) => true,
            _ => unimplemented!()
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RowSchema(BTreeMap<ColumnName, Column>);

impl RowSchema {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::collections::btree_map::Iter<ColumnName, Column> {
        self.0.iter()
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
    Unknown(TypeName),
}

impl Row {
    pub fn from_value_types(types: BTreeMap<ColumnName, Type>) -> Self {
        Row::Known(RowSchema(
            types
                .into_iter()
                .map(|(name, typ)| {
                    let base = match typ {
                        Type::Value(Primitive::Known(base)) => Column::Known(base),
                        Type::Value(Primitive::Unknown(name, null)) => Column::Unknown(name, null),
                        _ => unimplemented!(),
                    };
                    (name, base)
                })
                .collect(),
        ))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Value(Primitive),
    Column(Column),
    Row(Row),
    Table(Row),
    GroupedTable(Row, Row),
    Function(Vec<Type>, Box<Type>),
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
            (Type::Union(variants), _) => {
                for variant in variants {
                    if variant.captures(actual) {
                        return true;
                    }
                }
                false
            }
            (Type::Value(Primitive::Unknown(_, _)), Type::Value(Primitive::Known(_))) => true,
            (Type::Column(Column::Unknown(_, _)), Type::Column(Column::Known(_))) => true,
            _ => {
                dbg!(self);
                dbg!(actual);
                unimplemented!()
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    MistmatchArgumentCount(usize, usize),
    MistmatchRow(RowSchema, String),
    MissingAlias(TypeName),
    MissingAssignment(Identifier),
    NotAFunction(Identifier),
    UnexpectedType(Type, String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::MistmatchArgumentCount(expected, actual) => write!(
                f,
                "expected {} function arguments, has {}",
                expected, actual
            ),
            TypeError::MistmatchRow(expected, actual) => {
                write!(f, "expected: {:?}, actual: {}", expected, actual)
            }
            TypeError::MissingAlias(name) => write!(f, "missing type alias for: {}", name),
            TypeError::MissingAssignment(ident) => {
                write!(f, "missing type assignment for: {}", ident)
            }
            TypeError::NotAFunction(ident) => {
                write!(f, "cannot call {}, it is not a function", ident)
            }
            TypeError::UnexpectedType(expected, actual) => {
                write!(f, "expected: {:?}, actual: {}", expected, actual)
            }
        }
    }
}

fn value(base: Base) -> Type {
    Type::Value(Primitive::Known(base))
}

fn unknown_value<S: Into<String>>(name: S, nullable: bool) -> Type {
    Type::Value(Primitive::Unknown(base::type_name(name), nullable))
}

fn col(base: Base) -> Type {
    Type::Column(Column::Known(base))
}

fn unknown_col<S: Into<String>>(name: S, nullable: bool) -> Type {
    Type::Column(Column::Unknown(base::type_name(name), nullable))
}

fn unknown_row<S: Into<String>>(name: S) -> Type {
    Type::Row(Row::Unknown(base::type_name(name)))
}

fn func(args: Vec<Type>, ret: Type) -> Type {
    Type::Function(args, Box::new(ret))
}

fn unknown_table<S: Into<String>>(name: S) -> Type {
    Type::Table(Row::Unknown(base::type_name(name)))
}

#[derive(Clone, Debug)]
struct BoundType {
    kinds: Kinds,
    typ: Type,
}

impl BoundType {
    fn new(kinds: Kinds, typ: Type) -> BoundType {
        BoundType {
            kinds: kinds,
            typ: typ,
        }
    }

    fn from_type(typ: Type) -> BoundType {
        BoundType {
            kinds: Kinds::empty(),
            typ: typ,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeContext {
    aliases: HashMap<TypeName, BoundType>,
    assignments: HashMap<Identifier, BoundType>,
}

impl TypeContext {
    pub fn new() -> TypeContext {
        TypeContext {
            aliases: HashMap::new(),
            assignments: HashMap::new(),
        }
    }

    pub fn alias(&mut self, name: TypeName, typ: Type) {
        self.aliases.insert(name, BoundType::from_type(typ));
    }

    pub fn lookup_alias(&self, name: &TypeName) -> Result<&Type, TypeError> {
        self.aliases
            .get(name)
            .map(|bound| &bound.typ)
            .ok_or_else(|| TypeError::MissingAlias(name.clone()))
    }

    pub fn add(&mut self, id: Identifier, kinds: Kinds, typ: Type) {
        self.assignments.insert(id, BoundType::new(kinds, typ));
    }

    pub fn get(&self, id: &Identifier) -> Result<&Type, TypeError> {
        self.assignments
            .get(id)
            .map(|bound| &bound.typ)
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

    ctx.add(
        base::ident("col"),
        base::kinds(vec![("P", Kind::Primitive)]),
        func(vec![unknown_value("P", false)], unknown_col("P", false)),
    );

    // sum :: N : Int | Float :: Col<N?> -> Value<N>
    // FIXME: Support unions

    // avg :: N : Int | Float :: Col<N?> -> Value<N>
    // FIXME: Support unions

    // default :: P : Primitive :: Col<P?>, P -> Col<P>
    ctx.add(
        base::ident("default"),
        base::kinds(vec![("P", Kind::Primitive)]),
        func(
            vec![unknown_col("P", true), unknown_value("P", false)],
            col(Base::Int(false)),
        ),
    );
}

fn std_table_functions(ctx: &mut TypeContext) {
    // select :: R1, R2 : Row :: Table<R1>, (R1 -> R2) -> Table<R2>
    ctx.add(
        base::ident("select"),
        base::kinds(vec![("R1", Kind::Row), ("R2", Kind::Row)]),
        func(
            vec![func(vec![unknown_row("R1")], unknown_row("R2"))],
            unknown_table("R2"),
        ),
    );

    // filter :: R : Row :: Table<R>, (R -> Col<Bool>) -> Table<R>
    ctx.add(
        base::ident("filter"),
        base::kinds(vec![("R", Kind::Row)]),
        func(
            vec![func(vec![unknown_row("R")], col(Base::Bool(false)))],
            unknown_table("R"),
        ),
    )
}

fn std_infix_functions(ctx: &mut TypeContext) {
    for cmp in &["__eq__", "__ne__", "__gte__", "__lte__", "__gt__", "__lt__"] {
        ctx.add(
            base::ident(*cmp),
            base::kinds(vec![("P", Kind::Primitive)]),
            func(
                vec![unknown_value("P", true), unknown_value("P", true)],
                value(Base::Bool(false)),
            ),
        )
    }
}

pub fn std() -> TypeContext {
    let mut ctx = TypeContext::new();
    std_base_types(&mut ctx);
    std_column_functions(&mut ctx);
    std_table_functions(&mut ctx);
    std_infix_functions(&mut ctx);
    ctx
}
