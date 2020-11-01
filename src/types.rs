use log::debug;
use std::collections::{BTreeMap, HashMap};
use std::fmt;

use crate::base::{ColumnName, Constant, Identifier, Kinds, TypeName};

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
            _ => unimplemented!(),
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
    pub fn from_constant(value: &Constant) -> Self {
        let base = match value {
            Constant::Bool(_) => Base::Bool(false),
            Constant::Int(_) => Base::Int(false),
            Constant::Float(_) => Base::Float(false),
            Constant::String(_) => Base::String(false),
            Constant::Date(_) => Base::Date(false),
        };
        Type::Value(Primitive::Known(base))
    }

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
            (Type::Value(Primitive::Unknown(_, _)), Type::Value(_)) => true,
            (Type::Column(Column::Unknown(_, _)), Type::Column(_)) => true,
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

#[derive(Clone, Debug)]
struct BoundType {
    kinds: Kinds,
    typ: Type,
}

impl BoundType {
    fn new(kinds: Kinds, typ: Type) -> BoundType {
        BoundType { kinds, typ }
    }

    fn from_type(typ: Type) -> BoundType {
        BoundType {
            typ,
            kinds: Kinds::empty(),
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

#[derive(Default, Debug)]
pub struct RefinementContext {
    primitives: HashMap<TypeName, Base>,
    rows: HashMap<TypeName, RowSchema>,
}

impl RefinementContext {
    pub fn new() -> Self {
        RefinementContext::default()
    }

    pub fn add(&mut self, expected: &Type, refined: Type) {
        match refined {
            Type::Value(Primitive::Known(primitive)) | Type::Column(Column::Known(primitive)) => {
                match expected {
                    Type::Value(Primitive::Unknown(name, _))
                    | Type::Column(Column::Unknown(name, _)) => {
                        self.primitives.insert(name.clone(), primitive);
                    }
                    _ => {}
                }
            }
            Type::Row(Row::Known(row_schema)) => match expected {
                Type::Row(Row::Unknown(name)) => {
                    self.rows.insert(name.clone(), row_schema);
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn refine(&self, expected: &Type) -> Type {
        debug!("[refine] {:?} within the context {:?}", expected, self);
        let refined_type = match expected {
            Type::Value(Primitive::Unknown(name, _)) => self
                .primitives
                .get(name)
                .map(|p| Type::Value(Primitive::Known(*p))),
            Type::Column(Column::Unknown(name, _)) => self
                .primitives
                .get(name)
                .map(|p| Type::Column(Column::Known(*p))),
            Type::Row(Row::Unknown(name)) => self
                .rows
                .get(name)
                .map(|r| Type::Row(Row::Known(r.clone()))),
            _ => None,
        };
        refined_type.unwrap_or_else(|| expected.clone())
    }
}
