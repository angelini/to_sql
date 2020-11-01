use crate::base::{self, Kind};
use crate::types::{Base, Column, Primitive, Row, Type, TypeContext};

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

fn base_types(ctx: &mut TypeContext) {
    ctx.alias(base::type_name("Bool"), value(Base::Bool(false)));
    ctx.alias(base::type_name("Int"), value(Base::Int(false)));
    ctx.alias(base::type_name("Float"), value(Base::Float(false)));
    ctx.alias(base::type_name("String"), value(Base::String(false)));
    ctx.alias(base::type_name("Date"), value(Base::Date(false)));
}

fn column_functions(ctx: &mut TypeContext) {
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

fn table_functions(ctx: &mut TypeContext) {
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

fn infix_functions(ctx: &mut TypeContext) {
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

pub fn type_context() -> TypeContext {
    let mut ctx = TypeContext::new();
    base_types(&mut ctx);
    column_functions(&mut ctx);
    table_functions(&mut ctx);
    infix_functions(&mut ctx);
    ctx
}
