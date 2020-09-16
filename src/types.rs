use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Eq, PartialEq)]
enum Base {
    Bool,
    Int,
    Float,
    String,
    Date,
}

#[derive(Debug, Eq, PartialEq)]
enum Nullable {
    Has(Base),
    None(Base),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Pid(usize);

#[derive(Debug, Eq, PartialEq)]
enum Primitive {
    Known(Base),
    Unknown(Pid),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Rid(usize);

#[derive(Debug, Eq, PartialEq)]
enum RowSchema {
    Known(BTreeMap<String, Nullable>),
    Unknown(Rid),
}

#[derive(Debug, Eq, PartialEq)]
enum Type {
    Constant(Primitive),
    Value(Primitive),
    Column(Primitive),
    Row(RowSchema),
    Table(RowSchema),
    GroupedTable(RowSchema, RowSchema),
    Function(Vec<Type>, Box<Type>),
    List(Box<Type>),
    Union(Vec<Type>),
}

fn null(base: Base) -> Nullable {
    Nullable::Has(base)
}

fn not_null(base: Base) -> Nullable {
    Nullable::None(base)
}

fn constant(base: Base) -> Type {
    Type::Constant(Primitive::Known(not_null(base)))
}

fn unknown_constant(pid: Pid) -> Type {
    Type::Constant(Primitive::Unknown(pid))
}

fn col(base: Base) -> Type {
    Type::Column(Primitive::Known(not_null(base)))
}

fn null_col(base: Base) -> Type {
    Type::Column(Primitive::Known(null(base)))
}

fn unknown_col(pid: Pid) -> Type {
    Type::Constant(Primitive::Unknown(pid))
}

fn schema<S: Into<String>>(columns: Vec<(S, Nullable)>) -> RowSchema {
    RowSchema::Known(columns.into_iter().map(|(k, v)| (k.into(), v)).collect())
}

fn row<S: Into<String>>(columns: Vec<(S, Nullable)>) -> Type {
    Type::Row(schema(columns))
}

fn unknown_row(id: Rid) -> Type {
    Type::Row(RowSchema::Unknown(id))
}

fn func(args: Vec<Type>, ret: Type) -> Type {
    Type::Function(args, Box::new(ret))
}

fn table(row: RowSchema) -> Type {
    Type::Table(row)
}

fn unknown_table(id: Rid) -> Type {
    Type::Table(RowSchema::Unknown(id))
}

pub struct TypeContext {
    definitions: HashMap<String, Type>,
    next_pid: Pid,
    next_rid: Rid,
}

impl TypeContext {
    fn new() -> TypeContext {
        TypeContext {
            definitions: HashMap::new(),
            next_pid: Pid(1),
            next_rid: Rid(1),
        }
    }

    fn get_pid(&mut self) -> Pid {
        let id = self.next_pid;
        self.next_pid = Pid(self.next_pid.0 + 1);
        id
    }

    fn get_rid(&mut self) -> Rid {
        let id = self.next_rid;
        self.next_rid = Rid(self.next_rid.0 + 1);
        id
    }

    fn add<S: Into<String>>(&mut self, key: S, typ: Type) {
        self.definitions.insert(key.into(), typ);
    }
}

fn std_column_functions(ctx: &mut TypeContext) {
    // col :: P : Primitive :: P? -> Col<P?>
    {
        let p = ctx.get_pid();
        ctx.add(
            "col",
            func(
                vec![unknown_constant(p)],
                unknown_col(p),
            )
        )
    }

    // sum :: N : Int | Float :: Col<N?> -> Value<N>
    // FIXME: Support unions

    // avg :: N : Int | Float :: Col<N?> -> Value<N>
    // FIXME: Support unions

    // default :: P : Primitive :: Col<P?>, P -> Col<P>
    {
        let p = ctx.get_pid();
        ctx.add(
            "default",
            func(
                vec![
                    unknown_col(pid: Pid)
                ],
                col(Base::Int)
            )
        )
}

fn std_table_functions(ctx: &mut TypeContext) {
    // select :: R1, R2 : Row :: Table<R1>, (R1 -> R2) -> Table<R2>
    {
        let (r1, r2) = (ctx.get_rid(), ctx.get_rid());
        ctx.add(
            "select",
            func(
                vec![
                    unknown_table(r1),
                    func(vec![unknown_row(r1)], unknown_row(r2)),
                ],
                unknown_table(r2),
            ),
        );
    }

    // filter :: R : Row :: Table<R>, (R -> Col<Bool>) -> Table<R>
    {
        let r = ctx.get_rid();
        ctx.add(
            "filter",
            func(
                vec![
                    unknown_table(r),
                    func(vec![unknown_row(r)], col(Base::Bool)),
                ],
                unknown_table(r),
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
