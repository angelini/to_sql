# To SQL

## Example

Given a `.tsql` and all required positional arguments, `build` will return valid SQL

```
$ tosql build example.tsql 5 "dev"

SELECT user, score
FROM scores
WHERE id > 5 AND profile = 'dev'
```

## Types

### Aliases

Any type can be aliased to any capitalized name

```
type Integer = Int
type Strings = List<String>
type BoolCol = Col<Bool>
```

### Unions

```
type Union = Left | Right
```

### Primitives

```
type Primitive = Bool | Int | Float | String | Date
```

### Rows

```
{
    <column name>: <column type>,
    ...
}
```

Anonymous type definition: `{ id: Int, value: String }`

Named type definition: `type Example = { id: Int, value: String }`

Value: `{ id: row.id, value: capitalize(row.value) }`

### Annotations

Named functions

```
is_longer :: Col<String>, Int -> Col<Bool>
```

Function with generic type classes:

```
filter :: R : Row :: Table<R>, (R -> Col<Bool>) -> Table<R>
```

Multiple occurences of the same type class:

```
select :: R1, R2 : Row :: Table<R1>, (R1 -> R2) -> Table<R2>
```

### Type classes

- `P : Primitive :: Value<P?>`
- `P : Primitive :: Col<P?>`
- `R : Row :: Table<R>`
- `R1, R2 : Row :: GroupedTable<R1, R2>`
- `List<T>`

### Nullable

The `?` suffix is used to indicate a nullable `Primitive` value.

Nullable column: `Col<Int?>`

Non-nullable column: `Col<Int>`

### Functions

Type: `Col<String>, Int -> Col<Bool>`

Single expression value: `(col, max) -> len(col) > max`

Multi expression value:

```
(max, value) -> {
    let length = len(value);
    length > max
}
```

Named value: `is_longer = (col, max) -> len(col) > max`

-- FIXME: ambiguous
```
example(left, right -> len(right))
example((left, right) -> len(right))
example(left, (right) -> len(right))
```

## Column Names

Quotes are not necessary for column names:

```
type Example = { id: Int, value: String }
example :: Example
```

Row access: `example.id` or `example.'id'`

But they can be used to support names with spaces:

```
type Other = { 'with space': Int }
other :: Other
```

Row access: `other.'with space'`

## Standard Library

### Table functions

`select :: R1, R2 : Row :: Table<R1>, (R1 -> R2) -> Table<R2>`

`filter :: R : Row :: Table<R>, (R -> Col<Bool>) -> Table<R>`

`group :: R1, R2, R3 : Row :: Table<R1>, (R1 -> R2) -> GroupedTable<R2, R3>`

`agg :: R1, R2, R3, R4 : Row :: GroupedTable<R1, R2>, (R2 -> R3) -> Table<R4>`

`limit :: R : Row :: Table<R>, Int -> Table<R>`

`order :: R : Row :: Table<R>, List<String> -> Table<R>`

`join :: R1, R2, R3 : Row :: Table<R1>, Table<R2>, (R1, R2 -> Col<Bool>) -> Table<R3>`

`union :: R : Row :: Table<R>, Table<R> -> Table<R>`

### Column functions

`col :: P : Primitive :: P? -> Col<P?>`

`sum :: N : Int | Float :: Col<N?> -> Value<N>`

`avg :: N : Int | Float :: Col<N?> -> Value<N>`

`default :: P : Primitive :: Col<P?>, P -> Col<P>`

`not :: Col<Bool> -> Col<Bool>`

`len :: Col<String> -> Col<Int>`

`date_part :: Col<Date>, String -> Col<Int>`

`capitalize :: Col<String> -> Col<String>`

`is_null :: P : Primitive :: Col<P?> -> Col<Bool>`

`first :: P : Primitive :: Col<P?> -> Value<P?>`

### Infix functions

`cmp :: P : Primitive :: Value<P?>, Value<P?> -> Col<Bool>`

Where `cmp` is any of: `==, !=, >=, <=, >, <`

`op :: Col<Bool>, Col<Bool> -> Col<Bool>`

Where `op` is any of: `&&, ||`

## Application

Call function: `select(example, r -> row.value)`

Pipe operator:

```
example
    |> filter(r -> row.id > 10)
    |> select(r -> row.value)
```

## Case Statements

```
case table { r ->
    r.value == 4: col(true),
    _: col(false),
}
```

## Comments

Rest of the line: `value > 10  // example comment`

Multi line:

```
/*
longer comment
*/
```

## Context

Object that contains all table definitions

```
ctx :: Ctx

ctx.<table1 name> = <schema1>
ctx.<table2 name> = <schema2>
```

Supports quoted and non-quoted table names:

```
ctx.example :: Example
ctx.'other' :: Other
```

## Main Function

```
main :: R : Row :: Ctx, Arg, ... -> Table<R>
```

First arg must always be of type `Ctx`

Return type must always be `R : Row :: Table<R>`

All other args are passed in via the cli.
