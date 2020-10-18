mod ast;
mod base;
mod interpreter;
mod parser;
mod sql;
mod types;

use ast::{Ast, InputError};

fn main() -> Result<(), InputError> {
    for expression_input in &[
        "5",
        "hello_world",
        "true",
        "'true'",
        "b = 4",
        "foo(bar, baz, q)",
        "(foo, bar) -> 1",
        "{a}",
        "{'foo'; a}",
        "{a; 'foo'; 1}",
        "foo.bar",
        "foo.'baz'",
        "4.5",
        "4 == 4",
        "5 >= 3 == 2",
    ] {
        println!("Expression Input: {}", expression_input);
        match parser::parse_expression(expression_input) {
            Some((token, rest)) => {
                println!("Token:            {:?}", token);
                println!("Rest:             '{}'", rest);
            }
            None => println!("Error"),
        };
        println!();
    }

    for type_input in &[
        "{a: B}",
        "Foo",
        "Col<Int>",
        "List<Col<Bool>>",
        "type Foo = Bar",
        "type Foo = {a :B, cd: De}",
        "a :: Int",
        "foo :: (Int, Bool) -> Int",
        "bar :: R : Row :: (Int) -> Table<R>",
        "Foo | Bar",
        "Foo | Bar<Baz> | B",
    ] {
        println!("Type Input: {}", type_input);
        match parser::parse_type(type_input) {
            Some((token, rest)) => {
                println!("Token:      {:?}", token);
                println!("Rest:       '{}'", rest);
            }
            None => println!("Error"),
        };
        println!();
    }

    let input = "
        a :: Int
        a = 5

        foo :: (Int) -> Bool
        foo = (test) -> true

        result :: Bool
        result = foo(1)

        type A = Int
        type Bc = Bool

        colInt :: (Int) -> Col<Int>
        colBool :: (Bool) -> Col<Bool>

        row :: { a: A, 'b c': Bc }
        row = { a: colInt(1), 'b c': colBool(true) }

        union :: Int | Bool
        union = true

        result :: Bool
        result = a == 5

        gen :: P : Primitive :: Col<P>
        gen = colInt(1)

        main :: String
        main = 'foo'
    ";

    let tokens = parser::parse(input)?;
    let ast = Ast::from_tokens(tokens)?;
    println!("{}", ast);

    let input = "
        block :: Int
        block = {
            let a :: Int = 1;
            a
        }

        main :: Int
        main = block
    ";

    let tokens = parser::parse(input)?;
    let ast = Ast::from_tokens(tokens)?;
    println!("{}", ast);

    let value = interpreter::run(ast);
    println!("{:?}", value);

    Ok(())
}
