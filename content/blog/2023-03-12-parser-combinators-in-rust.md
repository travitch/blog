+++
title = "Parser combinators in Rust"
date = 2023-03-12
[taxonomies]
tags = ["rust", "parsing"]
+++

I have been working on a side project in Rust recently and thought I would write up some of the interesting things I've encountered while building something substantial and learning Rust at the same time.

Early in the project, I had a need to some strings into structured types.  Coming from Haskell, my first thought was to use parser combinators; however, I was open to other options if there was something better or more conventional in the Rust ecosystem.  It is probably worth a separate post to explore the parsing ecosystem, but long story short: none of the solutions really stood out to me.  In the realm of parser combinators, the most prominent solution seems to be [nom](https://crates.io/crates/nom), but it really did not click for me and seemed to have poor composition tools, which is a problem for a parser combinator library.  I ended up using [combine](https://crates.io/crates/combine), which is more like what I would expect from a parser combinator library.

Consider this definition of types for a language:

```rust
pub enum Type {
    PrimInteger,
    PrimBoolean,
    PrimString,
    List(Box<Type>),
}
```

The first three constructors are primitive types, while the last type is a recursive type that can contain other types.  For example, `List<List<PrimInteger>>` is allowed.  I'll walk through my journey trying to parse this simple type.  First, start with some imports for the `combine` library:

```rust
use combine::parser::char::string;
use combine::{between, choice, parser};
use combine::{ParseError, Parser, Stream};
```

Next, this combinator parses the basic types:

```rust
fn basic_type_parser<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice!(
        string("int").map(|_| Type::PrimInteger),
        string("boolean").map(|_| Type::PrimBoolean),
        string("string").map(|_| Type::PrimString)
    )
}
```

I want to make a few observations before continuing.  The basic combinator implementation with the `choice!` macro and simple `string` parsing combinator are exactly what I expect from a parser combinator library.  I miss the clean `Applicative` interface from Haskell's parser combinator libraries, but the `map` interface in `combine` is fine.  I like types, but the type signature is unwieldy and bordering on unreasonable; the required definitions in the `where` clause are especially egregious.

Next, the following two parser combinators build on the primitive combinator above to parse the rest of the type language:

```rust
// A parser for the list type (note that List<List<X>> is allowed, so this can
// be recursive)
fn aggregate_type_parser<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice!(
        (
            string("List"),
            between(string("<"), string(">"), type_parser())
        ).map(|(_, ty)| Type::List(Box::new(ty)))
    )
}

// A top-level parser to parse any type
fn type_parser<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    basic_type_parser().or(aggregate_type_parser())
}
```

Unfortunately, this does not compile. The compiler gives the following error:

```
error[E0720]: cannot resolve opaque type
   --> src/main.rs:27:38
    |
27  |   fn aggregate_type_parser<Input>() -> impl Parser<Input, Output = Type>
    |                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ recursive opaque type
...
33  | /         (
34  | |             string("List"),
35  | |             between(string("<"), string(">"), type_parser())
36  | |         )
37  | |             .map(|(_, ty)| Type::List(Box::new(ty)))
    | |                                                    -
    | |____________________________________________________|
    | |____________________________________________________returning here with type `combine::parser::combinator::Map<(impl Parser<Input, Output = &str>, Between<Input, impl Parser<Input, Output = &str>, impl Parser<Input, Output = &str>, impl Parser<Input, Output = Type>>), [closure@src/main.rs:37:18: 37:27]>`
    | |____________________________________________________returning here with type `combine::parser::combinator::Map<(impl Parser<Input, Output = &str>, Between<Input, impl Parser<Input, Output = &str>, impl Parser<Input, Output = &str>, impl Parser<Input, Output = Type>>), [closure@src/main.rs:37:18: 37:27]>`
    | |____________________________________________________returning here with type `combine::parser::combinator::Map<(impl Parser<Input, Output = &str>, Between<Input, impl Parser<Input, Output = &str>, impl Parser<Input, Output = &str>, impl Parser<Input, Output = Type>>), [closure@src/main.rs:37:18: 37:27]>`
    |                                                      returning here with type `combine::parser::combinator::Map<(impl Parser<Input, Output = &str>, Between<Input, impl Parser<Input, Output = &str>, impl Parser<Input, Output = &str>, impl Parser<Input, Output = Type>>), [closure@src/main.rs:37:18: 37:27]>`
...
41  | | fn type_parser<Input>() -> impl Parser<Input, Output = Type>
    | |                            --------------------------------- returning this opaque type `combine::parser::combinator::Map<(impl Parser<Input, Output = &str>, Between<Input, impl Parser<Input, Output = &str>, impl Parser<Input, Output = &str>, impl Parser<Input, Output = Type>>), [closure@src/main.rs:37:18: 37:27]>`
    |
   ::: /home/tristan/.cargo/registry/src/github.com-1ecc6299db9ec823/combine-4.6.6/src/parser/char.rs:260:46
    |
260 |   pub fn string<'a, Input>(s: &'static str) -> impl Parser<Input, Output = &'a str>
    |                                                ------------------------------------ returning this opaque type `combine::parser::combinator::Map<(impl Parser<Input, Output = &str>, Between<Input, impl Parser<Input, Output = &str>, impl Parser<Input, Output = &str>, impl Parser<Input, Output = Type>>), [closure@src/main.rs:37:18: 37:27]>`

error[E0720]: cannot resolve opaque type
  --> src/main.rs:41:28
   |
13 | fn basic_type_parser<Input>() -> impl Parser<Input, Output = Type>
   |                                  --------------------------------- returning this opaque type `Or<impl Parser<Input, Output = Type>, impl Parser<Input, Output = Type>>`
...
27 | fn aggregate_type_parser<Input>() -> impl Parser<Input, Output = Type>
   |                                      --------------------------------- returning this opaque type `Or<impl Parser<Input, Output = Type>, impl Parser<Input, Output = Type>>`
...
41 | fn type_parser<Input>() -> impl Parser<Input, Output = Type>
   |                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ recursive opaque type
...
46 |     basic_type_parser().or(aggregate_type_parser())
   |     -----------------------------------------------
   |     |
   |     returning here with type `Or<impl Parser<Input, Output = Type>, impl Parser<Input, Output = Type>>`
   |     returning here with type `Or<impl Parser<Input, Output = Type>, impl Parser<Input, Output = Type>>`
```

This set of errors is overwhelming, but has a single cause.  The first error message indicates that `impl Parser<Input, Output = Type>` (the return type of the aggregate type parser) is a "recursive opaque type".  It sure is.  The `impl Trait` feature is a way to tell the compiler that the function will return some type that implements the named trait and ask the compiler to infer the concrete type.  My understanding is that the Rust compiler uses a constraint solver to do so, but will not attempt to solve `impl Trait` declarations for functions that occur in recursive cycles.  That is a problem for parser combinators, where recursion is essential.

It seems like it should be possible to break the recursive cycle by writing the type by hand to save the compiler from having to solve it.  It is not clear what that type should really be; I suspect that it is challenging to write.  It turns out that the right answer is to rewrite the functions as follows:

```rust
fn type_parser<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    type_parser_()
}

parser! {
    fn type_parser_[Input]()(Input) -> Type
    where [ Input: Stream<Token = char> ]
    {
        basic_type_parser().or(aggregate_type_parser())
    }
}
```

The `parser!` macro provided by the `combine` library makes the type explicit, accomplishing that goal{% sidenote() %}
I was not able to find this explicitly called out in the documentation.  I found it by reading [an example](https://github.com/Marwes/combine/blob/3f59f9d9173ffc8d017a7eed7dc52c128ba5159c/benches/json.rs#L162) in the GitHub repository.
{% end %}.  While this works, it is unfortunate that it has to be this way.  There are at least two major factors at play here:

1. The design of the `combine` library makes this problem inevitable
2. The implementation of the Rust language feels incomplete

The `combine` library made the choice for one style of ergonomic type signature, which relies on the `impl Trait` pattern.  I do not know what all of the alternatives look like, so it is difficult to evaluate that choice right now.

The Rust language chooses not to require or support inference of `impl Trait` types in the presence of recursion.  That does not seem like an inherent limitation to the model, as general constraint solvers have no trouble with recursion.  It seems like this is omitted because the implementation cannot reliably handle it.  I have not looked for sources on that, but I would be shocked if it was computationally infeasible to support.

For now, the `parser!` macro is an acceptable workaround.  I hope the Rust language evolves to be more general and reduce the number of sharp edges in the type system.
