# foo lang

## what's this
experimental programming language with algebraic data types and pattern matching inspired by rust

## installation & usage
```shell
git clone https://github.com/Mari-W/foo-lang
cd foo-lang
cargo install --path cli
foo examples/main.foo
```

## structure & crates
- `ast`
    - `lib.rs`: abstract syntax tree
    - `ctx.rs`: debruijn like context
    - `tag.rs`: syntax tree annotations
    - `err.rs`: pretty error messages
- `parse`
    - `lib.rs`: file or string parsing
    - `grammar.rs`: actual peg
- `ir`
    - `lib.rs`: transform to immediate representation
    - `debruijn.rs`: debruijn indices
    - `resolve.rs`: resolving imports
- `typing`
    - `lib.rs`: type checking
    - `eq.rs`: defines type equality
    - `dups.rs`: filtering duplicated binders or definitions
    - `proj.rs`: handles projections on tuple / record (variants)
    - `usefulness.rs`: match exhaustiveness & pattern reachability
- `ffi`
    - `translate.rs`: translates to rust code
    - `lib.rs`: compiles & calls rust code
- `eval`
    - `lib.rs`: interpreter
- `cli`
    - `main.rs`: command line interface binary

## syntax
```rust
todo!()
```