# foo lang

## what's this
experimental programming language with algebraic data types and pattern matching inspired by rust

## usage
### install
```shell
git clone https://github.com/Mari-W/foo-lang
cd foo-lang
cargo install --path cli
```
### usage
```shell
foo path/to/file/with/main.foo
```
### update
```shell
git pull 
cargo install --path cli
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
    - `useful.rs`: match exhaustiveness & pattern reachability
- `ffi`
    - `translate.rs`: translates to rust code
    - `lib.rs`: dynamically links rust code & provide type safe call interface
- `eval`
    - `lib.rs`: interpreter
- `cli`
    - `main.rs`: command line interface binary
- `tests`
    - `macros`: contains compile time interpreter macros
    - `src`: actual tests
    - `util`: language util functions 
