# Mini Rust

## About
An interpreter for a subset of the rust language. The main focus of this project was to implement algebraic data types (ADTs) with pattern matching.

### Language Features
- statically typed
- higher order functions
- algebraic data types
- pattern matching 
    - exhaustiveness
    - reachability

### Example
```rust
use std::io
use std::conv

enum Color {
    Rgb(Int, Int, Int),
    Hsv(Int, Int, Int),
}

enum Message {
    Quit,
    Move { x: Int, y: Int },
    Write(Str),
    ChangeColor(Color),
}

fn main() {
    let msg = Message::ChangeColor(Color::Hsv(0, 160, 255))

    match msg {
        Message::ChangeColor(Color::Rgb(r, g, b)) => io::println(
            "Change the color to red" + conv::int_to_str(r) 
            + ", green " + conv::int_to_str(g) 
            + ", and blue " + conv::int_to_str(b)
        ),
        Message::ChangeColor(Color::Hsv(h, s, v)) => io::println(
            "Change the color to hue " + conv::int_to_str(h) 
            + ", saturation " + conv::int_to_str(s) 
            +  ", and value " + conv::int_to_str(v)
        ),
        _ => (),
    }
}
```

_https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html : listing 18-16, slightly modified_ 

## Getting Started

### Installation
```shell
cargo install --git https://github.com/Mari-W/minirust cli
```
### Usage
```shell
minirust path/to/file/with/main.mrs
```

## Language

### Types & Values

#### Unit
```rust
let u: () = ()
```

#### Bool
```rust
let b: Bool = true
```

#### Int
Supports 64-bit integers.
```rust
let i: Int = 42
```

#### Str
Supports `UTF-8` characters.
```rust
let s: Str = "Hello World!"
```

#### Tuple
```rust
let (x, y, z): (Int, Int, Int) = (42, 42, 42)
```

#### Record
```rust
let {x, y, z}: {x: Int, y: Int, z: Int} = {x: 42, y: 42, z: 42}
```

#### Function
```rust
let id: Int -> Int = |x: Int| x
```

#### Struct
```rust
struct Point {
    x: Int,
    y: Int,
    z: Int
}

fn origin() -> Point {
    Point{x: 0, y: 0, z: 0}
}
```

#### Enum
```rust
enum Point {
    Dim2(Int, Int),
    Dim3{x: Int, y: Int, z: Int}
}

fn origin_dim2() -> Point {
    Point::Dim2(0, 0)
}
fn origin_dim3() -> Point {
    Point::Dim3{x: 0, y: 0, z: 0}
}
```

### Terms

#### Top Level
At the top level only few terms are allowed. The order in which top level terms are written _does not_ matter, while inside a top level function the language is purely functional and order does matter. Besides that name duplications (in both types and functions) are not allowed.

##### Type Definition
Type definitions are only allowed at top level.
```rust
struct UnitStruct
struct TupleStruct(Int, Int)
struct RecordStruct{x: Int, y: Int}

enum Enum {
    UnitVariant,
    TupleVariant(Int, Int),
    RecordVariant{x: Int, y: Int},
    RecursiveVariant(Enum)
}
```

##### Function Definition
Top level functions can recurse while inner functions can't. The `main` function is used as entry point into the program and cannot have arguments or return anything other than `()`.


```rust
use std::io
use std::conv

fn main() {
    io::println(conv::int_to_str(fib(10)))
}

fn fib(n: Int) -> Int {
    match n {
        0 | 1 => n,
        _ => fib(n - 1) + fib(n - 2)
    }
}
```

##### Foreign Function Definition
These functions contain rust code inside their body. This code will be compiled and linked at runtime and can be called from inside the interpreter. The FFI is completely type safe and currently only supports 2 argument functions with base types, though a prototype for ADT support exists, but more work would need to be done. This _should_ be the only place where runtime errors can occur. Every function does return a dynamic result, so you can use the `?` operator.

```rust
fn arg(idx: Int) -> Str {~
    use std::env;
    use std::convert::TryFrom;
    env::args().collect::<Vec<String>>()[usize::try_from(idx)?].clone()
~}

fn args_len() -> Int {~
    use std::env;
    use std::convert::TryInto;
    env::args().len().try_into()?
~}
```

#### Inside Functions
Inside functions the language is purely functional, although it might not always seems like it is. Consider this example where the `let` binds a new variable `x` in the continuation term `x + 42 - 42`. When missing a continuation it becomes `()`. The `in` keyword was omitted to match rust's syntax.

```rust
let x = 42
x + 42 - 42
```

##### Let Bindings
Let bindings supports pattern syntax and optional type annotations, though these are just checked against the type the type checker expects.
```rust
let (x, y) = (42, 42)
let (a, b): (Int, Int) = (42, 42)
let computed = {
    (42 + 42) / 2
}
```

##### Sequencing
A sequence _ignores_ the type of the left term and continues with the right term. In this example the `Int` value produced by the match term is ignored and the sequence has type `Str`.

```rust
match 42 {
    0 => 1
    1 => 0
    _ => 42
};
"Hello World!"
```
An sequence with no right part returns `()`.

```rust
fn main() -> () {
    "Hello World!";
}
```

##### Anonymous & Nested Functions
```rust
use std::assert;

fn compose(f: Int -> Int, g: Int -> Int) -> Int -> Int {
    // could not recurse, the function only "exists" in 
    // the continuation of the function definition
    fn composed(i: Int) {
         f(g(i))
    }
    composed
}
fn main() {
    let id = compose(|i: Int| i + 1, |i: Int| i - 1)
    assert::assert(id(42) == 42, "should be identity!")
}
```

##### Projections
You can project to a tuple (-struct, -variant) by using constant `Int`s and to a record (-struct, -variant) by using constant `Str`s

```rust
let t = (42, 42)
let x: Int = t.0

let r = { x: 42, y: 42 }
let x = r.x
```

##### Pattern Matching
There are several patterns to match on any value, including ADTs. You can use a binder as pattern, or if you don't care about the actual value, a wildcard pattern, which acts the same as the binder expect it does not introduce a new variable. You can also use any constant value as pattern, e.g. `42` when matching on `Int`. You can match on a tuple (-struct, -variant) or record (-struct, -variant) as well. Finally there is the or pattern `|` in which case one of the branch patterns can match to match the or pattern. All branches of one or pattern must introduce exactly
the same variables.

```rust
struct Struct {
    t: (Int, Int)
}

enum Enum {
    Tup((Int, Int)),
    Rec(Struct),
    Unit
}

fn match_pattern(e: Enum) -> Int {
    match e {
        // uses variant, tuple, const, wildcard, binder and or pattern
        Enum::Tup((x, 42)) | Enum::Tup((_, x)) => x,
        // uses variant, struct, record, tuple, binder pattern
        Enum::Rec(Struct { t: (x, y) }) => x + y,
        // uses unit pattern
        Enum::Unit => 42
    }
}
```

You can also use patterns in `let` bindings and function arguments.

```rust
fn arg_pattern((x, y): (Int, Int)) -> Int {
    x + y
}

fn let_pattern() {
    let { x, y } = { x: 42, y: 42 }
    let _ = 42
}
```

##### Operators

###### Int

```rust
let x = 42
let y = 42

let add: Int = x + y
let sub: Int = x - y
let mul: Int = x * y
let div: Int = x / y
let gt: Bool = x > y
let gte: Bool = x >= y
let lt: Bool = x < y
let lte: Bool = x <=y
```

###### Bool

```rust
let t = true
let f = false

let and: Bool = t & f
let or: Bool = t | f
```

###### Equality
Equality works on any type including ADTs.

```rust
let x = (42, 42)
let y = (42 41)

let eq: Bool = x == y
let neq: Bool = x != y
```

## Implementation 

### Debruijn Indices
All binders, on the term as well as on the type level are translated to debruijn indices to avoid name clashing. Consider the following example:
```rust
struct S{ b: Bool }

fn f(s: S) -> Bool {
    let f = false

    let x = s.b
    let or_false = |x: Bool| {
        x | f
    }
    or_false(x)
}
```

All variables will be translated to an integers that denote the number of binders between the variable and it's corresponding binder, separately on the type and term level. In this case this translates to:

```rust
// names do not matter anymore!
struct S { b: Bool }

// there are 0 other definitions between S and the occurrence as argument
fn f(s: Env<0>) -> Bool {
    let f = false

    let x = s.b
    // x is shadowed, but the most inner x is linked
    let or_false = |x: Bool| {
        // f will be translated to the index 2
        // because two variables (both names x) are bound in between
        Ctx<0> | Ctx<2>
    }
    
    Ctx<0>(Ctx<1>)
}
```
where `Env` is a vector that holds the types corresponding type at position of the debruijn index and `Ctx` the values respectively.

### Foreign Function Interface
All FFI functions will be compiled during type checking. Only if compilation of the rust code succeeds the type checker will succeed. Type safety is guaranteed because the function definitions itself are written in mini rust and then correctly translated to the corresponding rust function definitions. After successful compiling the rust library is dynamically linked to the interpreter as [`dylib`](https://doc.rust-lang.org/reference/linkage.html). Using the `libloading` crate the `dylib` functions can be called at runtime. Consider this example which converts a mini rust str to an integer using rust:
```rust
fn str_to_int(s: Str) -> Int {~
    s.parse::<i64>()?
~}

fn main() {
    str_to_int("42");
}
```

it will be translated to the following rust code:

```rust
type DynResult<T> = std::result::Result<T, Box<dyn std::error::Error>>

// no mangle will keep the function name as is
#[no_mangle]
// returns a dynamic error, so you can use the `?` error monad most of the time
fn str_to_int(s: String) -> DynResult<i64> {
    // always wrap into `Ok` to hide the result wrapping
    Ok({
        s.parse::<i64>()?
    })
}

```

and will later be called from `main`inside the interpreter like this:

```rust
type DynResult<T> = std::result::Result<T, Box<dyn std::error::Error>>

let i: i64 = unsafe {
    let fun: Fn(String) = lib.get::<unsafe fn(String) -> DynResult<i64>>("str_to_int")?;
    fun("42")?
}

ast::Value::Int(i)
```

Although this is using `unsafe`, since we translated the mini rust function correctly to the rust definition, this won't fail. 

### Pattern Exhaustiveness & Reachability
When implementing pattern matching you need to ensure that a pattern is _exhaustive_ to keep your language sound. Further you may want to ensure that all branches of a match statement are _reachable_, tough this is not necessary but rather a design decision. The [`usefulness`-algorithm](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html) is used test a list of patterns (can be a list of an single element, when using `let` or function argument patterns) on exhaustiveness and reachability.

First patterns are _deconstructed_ to a `DeconstructedPattern` which consists of a `PatternConstructor` and a list of _deconstructed_ sub-patterns given by the following **pseudo** code:

```rust
// used in the actual language
enum Pattern {
    Variable(String),
    Wildcard,

    Constant(Constant),

    // Point { x: Int, y: Int }
    // ^^^^^ ^^^^^^^^^^^^^^^^^^
    // name      sub-pattern
    Struct(String, Pattern),

    // Enum::Variant (x, y)
    // ^^^^  ^^^^^^^ ^^^^^^
    // name  variant sub-p
    Variant(Strong, String, Pattern),

    // contains list of branches
    Or([Pattern]),

    Unit,
    Tuple([Pattern]),
    Record({ Ident: Pattern? })
}

// used in the algorithm
enum PatternConstructor {
    // covers everything
    Wildcard,

    // has one single way to be instantiated
    Single,

    // covers one variant of a enum
    Variant(String),

    // covers a integer range
    Range(Int, Int),

    // cannot be covered
    NonExhaustive
}

struct DeconstructedPattern {
    constructor: PatternConstructor,
    sub_patterns: [DeconstructedPattern]
}
```

The destruction is defined as follows:

```rust
fn deconstruct(pattern: Pattern) -> DeconstructedPattern { 
    match pattern {
        // variables and wildcards cover everything and have no sub patterns
        Variable(_) | Wildcard => DeconstructedPattern( Wildcard, [] ),
        
        Constant(c) => match type_of(c) {
            Unit => DeconstructedPattern( Single, [] ),

            // booleans are translated to int ranges for simplicity
            True => DeconstructedPattern( Range(0, 0), [] ),
            False => DeconstructedPattern( Range(1, 1), [] ),

            Int(i) => DeconstructedPattern( Range(i, i), [] ),

            // Str cannot be exhausted (too many possibilities)
            Str => DeconstructedPattern( NonExhaustive, [] ),
        }

        // only one way to instantiate a struct
        // by design it only can have a `Unit`, `Tup` or `Rec` sub-pattern, 
        // so we take their sub-patterns as the structs sub-pattern.
        Struct(_, _, sub_pattern) => {
            DeconstructedPattern( Single, deconstruct(sub_pattern).sub_patterns )
        }

        // equivalent to structs, but only covers one variant of the enum
        Variant(_, _, name, sub_pattern) => {
            DeconstructedPattern( Variant(name), deconstruct(sub_pattern).sub_patterns )
        }

        // we need to recursively unpack nested `Or` patterns
        Or(sub_patterns) => {
            DeconstructedPattern( Or, 
                flatten([deconstruct(sub_pattern).sub_patterns for sub_pattern in sub_patterns]) 
            )
        }

        Unit => DeconstructedPattern( Single, [] ),
        
        Tuple(sub_patterns) => { 
            DeconstructedPattern( Single, 
                [deconstruct(sub_pattern) for sub_pattern in sub_patterns] 
            )
        },

    }
}
```



## Project Structure
- `ast`
    - `lib.rs`: abstract syntax tree
    - `ctx.rs`: debruijn like context
    - `tag.rs`: syntax tree annotations to preserve code file positions
    - `err.rs`: pretty error messages
    - `fmt.rs`: pretty print types & values
- `parse`
    - `lib.rs`: parses a file or string
    - `grammar.rs`: actual grammar that is parsed
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
    - `translate.rs`: translates mini rust to rust code
    - `lib.rs`: dynamically links rust code & provide type safe call interface
- `eval`
    - `lib.rs`: interpreter for mini rust
- `cli`
    - `main.rs`: command line interface binary
- `tests`
    - `macros`: contains compile time mini rust interpreter macros
    - `src`: actual tests
    - `util`: language util functions 
