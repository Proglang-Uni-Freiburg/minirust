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
At the top level only few terms are allowed. Further name duplications (in between both types and functions) are not allowed.

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
These functions contain rust code inside their body. This code will be compiled and linked at runtime and can be called from inside the interpreter. The FFI is completely type safe and currently only supports 2 argument functions with base types, though a prototype for ADT support exists, but more work would need to be done. This _should_ be the only place where runtime errors can occur. Every function does return a dynamic result, so you can use the `?` error monad.

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
Let bindings support pattern syntax and optional type annotations.

```rust
let (a, b): (Int, Int) = (42, 42)
let (x, y) = (42, 42)
let z = {
    (42 + 42) / 2
}
```

##### Sequencing
A sequence _ignores_ the result of the left term and continues with the right term. In this example the `Int` value produced by the match term is ignored and the sequence has type `Str`.

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
You can project out of a tuple (-struct, -variant) by using constant `Int`s and out of a record (-struct, -variant) by using the corresponding label.

```rust
let t = (42, 42)
let x: Int = t.0

let r = { x: 42, y: 42 }
let x: Int = r.x
```

##### Pattern Matching
There are several patterns to match on any value, including ADTs. "You can bind a variable as a pattern", or if you don't care about the actual value, a wildcard pattern, which acts the same as the variable except it does not introduce a new variable. You can also use any constant value as pattern, e.g. `42` when matching on `Int`. You can match on a tuple (-struct, -variant) or record (-struct, -variant) as well. Finally there is the or-pattern `|` in which case one of the branch patterns can match to match the or-pattern itself. All branches of one or-pattern must introduce exactly
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
Equality works on any type including ADTs and performs structural equality, therefore the type path (includes name and file path to ensure uniqueness) and it's elements / field names & values must be equal.

```rust
use other::S;

struct S {
    x: 42
}

fn eq() {
    let x = S { x: 42 }
    let y = S { x: 42 }
    let o = other::S { x: 42 }

    let eq: Bool = x == y   // true
    let eq2: Bool = x == o  // false

    let neq: Bool = x != y  // false
    let neq2: Bool = x != o // true
}
```

## Implementation 

### Debruijn Indices
All variables, on the term as well as on the type level are translated to debruijn indices to avoid name clashing. Consider the following example:
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
struct S { b: Bool }

struct Unused

// there is 1 other definition between the definition of S 
// and the occurrence as argument
fn f(s: Env[1]) -> Bool {
    let f = false

    let x = s.b

    let or_false = |x: Bool| {
        // f will be translated to the index 2
        // because two variables (both names x) are bound in between
        // and x will be translated to the closest x in scope
        Ctx[0] | Ctx[2]
    }
    
    // or_else is the definition above 
    // and the closest x is directly above or_else
    Ctx[0](Ctx[1])
}
```
where `Env` is a vector that holds the corresponding types at the position of the debruijn indices and `Ctx` the values respectively.

### Foreign Function Interface
All FFI functions will be compiled during type checking. Only if compilation of the rust code succeeds the type checker will succeed. Type safety is guaranteed because the function definitions itself are written in mini rust and then correctly translated to the corresponding rust function definitions. After successful compiling, it is dynamically linked to the interpreter as [`dylib`](https://doc.rust-lang.org/reference/linkage.html). Using the `libloading` crate the `dylib` functions can be called at runtime. Consider this mini rust example which converts a `Str` to an `Int` using rust:
```rust
fn str_to_int(s: Str) -> Int {~
    s.parse::<i64>()?
~}

fn main() {
    str_to_int("42");
}
```

This will be generate the following rust code:

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

and will later be called from mini rust's `main` function like this:

```rust
type DynResult<T> = std::result::Result<T, Box<dyn std::error::Error>>

let i: i64 = unsafe {
    let fun = lib.get::<unsafe fn(String) -> DynResult<i64>>("str_to_int")?;
    fun("42")?
}

ast::Value::Int(i)
```
Although the call is `unsafe`, since we translated the mini rust function definition correctly to the rust definition, this won't fail. 

### Pattern Exhaustiveness & Reachability
In the following section a complete rust pseudo code algorithm will be given to implement exhaustiveness and reachability.

We will assume that the pattern were already type checked. Because of that we can assume that patterns on the same position and nesting level have the same type.

```rust
// wouldn't type check 
match (42, (42, 42)) {
    (42, (42, "42")) => 42,
    (42, (42, 42)) => 42,
    _ => -42
}
```

When implementing pattern matching we want to ensure that a list of patterns is _exhaustive_, that is, all possible values are covered by at least one pattern. Furthermore we want that all patterns of a match term are _reachable_, i.e they can be reached by _any_ of all possible input's with respect to the patterns before it.

The [`usefulness`-algorithm](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html) is used to test a list of patterns on exhaustiveness and reachability. A list of an single pattern can be tested as well, e.g. when checking `let` or function argument patterns. The Algorithm takes a, possibly empty, list of patterns `ps` and one pattern `q` to test if `q` is useful with respect to the patterns `ps` before it. 

When all patterns are useful with respect to the patterns before them, there is no _unreachable_ pattern.
The list of patterns is _exhaustive_ iff the wildcard pattern is **not** useful.   

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
    Variant(String, String, Pattern),

    // contains list of branches
    Or([Pattern]),

    Unit,
    Tuple([Pattern]),
    // {Ident : Pattern} denotes a dictionary
    Record({Ident: Pattern})
}

// used in the algorithm to express what the pattern does cover
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
    NonExhaustive,

    // internally used to represent missing constructors
    Missing([PatternConstructor])
}

struct DeconstructedPattern {
    // the constructor of the pattern
    constructor: PatternConstructor,
    // the constructors of its sub patterns
    sub_patterns: [DeconstructedPattern]
}
```

The deconstruction is defined as follows:

```rust
fn deconstruct(pattern: Pattern) -> DeconstructedPattern { 
    match pattern {
        // variables and wildcards cover everything and have no sub patterns
        Variable | Wildcard => DeconstructedPattern(Wildcard, []),
        
        Constant(constant) => match constant {
            Unit => DeconstructedPattern(Single, []),

            // booleans are translated to int ranges for simplicity
            Bool(True) => DeconstructedPattern(Range(0, 0), []),
            Bool(False) => DeconstructedPattern(Range(1, 1), []),

            Int(i) => DeconstructedPattern(Range(i, i), []),

            // Str cannot be exhausted (too many possibilities)
            Str => DeconstructedPattern(NonExhaustive, []),
        }

        // only one way to instantiate a struct.
        // by design it only can have a `Unit`, `Tup` or `Rec` sub-pattern, 
        // so we take their sub-patterns as the structs sub-patterns.
        Struct(sub_pattern) => {
            DeconstructedPattern(Single, deconstruct(sub_pattern).sub_patterns)
        }

        // equivalent to structs, but only covers one variant of the enum
        Variant(name, sub_pattern) => {
            DeconstructedPattern(Variant(name), deconstruct(sub_pattern).sub_patterns)
        }

        Or(sub_patterns) => {
            DeconstructedPattern(Or, 
                // code for recursively expanding nested or-patterns is omitted
                flatten([deconstruct(expand(sub_pattern)) for sub_pattern in sub_patterns]) 
            )
        }

        // single constructor
        Unit => DeconstructedPattern(Single, []),
        
        // again only one way to instantiate a tuple but has sub-patterns
        Tuple(sub_patterns) => { 
            DeconstructedPattern(Single, 
                [deconstruct(sub_pattern) for sub_pattern in sub_patterns] 
            )
        },

        // equivalent to tuples
        Record(sub_patterns) => { 
            DeconstructedPattern(Single, 
                [deconstruct(sub_pattern) for (_, sub_pattern) in sub_patterns] 
            )
        },
    }
}
```

Let's see an example:

```rust
struct S {
    t: (Int, Int)
}

fn main() {
    let s = S { t: (42, 42) }
    // consider this match term
    match s {
        S { t: (42, x) | ((x, 42) | (-42, -42)) } => 42
        _ => -42
    }
}
```

would be deconstructed as follows: 

```rust
let patterns: [DeconstructedPattern] = [
    // sub-pattern of the record pattern got 
    // "lifted" to be the structs sub-patterns
    DeconstructedPattern(Single, [
        DeconstructedPattern(Or, [
            // nested or got expanded
            DeconstructedPattern(Single, [
                DeconstructedPattern(Range(42, 42), []), 
                DeconstructedPattern(Wildcard, [])
            ]),
            DeconstructedPattern(Single, [
                DeconstructedPattern(Wildcard, []), 
                DeconstructedPattern(Range(42, 42), [])
            ]),
            DeconstructedPattern(Single, [
                DeconstructedPattern(Range(-42, -42), [])
                DeconstructedPattern(Range(-42, -42), [])
            ]),
        ]),
    ])
    DeconstructedPattern(Wildcard, [])
]
```

We now have translated our input, a list of patterns, to a list of deconstructed patterns.
Next we need to implement to helper functions for our final `usefulness`-algorithm.
From now we will refer to "deconstructed patterns" as "patterns".

First we implement two small helper functions `all_constructors` for listing all constructors for a given pattern by the type it tries to cover and  `covered_by` to check if a pattern _covers_ (e.g. range includes another) the other pattern:

```rust
// returns all possible constructors for a given pattern by the type it tries to match
fn all_constructors(pattern: DeconstructedPattern) -> [PatternConstructor] {
    match pattern type {
        Unit | Tup | Rec | Struct => [Single],
        // can only be 0 or 1
        Bool => [Range(0, 1)],
        Int => [Range(i64::MAX, i64::MIN)],
        Str => [NonExhaustive],
        // enum has all variants as possible constructor
        Enum(variants) => [Variant(variant) for variant in variants]
    }
}

// returns `true` if `pattern` is covered by `other`, `false` otherwise
// the algorithm e
fn covered_by(pattern: DeconstructedPattern, other: DeconstructedPattern) {
    match (pattern, other) {
        // pattern is always covered by wildcard
        (_, Wildcard) => true,
        // wildcard and missing constructors cannot be covered
        (Wildcard | Missing, _) => false
        
        (Single, Single) => true

        // range:                     lo -------- hi
        // other_range      other_hi ---------------- other_lo
        (Range(lo, hi), Range(other_lo, other_hi)) => lo >= other_lo && hi <= other_hi

        (Variant(variant), Variant(other_variant)) => variant == other_variant

        // non-exhaustive patterns cannot be covered
        (NonExhaustive, _) => false

        // other patterns cannot be reached by how the algorithm is constructed later on
    }
}
```

Second we want to _split_ a pattern `q` with respect to patterns `ps`. The idea of `constructor splitting` is to group together constructors that behave the same way and list all constructors implied by `q`. For example the wildcard pattern implies to cover _all_ the constructors of a given pattern and the ranges `(0, 0)` and `(0, 1)` can be grouped to be one range `(0, 1)`. Consider the following:

```rust
fn split(
    pattern: DeconstructedPattern, 
    others: [DeconstructedPattern]
) -> [DeconstructedPattern] {
    match pattern {
        Range(lo, hi) => {
            others
                // only take other ranges inside `others` into account
                .filter(|other| other is Range) 
                // range:            lo ------------------------ hi
                // other_range:          other_hi ---- other_lo
                // -> keep `other_range` if contained by `range`
                .filter(|other| is_covered(other, range))
                // sort the ranges and merge two consecutive ranges if they 
                // are connecting, e.g. end of previous is start of next
                .group()
        },
        Wildcard => {
            // get all possible constructors
            let all: [PatternConstructor] = all_constructors(pattern)
                // recursively split all the possible constructors for the wildcard 
                // with respect to the others and merge them.
                // recursion will indefinitely stop because split itself
                // does not returns wildcards
                .flat_map(|constructor| constructor.split(others))

            let others: [PatternConstructor] = others
                    // remove all wildcards from `others`
                    // they would cover everything
                    .filter(|other| !(other is Wildcard))

            let missing: [PatternConstructor] = all
                // check if `others` is covering all the possible constructors
                .filter(|constructor| 
                    !others
                        // keep the `constructor` if it is not covered by any
                        // `other` constructor from `others` to collect
                        // all constructors not covered by others 
                        .any(|other| covered_by(constructor, other))
                )
            
            // if there are missing constructors we return them as missing
            if missing.is_not_empty() {
                // since there are missing constructors 
                // we can ignore those present.
                // `Missing` can only be covered by wildcard
                [Missing(missing)]
            } 
            // if all constructors are covered we return them all
            else {
                all
            }
        },
        // nothing to split
        _ => [pattern]
    }
}
```

Finally we need to _specialize_ a pattern in respect to a list of patterns. The `specialize_vector` function takes a list of patterns `ps` and a pattern `q` and returns a list of constructors that are _only_ covered by `q` and not by any heads, i.e. the first element in the vector and therefore the other constructors on the same position and depth level, in `ps`. Equivalently `specialize_matrix` performs `specialize_vector` for all `ps` inside an matrix `ms`, if the head of the `ps` is covered by `q`, because otherwise `p` cannot cover anything `q` covers anyways.
```rust

// specialize one pattern with respect to another
fn specialize_constructor(
    pattern: DeconstructedPattern, 
    other: DeconstructedPattern
) -> [DeconstructedPattern] {
    match (pattern.constructor, other.constructor) {
        // a wildcard acts as wildcard for all sub patterns as well
        (Wildcard, _) => match other.constructor {
            Single => match pattern type {
                // give a wildcard for all the 
                // (struct-) tuple elements / (struct-) record fields
                Tuple(els) | Record(els) | Struct(els) => [Wildcard for _ in els],
                // all single patterns are matched here
            },
            Variant(variant) => match pattern type {
                // wildcard for all the tuple/record -variant elements/fields
                Enum(variants) => [Wildcard for variants.find(variant).els]
                // other cases cannot be reached, only enums have variants
            }
            // others do no need to be recursed
            _ => []
        }
        // otherwise we need to cover all the sub patterns
        _ => pattern.sub_patterns
    }
}

// specialized head of one row with respect to another pattern
fn specialize_vector(
    vector: [DeconstructedPattern],
    other: DeconstructedPattern,
) -> [DeconstructedPattern] {
    // specialize one row by specializing the head pattern (at index 0) of the
    // vector and removing it from the row
    let head = vector.pop(0)
    let new = specialize_constructor(head, other)
    // add all other patterns untouched
    new + vector
}

// specialized all heads of all rows with respect to another pattern
fn specialize_matrix(
    matrix: [[DeconstructedPattern]],
    other: DeconstructedPattern,
) -> [DeconstructedPattern] {
    let new = []
    for vector in matrix {
        if is_covered(other, row.head) {
            mat += [specialize_vector(other, vector)]
        }
    }
    new
}
```

In the end we can compute `usefulness` for a given list of patterns `ps` is respect to a matrix of patterns `ms` by defining the `usefulness`-algorithm:

```rust
fn is_useful(
    matrix: [[DeconstructedPattern]], 
    patterns: [DeconstructedPattern]
) -> bool {
    // base case
    // if there is no pattern left in pattern
    if patterns.is_empty() {
        // if matrix is empty the list of patterns was useful
        return matrix.is_empty()
    }

    // we recursively expand all `Or` patterns
    // at the heads of all rows in the matrix. 
    let matrix = matrix.flat_map(|vector| match vector {
        // code for expand omitted, we just lift 
        // all `sub_patterns` in the branches
        // to being rows in the matrix
        [Or, ..] => expand(vector)
        _ => [vector]
    })

    let reachable = false

    if patterns.head is Or {
        let all_branches_reachable = false

        for branch in expand(patterns) {
            // all branches must be useful inside or pattern with respect to 
            // branches before them
            all_branches_reachable &= is_useful(matrix, branch)
            
            // an or branch acts as yet another vector
            matrix += branch
        }

        reachable |= all_branches_reachable
    } else {
        // go through all the split constructors of head in respect to
        // all heads in patterns before
        for constructor in split(patterns.head, matrix.heads) {
            // its reachable iff sub-patterns are reachable in respect to 
            // all sub-patterns at that depth before
            reachable |= is_useful(
                specialize_matrix(matrix, constructor),
                specialize_vector(patterns, constructor)
            )

        }
    }

    return reachable
}
```

And to wrap it all up we can use this to define the `exhaustiveness`-algorithm for a list of patterns by checking if the wildcard is useful in respect to all patterns  

```rust
fn is_exhaustive(patterns: [Pattern]) -> .. {
    let matrix: [[DeconstructedPattern]] = []

    // deconstruct all patterns
    let deconstructed: [DeconstructedPattern] = patterns
        .map(|pattern| deconstruct(pattern))
    
    // check all patterns for reachability 
    let pattern_reachable: {DeconstructedPattern: Bool} = deconstructed
        .map(|pattern| {
            pattern: is_useful(matrix, [pattern])
        })
    
    // check if wildcard is useful, if so 
    // the list of patterns is non-exhaustive.
    // needs to be done after reachability checking
    // so that all patterns are in the matrix already
    if is_useful(matrix, [ DeconstructedPattern ( Wildcard, [] )]) {
        // handle non-exhaustiveness
    }

    // check if there are unreachable patterns / `Or`-branches
    for (pattern, reachable) in pattern_reachable {
        if !reachable {
            // handle non-reachability
        }
    }
}
```

Currently Witnesses, that is a listing  missing constructors when the exhaustiveness check fails, are not supported but should be fairly easy to implement.

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
