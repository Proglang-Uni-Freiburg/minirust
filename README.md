# Mini Rust

## About
An interpreter for a subset of the rust language. The main focus of this project was to algebraic data types with pattern matching.

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
Cargo & rustc should be installed. 

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
```rust
let i: Int = 42
```

#### Str
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
There can only be 5 different terms at the top level.

##### Alias
```rust
```

#### Operators
##### Int
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

##### Bool
```rust
let t = true
let f = false

let and: Bool = t & f
let or: Bool = t | f
```

##### Equality
Equality works on any type including ADT's.

```rust
let x = (42, 42)
let y = (42 41)

let eq: Bool = x == y
let neq: Bool = x != y
```


## project structure
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
