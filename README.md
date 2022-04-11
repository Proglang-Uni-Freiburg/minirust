# foo lang

## what's this
experimental programming language with algebraic data types and pattern matching inspired by rust

## installation & usage
```shell
git clone https://github.com/Mari-W/foo-lang
cd foo-lang
cargo install --path cli
foo examples/example.foo
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
- `ir` [soon: resolving imports]
    - `lib.rs`: debruijn transformation / resolve identical syntax
- `typing`
    - `lib.rs`: type checking
    - `eq.rs`: defines type equality
    - `dups.rs`: filtering duplicated binders or definitions
    - `proj.rs`: handles projections on tuple / record (variants)
    - `usefulness.rs`: match exhaustiveness / pattern reachability
- `eval`
    - `lib.rs`: interpreter
- `cli`
    - `main.rs`: command line interface binary

## syntax
```rust
todo!()
```

## grammar

### const

$Ident ::= \;   [\_ a-zA-Z][\_ a-zA-Z0-9]^* \\$

$Path ::= \;    Ident^+     \\$

$Int ::=\;  0               \;|\; 
            [1..9][0..9]^*  \;|\; 
            -[1..9][0..9]^* \\$

$Bool ::= \; \text{true}    \;|\; 
             \text{false}   \\$                        

### types
$Type ::= \; Path                       \;|\; 
             Unit                       \;|\;
             Bool                       \;|\; 
             Int                        \;|\; 
             (Type^+)                   \;|\; 
             \{ (Ident:Type)^+\}        \;|\; 
             Type^+ \rightarrow Type    \\$


### operators
$BinOp ::= \;   +   \;|\; 
                -   \;|\; 
                *   \;|\; 
                /   \;|\; 
                =   \;|\; 
                !=  \;|\; 
                <   \;|\; 
                >   \;|\; 
                >=  \;|\; 
                <=  \;|\; 
                \&  \;|\; 
                |   \\$

$UnOp ::= \;    -   \;|\; 
                !   \\$

### terms
$Constructor ::= \;     (Term^+)           \;|\; 
                        \{(Ident:Term)^+\} \\$

$Pattern ::= \;     \_                                   \;|\; 
                    Ident                               \;|\; 
                    Int                                 \;|\; 
                    Bool                                \;|\;  
                    (Pattern^*)                         \;|\;
                    \{(Ident: \; Pattern?)^*\}          \;|\;
                    Path \; Pattern                        \\$


$Term ::=\; Path                                                    \;|\; 
            ()                                                      \;|\; 
            Bool                                                    \;|\; 
            Int                                                     \;|\; 
            Term;Term                                               \;|\; 
            (Term^+)                                                \;|\;  
            \{(Ident: Term)^*\}                                     \;|\; 
            Term \; BinOp \; Term                                   \;|\; 
            UnOp \; Term                                            \;|\; 
            Path::Constructor                                       \;|\;
            Term (Term^*)                                           \;|\; 
            Term.Int                                                \;|\; 
            Term.Ident                                              \;|\;
            \text{let} \; Ident^+  = Term \; \text{in} \; Term      \;|\; 
            ((Ident : Type)^*) \rightarrow \{Term\}                 \;|\; 
            \text{match} \; Term \{ (Pattern \rightarrow Term)^+ \} \;|\; 
            \text{fn} \; Ident \; ((Ident : Type)^*) \; : \; Type \; \{Term\} Term   \\ $

### values

$Body ::=       (Value^+)              \;|\; 
                \{(Ident: \; Value)^+\}    \\$

$Value ::= \; ()                                \;|\; 
                \text{true}                     \;|\; 
                \text{false}                    \;|\; 
                Int \;|\; (Value^+)             \;|\;  
                \{(Ident: Value)^+\}            \;|\; 
                (Type^∗) \rightarrow Type       \;|\;  
                Path::Body^?                     \\$

### program

$Variant ::=  Ident \; (Type^*)               \;|\; 
            Ident \; \{(Ident:Type)^*\}\\     \\$

$Top ::= \text{fn} \; Ident \; ((Ident : Type)^*) \; : \; Type \; \{Term\}  \;|\;
         \text{alias} \; Ident = Type                                       \;|\; 
         \text{struct} \; Ident \; \{(Ident:Type)^+\}^?                     \;|\; 
         \text{enum} \; Ident \; \{ (Ident \; Variant^?)^+\}               \\$

$Program ::= Top^*$
