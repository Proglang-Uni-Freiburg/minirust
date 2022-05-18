use crate::KEYWORDS;
use ast::tag::Untag;
use ast::{
    err::CodeRef,
    err::GetCodeRef,
    tag::{Item, Tag},
    Named,
};

ast::def_ast_types! {
    type => Named,
    prefix => ast
}

peg::parser! {
    pub grammar lang(path: &ast::Path) for str {

        // helpers
        rule _  = quiet!{[' ' | '\t' | '\r']*} // optional space without \n
        rule __ = quiet!{[' ' | '\n' | '\t' | '\r' ]*} // optional space with \n

        rule spa() = quiet!{[' ' | '\t' | '\r']+} // required space without \n
        rule sep() = quiet!{[' ' | '\t' | '\r']*} ['\n' | ';'] quiet!{[' ' | '\t' | '\r' | '\n']*} // optional space with required \n or ;

        rule str() -> String
            = cs:$(quiet!{['_' | 'a' ..= 'z' | 'A' ..= 'Z']['a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9' | '_' ]*}) {?
                if KEYWORDS.contains(&cs) {
                    Err("non-keyword")
                } else {
                    Ok(cs.into())
                }
            }
        rule i64() -> i64
            = i:$(quiet!{(['1'..='9'])?['0'..='9']*}) {? i.parse::<i64>().or(Err("0..9")) }

        rule tag<T: Item>(x: rule<T>) -> Tag<CodeRef, T>
            = s:position!() x:x() e:position!() { Tag::new((path.clone(), (s, e)), x) }

        use peg::ParseLiteral;
        rule lit_tup<T: Item, V: Item>(lit: &'static str, x: rule<T>, y: rule<V>) -> (T, V)
            = x:x() _ ##parse_string_literal(lit) _ y:y() { (x, y) }
        rule lit_sep<T: Item>(lit: &'static str, x: rule<T>) -> Vec<T>
            = s:position!() v:(x() ** (_ ##parse_string_literal(lit) __)) (_ ##parse_string_literal(lit))? e:position!() {
                Tag::new((path.clone(), (s, e)), v)
            }
        rule lit_sep_plus<T: Item>(lit: &'static str, x: rule<T>) -> Vec<T>
            = s:position!() v:(x() ++ (_ ##parse_string_literal(lit) __)) (_ ##parse_string_literal(lit))? e:position!() {
                Tag::new((path.clone(), (s, e)), v)
            }
        rule lit_sep_plus_two<T: Item>(lit: &'static str, x: rule<T>) -> Vec<T>
            = s:position!() f:x() _ ##parse_string_literal(lit) __ v:(x() ++ (_ ##parse_string_literal(lit) __)) (_ ##parse_string_literal(lit))? e:position!() {
                let mut v = v.clone(); v.insert(0, f); Tag::new((path.clone(), (s, e)), v)
            }

        // primitives
        rule ident() -> Ident
            = i:tag(<str()>) { i }

        rule path() -> Path
            = ts:lit_sep_plus("::", <ident()>) { ts }

        rule actual_path() -> Path
            = ts:lit_sep_plus_two("::", <ident()>) { ts }

        rule int() -> Int
            = i:tag(<i64()>) { i }

        // types
        rule _typ() -> ast::Type<Named>
            = "(" __  ts:lit_sep_plus(",", <typ()>) __ ")" _ "->" _ c:typ() { ast::Type::Fun(ts, c) }
            / "{" __ ts:lit_sep_plus(",", <lit_tup(":", <ident()>, <typ()>)>) __ "}" { ast::Type::Rec(ts) }
            / "(" __ ts:lit_sep_plus(",", <typ()>) __ ")" { ast::Type::Tup(ts) }
            // / "(" _ ts:lit_sep_plus("|", <typ()>) _ ")" { ast::Type::Sum(ts) }
            / "()" { ast::Type::Unit }
            / i:path() { match i.as_ref()[0].as_ref().as_str() {
                "Bool" if i.as_ref().len() == 1 => ast::Type::Bool,
                "Int" if i.as_ref().len() == 1 => ast::Type::Int,
                "Str" if i.as_ref().len() == 1 => ast::Type::Str,
                _ => ast::Type::Name(i)
            } }

        rule typ() -> Type = precedence!{
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            d:@ _ "->" _ c:(@) { ast::Type::Fun(Tag::new((path.clone(), (d.start(), d.end())), vec!(d)), c) }
            s:position!() "()" e:position!() _ "->" _ c:@ { ast::Type::Fun(Tag::new((path.clone(), (s, e)), vec!()), c) }
            --
            t:_typ() { t }
        }


        // terms
         rule _constructor() -> ast::Constructor<Named>
            // = i:ident() _ "(" _ ts:lit_sep_plus(",", <term()>) _ ")" { ast::Constructor::Tup(i, ts) }
            = "{" __ ts:lit_sep_plus(",", <lit_tup(":", <ident()>, <term()>)>) __ "}" { ast::Constructor::Rec(ts) }
            // i:ident() { ast::Constructor::Unit(i) }

        rule constructor() -> Constructor = precedence! {
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            c:_constructor() { c }
        }

        rule _rec_pattern() -> (Ident, Option<Pattern>)
            = i:ident() _ ":" _ p:pattern() { (i, Some(p)) }
            / i:ident() { (i, None) }

        rule _var_pat() -> ast::Pattern<Named>
            = "(" __ ts:lit_sep_plus(",", <pattern()>) __ ")" { ast::Pattern::Tup(ts) }
            / "{" __ ts:lit_sep_plus(",", <_rec_pattern()>) __ "}" { ast::Pattern::Rec(ts) }

        #[cache_left_rec]
        rule _pattern() -> ast::Pattern<Named>
            = ts:lit_sep_plus_two("|", <pattern()>)  { ast::Pattern::Or(ts) }
            / p:_var_pat() { p }
            / i:path() _ s:position!() p:_var_pat() e:position!() { let mut x = i.untag(); ast::Pattern::Struct(i, x, Tag::new((path.clone(), (s, e)), p)) }
            / i:path() {  let mut p = i.untag(); let i_tag = i.tag.clone(); ast::Pattern::Struct(i, p, Tag::new(i_tag, ast::Pattern::Unit)) }
            / "_" { ast::Pattern::Wildcard }
            / t:const_term() { ast::Pattern::Const(t) }

        rule pattern() -> Pattern = precedence! {
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            p:_pattern() { p }
        }

        rule _const_term() -> ast::Term<Named>
            = "false" { ast::Term::False }
            / "true" { ast::Term::True }
            / i:i64() { ast::Term::Int(i) }
            / "\"" s:$(quiet!{[^'"']*}) "\"" { ast::Term::Str(s.into()) }
            / "()" { ast::Term::Unit }

        rule const_term() -> Term  = precedence! {
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            c:_const_term() { c }
        }

        #[cache_left_rec]
        rule _term() -> ast::Term<Named>
            = "let" spa() __ p:pattern() __ "=" __ t:term() _ "\n" __ c:term() { ast::Term::Let(p, None, t, c) }
            / "let" spa() __ s:position!() p:pattern() e:position!() __ "=" __ t:term() { ast::Term::Let(p, None, t, Tag::new((path.clone(), (s, e)), ast::Term::Unit)) }
            / "let" spa() __ p:pattern() _ ":" _ ty:typ() __  "=" __ t:term() _ "\n" __ c:term() { ast::Term::Let(p, Some(ty), t, c) }
            / "let" spa() __ s:position!() p:pattern() e:position!()  _ ":" _ ty:typ() __ "=" __ t:term() { ast::Term::Let(p, Some(ty), t, Tag::new((path.clone(), (s, e)), ast::Term::Unit)) }
            / "match" spa() t:term() _ "{" __ ts:lit_sep_plus(",", <lit_tup("=>", <pattern()>, <term()>)>) __ "}" { ast::Term::Match(t, ts) }
            / "fn" spa() i:ident() _ "(" _ ts:lit_sep(",", <lit_tup(":", <pattern()>, <typ()>)>) _ ")" _ "->" _ ty:typ() __ "{" __ t:term() __ "}" _ "\n" __ c:term() { ast::Term::Fun(i, ts, ty, t, c) }
            / "fn" spa() i:ident() _ "(" _ ts:lit_sep(",", <lit_tup(":", <pattern()>, <typ()>)>) _ ")" _ "->" _ ty:typ() __ "{" __ t:term() __ "}" { let i_tag = i.tag.clone(); ast::Term::Fun(i, ts, ty, t, Tag::new(i_tag, ast::Term::Unit)) }
            / "fn" spa() i:ident() _ "(" _ ts:lit_sep(",", <lit_tup(":", <pattern()>, <typ()>)>) _ ")" _ "{" __ t:term() __ "}"  _ "\n" __ c:term() { let i_tag = i.tag.clone(); ast::Term::Fun(i, ts, Tag::new(i_tag, ast::Type::Unit), t, c) }
            / "fn" spa() i:ident() _ "(" _ ts:lit_sep(",", <lit_tup(":", <pattern()>, <typ()>)>) _ ")" _ "{" __ t:term() __ "}" { let i_tag = i.tag.clone(); ast::Term::Fun(i, ts, Tag::new(i_tag.clone(), ast::Type::Unit), t, Tag::new(i_tag, ast::Term::Unit)) }
            / i:path() _ v:constructor() { let mut path = i.untag(); ast::Term::Struct(i, path, v) }
            / "|" _ ts:lit_sep(",", <lit_tup(":", <pattern()>, <typ()>)>) _ "|"  _ t:term()  { ast::Term::Lam(ts, t) }
            / "{" _ ts:lit_sep_plus(",", <lit_tup(":", <ident()>, <term()>)>) _ "}" { ast::Term::Rec(ts) }
            / "(" _ ts:lit_sep_plus(",", <term()>) _ ")" { ast::Term::Tup(ts) }
            / i:i64() { ast::Term::Int(i) }
            / "\"" s:$(quiet!{[^'"']*}) "\"" { ast::Term::Str(s.into()) }
            / "false" { ast::Term::False }
            / "true" { ast::Term::True }
            / "()" { ast::Term::Unit }
            / "(" __ t:term() __ ")" { t.into() }
            / "{" __ t:term() __ "}" { t.into() }
            / i:path() { ast::Term::Var(i)   }

        #[cache_left_rec]
        rule term() -> Term = precedence! {
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            l:(@) _ ";" __ r:@ { ast::Term::Seq(l, r) }
            l:(@) _ s:position!() ";" e:position!() { let l_tag = l.tag.clone(); ast::Term::Seq(l, Tag::new((path.clone(), (s, e)), ast::Term::Unit)) }
            --
            x:(@) __ s:position!() "==" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Eq), y) }
            x:(@) __ s:position!() "!=" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Neq), y) }
            --
            x:(@) __ s:position!() ">" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Gt), y) }
            x:(@) __ s:position!() ">=" e:position!() __ y:@  { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Gte), y) }
            x:(@) __ s:position!() "<" e:position!() __ y:@  { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Lt), y) }
            x:(@) __ s:position!() "<=" e:position!() __ y:@  { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Lte), y) }
            --
            x:(@) __ s:position!() "+" e:position!() __ y:@  { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Add), y) }
            x:(@) __ s:position!() "-" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Sub), y) }
            --
            x:(@) __ s:position!() "*" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Mul), y) }
            x:(@) __ s:position!() "/" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Div), y) }
            --
            x:(@) __ s:position!() "&" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::And), y) }
            x:(@) __ s:position!() "|" e:position!() __ y:@ { ast::Term::BinOp(x, Tag::new((path.clone(), (s, e)), ast::BinOp::Or), y) }
            --
            s:position!() "!" e:position!() _ x:term() { ast::Term::UnOp(Tag::new((path.clone(), (s, e)), ast::UnOp::Not), x) }
            s:position!() "-" e:position!() _ x:term() { ast::Term::UnOp(Tag::new((path.clone(), (s, e)), ast::UnOp::Neg), x) }
            --
            l:(@) _ "(" __ ts:lit_sep(",", <term()>) __ ")" { ast::Term::App(l, ts) }
            --
            t:(@) "." i:int() { ast::Term::TupProj(t, i) }
            t:(@) "." i:ident() { ast::Term::RecProj(t, i) }
            --
            t:_term() { t }
        }

        // program

        rule _variant() -> ast::Variant<Named>
            = "(" __ ts:lit_sep_plus(",", <typ()>) __ ")" { ast::Variant::Tup(ts) }
            / "{" __ ts:lit_sep_plus(",", <lit_tup(":", <ident()>, <typ()>)>) __ "}" { ast::Variant::Rec(ts) }

        rule variant() -> Variant = precedence! {
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            v:_variant() { v }
        }

        rule ident_variant() -> (Ident, Variant)
            = i:ident() _ v:variant() { (i, v) }
            / i:ident()  { let i_tag = i.tag.clone(); (i, Tag::new(i_tag, ast::Variant::Unit)) }

        rule top() -> Top = precedence! {
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            "use" spa() p:path() { ast::Top::Use(p) }
            "struct" spa() i:ident() _ v:variant() { ast::Top::Struct(i, v) }
            "struct" spa() i:ident() s:position!() { let i_tag = i.tag.clone(); ast::Top::Struct(i, Tag::new(i_tag, ast::Variant::Unit)) }
            "enum" spa() i:ident() _ "{" __ ts:lit_sep_plus(",", <ident_variant()>) __ "}" { ast::Top::Enum(i, ts) }
            "fn" spa() i:ident() _ "(" __ ts:lit_sep(",", <lit_tup(":", <pattern()>, <typ()>)>) __ ")" _ "->" _ ty:typ() __ "{" __ t:term() __ "}" { ast::Top::Fun(i, ts, ty, t) }
            "fn" spa() i:ident() _ "(" __ ts:lit_sep(",", <lit_tup(":", <pattern()>, <typ()>)>) __ ")" _ "{" __ t:term() __ "}" { let i_tag = i.tag.clone(); ast::Top::Fun(i, ts, Tag::new(i_tag, ast::Type::Unit), t) }
            "fn" spa() i:ident() _ "(" __ ts:lit_sep(",", <lit_tup(":", <ident()>, <typ()>)>) __ ")" _ "->" _ ty:typ() __ "{~" s:position!() c:$(quiet!{[^'~']*}) e:position!() "~}" { ast::Top::FFIFun(i, ts, ty, Tag::new((path.clone(), (s, e)), c.into())) }
            "fn" spa() i:ident() _ "(" __ ts:lit_sep(",", <lit_tup(":", <ident()>, <typ()>)>) __ ")" _ "{~" s:position!() c:$(quiet!{[^'~']*}) e:position!() "~}" { let i_tag = i.tag.clone(); ast::Top::FFIFun(i, ts, Tag::new(i_tag, ast::Type::Unit), Tag::new((path.clone(), (s, e)), c.into())) }
            "type" spa() i:ident() _ "=" _ t:typ() { ast::Top::Alias(i, t) }
        }

        rule rm<T>(x: rule<T>) -> T
            = x:x() { x }
            / __ "/*" consume() x:x() { x }

        #[cache_left_rec]
        rule consume()
            = quiet!{[^'*']*} "*/" __
            / consume()

        pub rule program() -> Program = precedence! {
            s:position!() t:@ e:position!() { Tag::new((path.clone(), (s, e)), t) }
            --
            __ ts:(rm(<top()>) ** (sep() __)) __? { ts }
        }
    }
}
