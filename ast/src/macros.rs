#[macro_export]
macro_rules! path {
    ($x:expr) => {{
        let prefix = $x.code_ref().0.last().unwrap().clone();
        vec![prefix, $x.to_string().clone()]
    }};
}

#[macro_export]
macro_rules! def_from_to_ast_types {
    (from => $i:ident, to => $o:ident, prefix => $p:ident) => {
        type FromIdent = $p::_Ident<$p::$i>;
        type FromInt = $p::_Int<$p::$i>;
        type FromVec<T> = $p::_Vec<$p::$i, T>;
        type FromType = $p::_Type<$p::$i>;
        type FromBinOp = $p::_BinOp<$p::$i>;
        type FromUnOp = $p::_UnOp<$p::$i>;
        type FromConstructor = $p::_Constructor<$p::$i>;
        type FromPattern = $p::_Pattern<$p::$i>;
        type FromTerm = $p::_Term<$p::$i>;
        type FromBody = $p::_Body<$p::$i>;
        type FromValue = $p::_Value<$p::$i>;
        type FromVariant = $p::_Variant<$p::$i>;
        type FromTop = $p::_Top<$p::$i>;
        type FromProgram = $p::_Program<$p::$i>;
        type FromPath = $p::_Path<$p::$i>;

        type ToIdent = $p::_Ident<$p::$o>;
        type ToInt = $p::_Int<$p::$o>;
        type ToVec<T> = $p::_Vec<$p::$o, T>;
        type ToType = $p::_Type<$p::$o>;
        type ToBinOp = $p::_BinOp<$p::$o>;
        type ToUnOp = $p::_UnOp<$p::$o>;
        type ToConstructor = $p::_Constructor<$p::$o>;
        type ToPattern = $p::_Pattern<$p::$o>;
        type ToTerm = $p::_Term<$p::$o>;
        type ToBody = $p::_Body<$p::$o>;
        type ToValue = $p::_Value<$p::$o>;
        type ToVariant = $p::_Variant<$p::$o>;
        type ToTop = $p::_Top<$p::$o>;
        type ToProgram = $p::_Program<$p::$o>;
        type ToPath = $p::_Path<$p::$o>;
    };
}

#[macro_export]
macro_rules! def_ast_types {
    (type => $i:ident, prefix => $p:ident) => {
        type Ident = $p::_Ident<$p::$i>;
        type Int = $p::_Int<$p::$i>;
        type Vec<T> = $p::_Vec<$p::$i, T>;
        type Type = $p::_Type<$p::$i>;
        type BinOp = $p::_BinOp<$p::$i>;
        type UnOp = $p::_UnOp<$p::$i>;
        type Constructor = $p::_Constructor<$p::$i>;
        type Pattern = $p::_Pattern<$p::$i>;
        type Term = $p::_Term<$p::$i>;
        type Body = $p::_Body<$p::$i>;
        type Value = $p::_Value<$p::$i>;
        type Variant = $p::_Variant<$p::$i>;
        type Top = $p::_Top<$p::$i>;
        type Program = $p::_Program<$p::$i>;
        type Path = $p::_Path<$p::$i>;
    };
}
