#![allow(non_upper_case_globals)]

use wolfram_expr::symbol::SymbolRef;

pub type Symbol = SymbolRef<'static>;


//==========================================================
// Symbol constant declarations
//==========================================================

macro_rules! Symbol {
    ($name:ident) => {
        pub const $name: Symbol =
            unsafe { Symbol::unchecked_new(concat!("System`", stringify!($name))) };
    };

    ($($name:ident);* $(;)?) => {
        $(
            Symbol!($name);
        )*
    };
}

macro_rules! Symbol2 {
    ($context:ident :: { $($name:ident),* }) => {
        pub mod $context {
        $(
            pub const $name: $crate::symbol::Symbol = unsafe {
                $crate::symbol::Symbol::unchecked_new(concat!(
                    stringify!($context),
                    "`",
                    stringify!($name)
                ))
            };
        )*
        }
    };

    ($($($context:ident ::)+ $name:ident);* $(;)?) => {
        $(
            Symbol!($($context ::)* $name);
        )*
    };
}

//======================================
// Constants
//======================================

Symbol! {
    Inequality;

    Equal;
    Unequal;
    Less;
    Greater;
    LessEqual;
    GreaterEqual;
    GreaterEqualLess;
    GreaterFullEqual;
    GreaterGreater;
    GreaterLess;
    GreaterSlantEqual;
    LessSlantEqual;
    //
    // GreaterSlantEqual parses to GreaterEqual
    // Related bugs: 78439
    //
    GreaterTilde;
    LessEqualGreater;
    LessFullEqual;
    LessGreater;
    LessLess;
    //
    // LessSlantEqual parses to LessEqual
    // Related bugs: 78439
    //
    LessTilde;
    NestedGreaterGreater;
    NestedLessLess;
    NotGreater;
    NotGreaterEqual;
    NotGreaterFullEqual;
    NotGreaterGreater;
    NotGreaterLess;
    NotGreaterSlantEqual;
    NotGreaterTilde;
    NotLess;
    NotLessEqual;
    NotLessFullEqual;
    NotLessGreater;
    NotLessLess;
    NotLessSlantEqual;
    NotLessTilde;
    NotNestedGreaterGreater;
    NotNestedLessLess;

    VectorLess;
    VectorGreater;
    VectorLessEqual;
    VectorGreaterEqual;
}


Symbol2! {
    Developer::{
        VectorInequality
    }
}
