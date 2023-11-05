#![allow(non_upper_case_globals)]

use wolfram_expr::symbol::SymbolRef;

pub type Symbol = SymbolRef<'static>;


//==========================================================
// Symbol constant declarations
//==========================================================

macro_rules! symbol {
    ($name:ident) => {
        pub const $name: Symbol =
            unsafe { Symbol::unchecked_new(concat!("System`", stringify!($name))) };
    };

    ($($name:ident);* $(;)?) => {
        $(
            $crate::symbol::symbol!($name);
        )*
    };
}

macro_rules! nested_symbol {
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
            symbol!($($context ::)* $name);
        )*
    };
}


pub(crate) use {nested_symbol, symbol};
