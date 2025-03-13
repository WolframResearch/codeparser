use std::os::raw::c_int;

/// A kernel symbol
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: &'static str,
    pub id: c_int,
}

// impl PartialEq for Symbol {
//     fn eq(&self, other: &Symbol) -> bool {
//         self.id == other.id
//     }
// }

// TODO: Display
// void Symbol::print(std::ostream& s) const {
//     s << Name;
// }

impl Symbol {
    pub const fn new(name: &'static str, id: c_int) -> Self {
        Symbol { name, id }
    }
}
