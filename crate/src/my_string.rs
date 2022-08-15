use std::os::raw::c_int;

#[derive(Debug, Clone, Hash)]
pub struct MyString {
    pub(crate) val: &'static str, // const char *Val;
    pub(crate) id: c_int,
}

impl MyString {
    pub const fn new(val: &'static str, id: c_int) -> MyString {
        MyString { val, id }
    }
}

impl PartialEq for MyString {
    fn eq(&self, other: &MyString) -> bool {
        let MyString { val: _, id } = *self;

        id == other.id
    }
}
