#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MyString(pub &'static str);

impl MyString {
    pub const fn new(val: &'static str) -> MyString {
        MyString(val)
    }
}
