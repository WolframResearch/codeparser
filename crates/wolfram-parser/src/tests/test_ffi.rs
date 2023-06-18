//! Test that types used in this crate's LibraryLink API have a stable
//! representation. If these tests fail, that implies that a version of this
//! crate is not backwards compatible with the version the tests were initially
//! written for.

use pretty_assertions::assert_eq;

use crate::{EncodingMode, FirstLineBehavior, SourceConvention, StringifyMode};

#[test]
fn public_enum_values() {
    assert_eq!(FirstLineBehavior::NotScript as i32, 0);
    assert_eq!(FirstLineBehavior::Check as i32, 1);
    assert_eq!(FirstLineBehavior::Script as i32, 2);

    assert_eq!(EncodingMode::Normal as i32, 0);
    assert_eq!(EncodingMode::Box as i32, 1);

    assert_eq!(StringifyMode::Normal as i32, 0);
    assert_eq!(StringifyMode::Tag as i32, 1);
    assert_eq!(StringifyMode::File as i32, 2);

    assert_eq!(SourceConvention::LineColumn as i32, 0);
    assert_eq!(SourceConvention::CharacterIndex as i32, 1);
}
