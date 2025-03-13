use crate::token_enum::TokenEnum::*;


#[test]
fn TokenEnumTest_Trivia() {
    assert!(TOKEN_WHITESPACE.isTrivia());
}

#[test]
fn TokenEnumTest_PossibleBeginning() {
    assert!(TOKEN_SYMBOL.isPossibleBeginning());

    assert!(TOKEN_SEMISEMI.isPossibleBeginning());
}

#[test]
fn TokenEnumTest_Closer() {
    assert!(TOKEN_CLOSESQUARE.isCloser());

    assert!(TOKEN_LONGNAME_RIGHTCEILING.isCloser());
}

#[test]
fn TokenEnumTest_Error() {
    assert!(TOKEN_ERROR_EXPECTEDTAG.isError());

    assert!(TOKEN_ERROR_UNSUPPORTEDTOKEN.isError());
}

#[test]
fn TokenEnumTest_Unterminated() {
    assert!(TOKEN_ERROR_UNTERMINATEDSTRING.isUnterminated());

    assert!(TOKEN_ERROR_UNTERMINATEDCOMMENT.isUnterminated());
}

#[test]
fn TokenEnumTest_Empty() {
    assert!(TOKEN_ENDOFFILE.isEmpty());

    assert!(TOKEN_ERROR_EXPECTEDOPERAND.isEmpty());
}
