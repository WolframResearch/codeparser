use crate::tokenize::TokenKind;


#[test]
fn TokenEnumTest_Trivia() {
    assert!(TokenKind::Whitespace.isTrivia());
}

#[test]
fn TokenEnumTest_PossibleBeginning() {
    assert!(TokenKind::Symbol.isPossibleBeginning());

    assert!(TokenKind::SemiSemi.isPossibleBeginning());
}

#[test]
fn TokenEnumTest_Closer() {
    assert!(TokenKind::CloseSquare.isCloser());

    assert!(TokenKind::LongName_RightCeiling.isCloser());
}

#[test]
fn TokenEnumTest_Error() {
    assert!(TokenKind::Error_ExpectedTag.isError());

    assert!(TokenKind::Error_UnsupportedToken.isError());
}

#[test]
fn TokenEnumTest_Unterminated() {
    assert!(TokenKind::Error_UnterminatedString.isUnterminated());

    assert!(TokenKind::Error_UnterminatedComment.isUnterminated());
}

#[test]
fn TokenEnumTest_Empty() {
    assert!(TokenKind::EndOfFile.isEmpty());

    assert!(TokenKind::Error_ExpectedOperand.isEmpty());
}
