use crate::read::WLCharacter;

#[test]
fn SourceCharacterTest_Graphical1() {
    assert_eq!(WLCharacter::new('\t').graphicalString(), "\\t");

    assert_eq!(WLCharacter::new(0x1b).graphicalString(), "\\[RawEscape]");

    assert_eq!(WLCharacter::new(0xb0).graphicalString(), "\\[Degree]");

    assert_eq!(WLCharacter::new('\u{abcd}').graphicalString(), "\\:abcd");
}
