use crate::wl_character::WLCharacter;

#[test]
fn SourceCharacterTest_Graphical1() {
    let mut C = WLCharacter::new('\t');

    assert_eq!(C.graphicalString(), "\\t");

    C = WLCharacter::new(0x1b);

    assert_eq!(C.graphicalString(), "\\[RawEscape]");
}
