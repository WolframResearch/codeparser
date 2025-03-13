use pretty_assertions::assert_eq;

use crate::{
    cst::{
        CompoundNode, CompoundOperator,
        Cst::{self, Token},
        InfixNode, InfixOperator, OperatorNode, PrefixNode, PrefixOperator,
    },
    macros::{src, token},
    parse_cst, NodeSeq,
};

// TODO(test): Test every value in token_kind.rs ERROR list

#[test]
fn test_prefix_expected_file_and_operand_errors() {
    assert_eq!(
        // <<< is not a token, it is tokenized as `a` `<<` and then an illegal
        // start token.
        parse_cst("<<", &Default::default()).syntax,
        Cst::Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Get,
            children: NodeSeq(vec![
                Token(token!(LessLess, "<<", 1:1-3)),
                Token(token!(Error_ExpectedFile, "", 1:3-3))
            ]),
            src: src!(1:1-3).into()
        }))
    );

    assert_eq!(
        // <<< is not a token, it is tokenized as `<<` and then an illegal
        // start token.
        parse_cst("<<<", &Default::default()).syntax,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::CodeParser_InfixInequality,
            children: NodeSeq(vec![
                Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Get,
                    children: NodeSeq(vec![
                        Token(token!(LessLess, "<<", 1:1-3)),
                        Token(token!(Error_ExpectedFile, "", 1:3-3)),
                    ]),
                    src: src!(1:1-3).into(),
                })),
                Token(token!(Less, "<", 1:3-4)),
                Token(token!(Error_ExpectedOperand, "", 1:4-4)),
            ]),
            src: src!(1:1-4).into(),
        }))
    );
}

#[test]
fn test_missing_tag_and_missing_operand() {
    assert_eq!(
        // <<< is not a token, it is tokenized as `<<` and then an illegal
        // start token.
        parse_cst("a::<", &Default::default()).syntax,
        Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::CodeParser_InfixInequality,
            children: NodeSeq(vec![
                Cst::Infix(InfixNode(OperatorNode {
                    op: InfixOperator::MessageName,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(ColonColon, "::", 1:2-4)),
                        Token(token!(Error_ExpectedTag, "", 1:4-4)),
                    ]),
                    src: src!(1:1-4).into(),
                })),
                Token(token!(Less, "<", 1:4-5)),
                Token(token!(Error_ExpectedOperand, "", 1:5-5)),
            ]),
            src: src!(1:1-5).into(),
        }))
    );
}

#[test]
fn test_expected_letterlike_after_blank() {
    // TID:231016/1
    assert_eq!(
        parse_cst("_a`", &Default::default()).syntax,
        Cst::Compound(CompoundNode(OperatorNode {
            op: CompoundOperator::Blank,
            children: NodeSeq(vec![
                Token(token!(Under, "_", 1:1-2)),
                Token(token!(Error_ExpectedLetterlike, "a`", 1:2-4)),
            ]),
            src: src!(1:1-4).into(),
        }))
    );

    // TID:231016/2
    assert_eq!(
        parse_cst("a_b`", &Default::default()).syntax,
        Cst::Compound(CompoundNode(OperatorNode {
            op: CompoundOperator::CodeParser_PatternBlank,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Cst::Compound(CompoundNode(OperatorNode {
                    op: CompoundOperator::Blank,
                    children: NodeSeq(vec![
                        Token(token!(Under, "_", 1:2-3)),
                        Token(token!(Error_ExpectedLetterlike, "b`", 1:3-5)),
                    ]),
                    src: src!(1:2-5).into()
                }))
            ]),
            src: src!(1:1-5).into(),
        }))
    );
}
