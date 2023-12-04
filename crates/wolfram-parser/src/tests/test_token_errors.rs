use pretty_assertions::assert_eq;

use crate::{
    cst::{
        BinaryNode, CallBody, CallHead, CallNode, CompoundNode,
        Cst::{self, Token},
        GroupNode, InfixNode, OperatorNode, PrefixNode,
    },
    macros::{src, token},
    parse::operators::{
        BinaryOperator, CallOperator, CompoundOperator, InfixOperator,
        PrefixOperator,
    },
    parse_cst, NodeSeq,
};

use super::{assert_cst, assert_src};

// TODO(test): Test every value in token_kind.rs ERROR list

#[test]
fn test_prefix_expected_file_and_operand_errors() {
    assert_eq!(
        // <<< is not a token, it is tokenized as `a` `<<` and then an illegal
        // start token.
        parse_cst("<<", &Default::default()).syntax,
        assert_src!(1:1-3 => Cst::Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Get,
            children: NodeSeq(vec![
                Token(token!(LessLess, "<<", 1:1-3)),
                Token(token!(Error_ExpectedFile, "", 1:3-3))
            ]),
        })))
    );

    assert_eq!(
        // <<< is not a token, it is tokenized as `<<` and then an illegal
        // start token.
        parse_cst("<<<", &Default::default()).syntax,
        assert_src!(1:1-4 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::CodeParser_InfixInequality,
            children: NodeSeq(vec![
                assert_src!(1:1-3 => Cst::Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Get,
                    children: NodeSeq(vec![
                        Token(token!(LessLess, "<<", 1:1-3)),
                        Token(token!(Error_ExpectedFile, "", 1:3-3)),
                    ]),
                }))),
                Token(token!(Less, "<", 1:3-4)),
                Token(token!(Error_ExpectedOperand, "", 1:4-4)),
            ]),
        })))
    );
}

#[test]
fn test_missing_tag_and_missing_operand() {
    assert_eq!(
        // <<< is not a token, it is tokenized as `<<` and then an illegal
        // start token.
        parse_cst("a::<", &Default::default()).syntax,
        assert_src!(1:1-5 => Cst::Infix(InfixNode(OperatorNode {
            op: InfixOperator::CodeParser_InfixInequality,
            children: NodeSeq(vec![
                assert_src!(1:1-4 => Cst::Infix(InfixNode(OperatorNode {
                    op: InfixOperator::MessageName,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(ColonColon, "::", 1:2-4)),
                        Token(token!(Error_ExpectedTag, "", 1:4-4)),
                    ]),
                }))),
                Token(token!(Less, "<", 1:4-5)),
                Token(token!(Error_ExpectedOperand, "", 1:5-5)),
            ]),
        })))
    );
}

#[test]
fn test_expected_letterlike_after_blank() {
    // TID:231016/1
    assert_eq!(
        parse_cst("_a`", &Default::default()).syntax,
        assert_src!(1:1-4 => Cst::Compound(CompoundNode(OperatorNode {
            op: CompoundOperator::Blank,
            children: NodeSeq(vec![
                Token(token!(Under, "_", 1:1-2)),
                Token(token!(Error_ExpectedLetterlike, "a`", 1:2-4)),
            ]),
        })))
    );

    // TID:231016/2
    assert_eq!(
        parse_cst("a_b`", &Default::default()).syntax,
        assert_src!(1:1-5 => Cst::Compound(CompoundNode(OperatorNode {
            op: CompoundOperator::CodeParser_PatternBlank,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                assert_src!(1:2-5 => Cst::Compound(CompoundNode(OperatorNode {
                    op: CompoundOperator::Blank,
                    children: NodeSeq(vec![
                        Token(token!(Under, "_", 1:2-3)),
                        Token(token!(Error_ExpectedLetterlike, "b`", 1:3-5)),
                    ]),
                })))
            ]),
        })))
    );
}

#[test]
fn test_prefix_comma_errors() {
    // TID:231016/3
    assert_cst!(
        parse_cst("f[a@,2]", &Default::default()).syntax,
        Cst::Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Token(
                token!(Symbol, "f", 1:1-2),
            )])),
            body: CallBody::Group(
                assert_src!(1:2-8 => GroupNode(OperatorNode {
                    op: CallOperator::CodeParser_GroupSquare,
                    children: NodeSeq(vec![
                        Token(token!(OpenSquare, "[", 1:2-3),),
                        assert_src!(1:3-7 => Cst::Infix(InfixNode(OperatorNode {
                            op: InfixOperator::CodeParser_Comma,
                            children: NodeSeq(vec![
                                assert_src!(1:3-5 => Cst::Binary(BinaryNode(OperatorNode {
                                    op: BinaryOperator::CodeParser_BinaryAt,
                                    children: NodeSeq(vec![
                                        Token(token!(Symbol, "a", 1:3-4)),
                                        Token(token!(At, "@", 1:4-5)),
                                        Token(
                                            token!(Error_ExpectedOperand, "", 1:5-5)
                                        ),
                                    ]),
                                }))),
                                Token(token!(Comma, ",", 1:5-6)),
                                Token(token!(Integer, "2", 1:6-7)),
                            ]),
                        }))),
                        Token(token!(CloseSquare, "]", 1:7-8)),
                    ]),
                }))
            ),
        }),
        src!(1:1-8).into()
    );

    // TID:231016/4
    assert_cst!(
        parse_cst("f[,2]", &Default::default()).syntax,
        Cst::Call(CallNode {
            head: CallHead::Concrete(NodeSeq(vec![Token(
                token!(Symbol, "f", 1:1-2),
            )])),
            body: assert_src!(1:2-6 => CallBody::Group(GroupNode(OperatorNode {
                op: CallOperator::CodeParser_GroupSquare,
                children: NodeSeq(vec![
                    Token(token!(OpenSquare, "[", 1:2-3),),
                    assert_src!(1:3-5 => Cst::Infix(InfixNode(OperatorNode {
                        op: InfixOperator::CodeParser_Comma,
                        children: NodeSeq(vec![
                            Token(token!(Error_PrefixImplicitNull, "", 1:3-3)),
                            Token(token!(Comma, ",", 1:3-4)),
                            Token(token!(Integer, "2", 1:4-5)),
                        ]),
                    }))),
                    Token(token!(CloseSquare, "]", 1:5-6)),
                ]),
            }))),
        }),
        src!(1:1-6).into()
    );
}
