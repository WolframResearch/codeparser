use crate::{
    cst::{
        CallBody, CallHead, CallNode, CallOperator as CallOp,
        Cst::{Call, Ternary, Token},
        GroupNode, OperatorNode, TernaryNode, TernaryOperator as TernaryOp,
    },
    macros::{src, token},
    parse_cst,
    tests::assert_src,
    NodeSeq,
};

use pretty_assertions::assert_eq;

#[test]
fn ternary_tag_unset() {
    //==================================
    // Typical TagUnset (TID:231105/1)
    //==================================

    assert_eq!(
        parse_cst("a /: b[a] =.", &Default::default()).syntax,
        assert_src!(1:1-13 => Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::TagUnset,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(SlashColon, "/:", 1:3-5)),
                Token(token!(Whitespace, " ", 1:5-6)),
                assert_src!(1:6-10 => Call(CallNode {
                    head: CallHead::Concrete(NodeSeq(vec![Token(
                        token!(Symbol, "b", 1:6-7),
                    )])),
                    body: assert_src!(1:7-10 => CallBody::Group(GroupNode(OperatorNode {
                        op: CallOp::CodeParser_GroupSquare,
                        children: NodeSeq(vec![
                            Token(token!(OpenSquare, "[", 1:7-8)),
                            Token(token!(Symbol, "a", 1:8-9)),
                            Token(token!(CloseSquare, "]", 1:9-10)),
                        ]),
                    }))),
                })),
                Token(token!(Whitespace, " ", 1:10-11)),
                Token(token!(Equal, "=", 1:11-12)),
                Token(token!(Dot, ".", 1:12-13)),
            ]),
        })))
    );

    //=====================================
    // TagUnset with interior trivia ("= .") (TID:231105/2)
    //=====================================

    assert_eq!(
        parse_cst("a /: b[a] = .", &Default::default()).syntax,
        assert_src!(1:1-14 => Ternary(TernaryNode(OperatorNode {
            op: TernaryOp::TagUnset,
            children: NodeSeq(vec![
                Token(token!(Symbol, "a", 1:1-2)),
                Token(token!(Whitespace, " ", 1:2-3)),
                Token(token!(SlashColon, "/:", 1:3-5)),
                Token(token!(Whitespace, " ", 1:5-6)),
                assert_src!(1:6-10 => Call(CallNode {
                    head: CallHead::Concrete(NodeSeq(vec![Token(
                        token!(Symbol, "b", 1:6-7),
                    )])),
                    body: assert_src!(1:7-10 => CallBody::Group(GroupNode(OperatorNode {
                        op: CallOp::CodeParser_GroupSquare,
                        children: NodeSeq(vec![
                            Token(token!(OpenSquare, "[", 1:7-8)),
                            Token(token!(Symbol, "a", 1:8-9)),
                            Token(token!(CloseSquare, "]", 1:9-10)),
                        ]),
                    }))),
                })),
                Token(token!(Whitespace, " ", 1:10-11)),
                Token(token!(Equal, "=", 1:11-12)),
                Token(token!(Whitespace, " ", 1:12-13)),
                Token(token!(Dot, ".", 1:13-14)),
            ]),
        })))
    )
}
