use crate::{
    cst::{
        BinaryNode, BinaryOperator, CompoundNode, CompoundOperator, Cst,
        OperatorNode,
    },
    macros::{src, token},
    parse_cst,
    source::Span,
    tests::assert_src,
    NodeSeq, ParseOptions, ParserSession,
};

use pretty_assertions::assert_eq;


#[test]
fn NodeTest_Bug1() {
    let input = "a_.";

    let session =
        ParserSession::new(input.as_bytes(), &ParseOptions::default());

    let T1 = token!(Symbol, "a", src!(1:1-1:2));
    let T2 = token!(UnderDot, "_.", src!(1:2-1:4));

    let N = CompoundNode::new2(
        CompoundOperator::CodeParser_PatternOptionalDefault,
        T1,
        T2,
    );

    let NSource = Cst::Compound(N).get_source();

    assert_eq!(NSource.start(), src!(1:1).into());
    assert_eq!(NSource.end(), src!(1:4).into());

    assert_eq!(session.non_fatal_issues().len(), 0);
    assert_eq!(session.fatal_issues().len(), 0);
}

#[test]
fn test_parse_span() {
    // Binary Span with implicit 1st arg
    assert_eq!(
        parse_cst(";; b", &Default::default()).syntax,
        assert_src!(1:1-5 => Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Span,
            children: NodeSeq(vec![
                Cst::Token(token![
                    Fake_ImplicitOne,
                    "",
                    Span::from(src!(1:1-1:1))
                ]),
                Cst::Token(token![SemiSemi, ";;", Span::from(src!(1:1-1:3))]),
                Cst::Token(token![Whitespace, " ", Span::from(src!(1:3-1:4))]),
                Cst::Token(token![Symbol, "b", Span::from(src!(1:4-1:5))]),
            ]),
        })))
    );


    // Binary Span
    assert_eq!(
        parse_cst("a ;; b", &Default::default()).syntax,
        assert_src!(1:1-7 => Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Span,
            children: NodeSeq(vec![
                Cst::Token(token![Symbol, "a", Span::from(src!(1:1-1:2))]),
                Cst::Token(token![Whitespace, " ", Span::from(src!(1:2-1:3))]),
                Cst::Token(token![SemiSemi, ";;", Span::from(src!(1:3-1:5))]),
                Cst::Token(token![Whitespace, " ", Span::from(src!(1:5-1:6))]),
                Cst::Token(token![Symbol, "b", Span::from(src!(1:6-1:7))]),
            ]),

        })))
    );
}
