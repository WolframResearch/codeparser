use crate::{
    cst::{
        BinaryNode, BinaryOperator, CompoundNode, CompoundOperator, Cst,
        OperatorNode,
    },
    macros::{src, token},
    source::Location,
    tests::nodes,
    NodeSeq, ParseOptions, ParserSession, Span,
};

use pretty_assertions::assert_eq;


#[test]
fn NodeTest_Bug1() {
    let mut Args: NodeSeq<_> = NodeSeq::new();

    let input = "a_.";

    let session =
        ParserSession::new(input.as_bytes(), &ParseOptions::default());

    let T1 = token!(Symbol, "a", src!(1:1-1:2));
    Args.push(Cst::Token(T1));

    let T2 = token!(UnderDot, "_.", src!(1:2-1:4));
    Args.push(Cst::Token(T2));

    let N = CompoundNode::new(
        CompoundOperator::CodeParser_PatternOptionalDefault,
        Args,
    );

    let NSource = Cst::Compound(N).getSource();

    assert_eq!(NSource.start, Location::new(1, 1));
    assert_eq!(NSource.end, Location::new(1, 4));

    assert_eq!(session.nonFatalIssues().len(), 0);
    assert_eq!(session.fatalIssues().len(), 0);
}

#[test]
fn test_parse_span() {
    // Binary Span with implicit 1st arg
    assert_eq!(
        nodes(";; b"),
        vec![Cst::Binary(BinaryNode(OperatorNode {
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
            src: Span::from(src!(1:1-1:5))
        }))]
    );


    // Binary Span
    assert_eq!(
        nodes("a ;; b"),
        vec![Cst::Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Span,
            children: NodeSeq(vec![
                Cst::Token(token![Symbol, "a", Span::from(src!(1:1-1:2))]),
                Cst::Token(token![Whitespace, " ", Span::from(src!(1:2-1:3))]),
                Cst::Token(token![SemiSemi, ";;", Span::from(src!(1:3-1:5))]),
                Cst::Token(token![Whitespace, " ", Span::from(src!(1:5-1:6))]),
                Cst::Token(token![Symbol, "b", Span::from(src!(1:6-1:7))]),
            ]),
            src: Span::from(src!(1:1-1:7))
        }))]
    );
}
