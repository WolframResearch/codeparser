use crate::{
    cst::{BinaryNode, CompoundNode, Cst, OperatorNode},
    macros::{src, token},
    parse::operators::{BinaryOperator, CompoundOperator},
    parse_cst,
    source::Span,
    tests::assert_src,
    NodeSeq, ParseOptions,
};

use pretty_assertions::assert_eq;


#[test]
fn NodeTest_Bug1() {
    let input = "a_.";

    let NodeSeq(tokens) = crate::tokenize(input, &ParseOptions::default());

    assert_eq!(
        tokens,
        vec![
            token!(Symbol, "a", src!(1:1-1:2)),
            token!(UnderDot, "_.", src!(1:2-1:4))
        ]
    );

    let [T1, T2] = tokens.try_into().unwrap();

    let N = CompoundNode::new2(
        CompoundOperator::CodeParser_PatternOptionalDefault,
        T1,
        T2,
    );

    let NSource = Cst::Compound(N).get_source();

    assert_eq!(NSource.start(), src!(1:1).into());
    assert_eq!(NSource.end(), src!(1:4).into());

    // FIXME: Check that no issues were generated; make tokenize() return a
    //        ParseResult
    // assert_eq!(session.non_fatal_issues().len(), 0);
    // assert_eq!(session.fatal_issues().len(), 0);
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
