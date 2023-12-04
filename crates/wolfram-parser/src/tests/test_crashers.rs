use crate::{
    cst::{
        BinaryNode,
        Cst::{
            self, Binary, GroupMissingCloser, Infix, Prefix, SyntaxError, Token,
        },
        GroupMissingCloserNode, InfixNode, OperatorNode, PrefixNode,
        SyntaxErrorNode,
    },
    issue::{Issue, IssueTag, Severity},
    macros::{src, token},
    parse::{
        operators::{
            BinaryOperator, GroupOperator, InfixOperator, PrefixOperator,
        },
        SyntaxErrorKind,
    },
    parse_bytes_cst, parse_cst,
    source::{Source, Span},
    symbols as sym,
    tokenize::Tokenizer,
    NodeSeq, ParseOptions, SourceConvention,
};

use pretty_assertions::assert_eq;


#[test]
fn CrashTest_Crash0_tokens() {
    let bufAndLen: &[u8] = b"1\\\n";

    let mut tokenizer = Tokenizer::new(bufAndLen, &ParseOptions::default());

    let mut tok = tokenizer.peek_token();

    assert_eq!(tok, token!(Integer, "1", src!(1:1-1:2)));

    let _ = tokenizer.next_token();

    tok = tokenizer.peek_token();

    assert_eq!(tok, token!(EndOfFile, "\\\n", src!(1:2-2:1)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn CrashTest_Crash1() {
    let result = parse_cst("1::*\\\r\n", &ParseOptions::default());

    assert_eq!(
        result.syntax,
        Infix(InfixNode(OperatorNode {
            op: InfixOperator::Times,
            children: NodeSeq(vec![
                Infix(InfixNode(OperatorNode {
                    op: InfixOperator::MessageName,
                    children: NodeSeq(vec![
                        Token(token!(Integer, "1", 1:1-2)),
                        Token(token!(ColonColon, "::", 1:2-4)),
                        Token(token!(Error_ExpectedTag, "", 1:4-4)),
                    ]),
                })),
                Token(token!(Star, "*", 1:4-5)),
                Token(token!(Error_ExpectedOperand, "", 1:5-5)),
            ]),
        }))
    );

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

/*
#if 0
#[test]
fn CrashTest_StackOverflow1() {

    unsigned char arr[1600];
    for (let i = 0; i < 1600 ; i++){
        arr[i] = '(';
    }

    let bufAndLen = BufferAndLength(arr, 1600);

    TheParserSession->init(bufAndLen, nullptr, SourceConvention::LineColumn);

    let N = TheParserSession->parseExpressions();

    TheParserSession->releaseNode(N);

    assert_eq!(session.non_fatal_issues().len(), 0);
    assert_eq!(session.fatal_issues().len(), 0);

    TheParserSession->deinit();
}
#endif // #if 0
*/

#[test]
fn CrashTest_Crash2() {
    let bufAndLen = "\\:feff";

    let result = parse_cst(bufAndLen, &ParseOptions::default());

    assert_eq!(result.syntax, Cst::Token(token!(Symbol, "\\:feff", 1:1-7)));

    assert_eq!(
        result.non_fatal_issues,
        vec![Issue {
            make_sym: sym::CodeParser_SyntaxIssue,
            tag: IssueTag::UnexpectedLetterlikeCharacter,
            msg: "Unexpected letterlike character: ``\\:feff``.".to_owned(),
            sev: Severity::Warning,
            src: Source::Span(Span::from(src!(1:1-7))),
            val: 0.8,
            actions: vec![],
            additional_descriptions: vec![],
            additional_sources: vec![],
        }]
    );
    assert_eq!(result.fatal_issues, Vec::new());
}

#[test]
fn CrashTest_Crash3() {
    let bufAndLen = "a:b~1:2";

    let result = parse_cst(bufAndLen, &ParseOptions::default());

    assert_eq!(
        result.syntax,
        Binary(BinaryNode(OperatorNode {
            op: BinaryOperator::Optional,
            children: NodeSeq(vec![
                Binary(BinaryNode(OperatorNode {
                    op: BinaryOperator::Pattern,
                    children: NodeSeq(vec![
                        Token(token!(Symbol, "a", 1:1-2)),
                        Token(token!(Colon, ":", 1:2-3)),
                        SyntaxError(SyntaxErrorNode {
                            err: SyntaxErrorKind::ExpectedTilde,
                            children: NodeSeq(vec![
                                Token(token!(Symbol, "b", 1:3-4)),
                                Token(token!(Tilde, "~", 1:4-5)),
                                Token(token!(Integer, "1", 1:5-6)),
                            ]),
                        }),
                    ])
                })),
                Token(token!(Colon, ":", 1:6-7)),
                Token(token!(Integer, "2", 1:7-8)),
            ])
        }))
    );

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

#[test]
fn CrashTest_Crash4() {
    let bufAndLen: &[u8] = &[
        b'\\', b'[', b'I', b'n', b't', b'e', b'g', b'r', b'a', b'l', b']',
        b'\\', b'[', b'S', b'u', b'm', b']',
    ];

    let result = parse_bytes_cst(bufAndLen, &ParseOptions::default());

    assert_eq!(
        result.syntax,
        Prefix(PrefixNode(OperatorNode {
            op: PrefixOperator::Integral,
            children: NodeSeq(vec![
                Token(token!(LongName_Integral, "\\[Integral]", 1:1-12)),
                Prefix(PrefixNode(OperatorNode {
                    op: PrefixOperator::Sum,
                    children: NodeSeq(vec![
                        Token(token!(LongName_Sum, "\\[Sum]", 1:12-18)),
                        Token(token!(Error_ExpectedOperand, "", 1:18-18)),
                    ])
                })),
            ])
        }))
    );

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

#[test]
fn CrashTest_Crash5() {
    let bufAndLen = b"{\t1\\\n^";

    let result = parse_bytes_cst(
        bufAndLen,
        &ParseOptions::default()
            .source_convention(SourceConvention::CharacterIndex),
    );

    assert_eq!(
        result.syntax,
        GroupMissingCloser(GroupMissingCloserNode(OperatorNode {
            op: GroupOperator::List,
            children: NodeSeq(vec![
                Token(token!(OpenCurly, "{", 1..2)),
                Token(token!(Whitespace, "\t", 2..3)),
                Token(token!(Integer, "1", 3..4)),
                Token(token!(Caret, "\\\n^", 4..7)),
                Token(token!(Error_ExpectedOperand, "", 7..7)),
            ]),
        }))
    );

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}
