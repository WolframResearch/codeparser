use crate::{
    issue::{CodeAction, Issue, IssueTag, Severity},
    macros::src,
    parse_cst,
    source::{Source, Span},
    symbols as sym, ParseOptions, SourceConvention, StringifyMode,
};

use pretty_assertions::assert_eq;


//
// this used to assert
//
#[test]
fn APITest_Bug1() {
    let result = crate::parse_to_token(
        b"abc[]",
        &ParseOptions::default(),
        StringifyMode::Normal,
    );

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to hang
//
#[test]
fn APITest_Hang1() {
    let strIn = "<<rr[R";

    let result = parse_cst(strIn, &ParseOptions::default());

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to crash
//
#[test]
fn APITest_Crash1() {
    let strIn = "0^^";

    let result = parse_cst(strIn, &ParseOptions::default());

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to crash
//
#[test]
fn APITest_Crash2() {
    let strIn = ".2^^0";

    let result = parse_cst(strIn, &ParseOptions::default());

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to crash
//
#[test]
fn APITest_Crash3() {
    let strIn = "12^^a.a";

    let result = parse_cst(strIn, &ParseOptions::default());

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to crash
//
#[test]
fn APITest_Crash4() {
    let strIn = "12..";

    let result = parse_cst(strIn, &ParseOptions::default());

    assert_eq!(
        result.non_fatal_issues,
        vec![Issue {
            make_sym: sym::CodeParser_FormatIssue,
            tag: IssueTag::Ambiguous,
            msg: "Ambiguous syntax.".to_owned(),
            sev: Severity::Formatting,
            src: Source::Span(Span::from(src!(1:3-3))),
            val: 1.0,
            actions: vec![CodeAction::insert_text(
                "Insert space".into(),
                Span::from(src!(1:3-3)),
                " ".into(),
            )],
            additional_descriptions: vec![],
            additional_sources: vec![],
        }]
    );
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to crash
//
#[test]
fn APITest_Crash5() {
    let strIn = "123\\\n.45";

    let result = parse_cst(strIn, &ParseOptions::default());

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to crash
//
#[test]
fn APITest_Crash6() {
    let strIn = "\\0560";

    let result = parse_cst(strIn, &ParseOptions::default());

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}

//
// this used to crash
//
// NOTE: This test was part of the C++ version, but is not possible in the
//       Rust version of CodeParser, which does not allow invalid values
//       for the SourceConvention.
#[test]
fn APITest_Crash7() {
    //let strIn = "1+1";

    //
    // this was originally using SOURCECONVENTION_UNKNOWN, which was 0
    // but 0 is now SourceConvention::LineColumn
    // so make up a bogus SourceConvention of 2
    //

    //let mut session = ParserSession::new(strIn.as_bytes(),static_cast<SourceConvention>(2), DEFAULT_TAB_WIDTH, FirstLineBehavior::NotScript, EncodingMode::Normal);
    //assert_eq!(res, PARSERSESSIONINIT_ERROR);
}

//
// this used to crash
//
// CODETOOLS-62
//
#[test]
fn APITest_Crash8() {
    let bufAndLen = "(*\r\n*)";

    let result = parse_cst(
        bufAndLen,
        &ParseOptions::default()
            .source_convention(SourceConvention::CharacterIndex),
    );

    assert_eq!(result.non_fatal_issues, Vec::new());
    assert_eq!(result.fatal_issues, Vec::new());
}
