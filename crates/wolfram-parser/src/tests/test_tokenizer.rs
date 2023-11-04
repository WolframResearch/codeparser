use crate::{
    macros::{src, token},
    source::{NextPolicyBits::RETURN_TOPLEVELNEWLINE, TOPLEVEL},
    tests::tokens,
    tokenize::Tokenizer,
    tokenize_bytes, NodeSeq, ParseOptions,
};

use pretty_assertions::assert_eq;


//
// This was asserting
//
#[test]
fn TokenizerTest_Bug1() {
    let strIn = "\\.GG";

    let session = Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.non_fatal_issues.len(), 0);
    assert_eq!(session.fatal_issues.len(), 0);
}

//
// This used to assert
//
#[test]
fn TokenizerTest_Bug2() {
    let strIn = "<<<";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok = tokenizer.peek_token();

    Tok.skip(&mut tokenizer);

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

//
// This used to assert
//
#[test]
fn TokenizerTest_Bug3() {
    let strIn = "\\\r";

    let tokenizer = Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_Bug4() {
    let strIn = "\\[";

    let session = Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.non_fatal_issues.len(), 0);
    assert_eq!(session.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_Bug5() {
    let strIn = "\"a\\\\\r\nb\"";

    let session = Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    assert_eq!(session.non_fatal_issues.len(), 0);
    assert_eq!(session.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_IntegerRealMixup() {
    let strIn = "0..";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok1 = tokenizer.peek_token();

    assert_eq!(Tok1, token!(Integer, "0", src!(1:1-1:2)));

    Tok1.skip(&mut tokenizer);

    let Tok2 = tokenizer.peek_token();

    assert_eq!(Tok2, token!(DotDot, "..", src!(1:2-1:4)));

    Tok2.skip(&mut tokenizer);

    let Tok3 = tokenizer.peek_token();

    assert_eq!(Tok3, token!(EndOfFile, "", src!(1:4-1:4)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 1);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_Basic2() {
    let strIn = "\\[Alpha]bc+1";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok1 = tokenizer.peek_token();

    assert_eq!(Tok1, token!(Symbol, "\\[Alpha]bc", src!(1:1-1:11)));

    Tok1.skip(&mut tokenizer);

    let Tok2 = tokenizer.peek_token();

    assert_eq!(Tok2, token!(Plus, "+", src!(1:11-1:12)));

    Tok2.skip(&mut tokenizer);

    let Tok3 = tokenizer.peek_token();

    assert_eq!(Tok3, token!(Integer, "1", src!(1:12-1:13)));

    Tok3.skip(&mut tokenizer);

    let Tok4 = tokenizer.peek_token();

    assert_eq!(Tok4, token!(EndOfFile, "", src!(1:13-1:13)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_OldAssert1() {
    let strIn = "8*";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(Integer, "8", src!(1:1-1:2)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_Basic3() {
    let strIn = "{\n}";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(OpenCurly, "{", src!(1:1-1:2)));

    Tok.skip(&mut tokenizer);

    //
    // Clear 0x100 because we are inside a group now
    //
    Tok = tokenizer.peek_token_with(TOPLEVEL.without(RETURN_TOPLEVELNEWLINE));

    assert_eq!(Tok, token!(InternalNewline, "\n", src!(1:2-2:1)));

    Tok.skip(&mut tokenizer);

    Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(CloseCurly, "}", src!(2:1-2:2)));

    Tok.skip(&mut tokenizer);

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_Basic4() {
    let arr = &[0xff];

    let mut tokenizer = Tokenizer::new(arr, &ParseOptions::default());

    assert_eq!(tokenizer.SrcLoc, src!(1:1).into());

    assert_eq!(tokenizer.wasEOF, false);

    let mut Tok = tokenizer.peek_token();

    assert_eq!(
        Tok,
        token!(Error_UnsafeCharacterEncoding, [0xff], src!(1:1-1:2))
    );

    assert_eq!(tokenizer.SrcLoc, src!(1:1).into());

    assert_eq!(tokenizer.wasEOF, false);

    Tok.skip(&mut tokenizer);

    Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(1:2-1:2)));

    Tok.skip(&mut tokenizer);

    assert_eq!(tokenizer.SrcLoc, src!(1:2).into());

    assert_eq!(tokenizer.wasEOF, true);

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 1);
}

#[test]
fn TokenizerTest_Crash1() {
    let arr = b"6`5..";

    let mut tokenizer = Tokenizer::new(arr, &ParseOptions::default());

    let _ = tokenizer.peek_token();

    assert_eq!(tokenizer.non_fatal_issues.len(), 1);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation1() {
    let strIn = "ab\\\ncd";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\ncd", src!(1:1-2:3)));

    let _ = tokenizer.next_token();

    Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(2:3-2:3)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation2() {
    let strIn = "ab\\\r\ncd";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\r\ncd", src!(1:1-2:3)));

    let _ = tokenizer.next_token();

    Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(2:3-2:3)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation3() {
    let strIn = "ab\\\rcd";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(Symbol, "ab\\\rcd", src!(1:1-2:3)));

    let _ = tokenizer.next_token();

    Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "", src!(2:3-2:3)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 1);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn TokenizerTest_LineContinuation4() {
    let strIn = "1\\\n";

    let mut tokenizer =
        Tokenizer::new(strIn.as_bytes(), &ParseOptions::default());

    let mut Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(Integer, "1", src!(1:1-1:2)));

    let _ = tokenizer.next_token();

    Tok = tokenizer.peek_token();

    assert_eq!(Tok, token!(EndOfFile, "\\\n", src!(1:2-2:1)));

    assert_eq!(tokenizer.non_fatal_issues.len(), 0);
    assert_eq!(tokenizer.fatal_issues.len(), 0);
}

#[test]
fn test_escaped_ascii_del() {
    let NodeSeq(tokens) =
        crate::tokenize_bytes(&[b'\\', 127], &ParseOptions::default()).unwrap();

    assert_eq!(
        tokens.as_slice(),
        [token![
            Error_UnhandledCharacter,
            [b'\\', 127],
            src!(1:1-1:3)
        ]]
    );
}

#[test]
fn test_tokenizing_escaped_chars() {
    // Test that escaped chars can be used outside of strings
    assert_eq!(
        // 0x33 is ASCII '3'
        tokens(r#"12\.33"#),
        [token![Integer, r#"12\.33"#, src!(1:1-1:7)]]
    );

    assert_eq!(
        // 0x0A is ASCII '3'
        tokens(r#"12\.0A3"#),
        [
            token![Integer, r#"12"#, src!(1:1-1:3)],
            token![ToplevelNewline, r#"\.0A"#, src!(1:3-1:7)],
            // An escaped newline doesn't increment the Location line number
            token![Integer, r#"3"#, src!(1:7-1:8)]
        ]
    );
}

#[test]
fn test_tokenize_jpeg_string() {
    assert_eq!(
        tokenize_bytes(
            include_bytes!("../../../../Tests/files/jpeg-string.txt"),
            &ParseOptions::default()
        )
        .unwrap(),
        NodeSeq(vec![
            token!(Symbol, "ÿØÿà\0\u{10}JFIF\0\u{1}\u{1}\u{1}\0H\0H\0\0ÿá\0ÔExif\0\0II", 1:1-33),
            token!(Star, "*", 1:33-34),
            token!(Symbol, "\0\u{8}\0\0\0\u{8}\0\0\u{1}", 1:34-43),
            token!(Whitespace, "\t", 1:43-45),
            token!(Symbol, "\0\u{1}\0\0\0\u{8}\0\0\0\u{1}\u{1}", 1:45-56),
            token!(Whitespace, "\t", 1:56-57),
            token!(Symbol, "\0\u{1}\0\0\0\u{11}\0\0\0\u{1a}\u{1}\u{5}\0\u{1}\0\0\0n\0\0\0\u{1b}\u{1}\u{5}\0\u{1}\0\0\0v\0\0\0", 1:57-90),
            token!(OpenParen, "(", 1:90-91),
            token!(Symbol, "\u{1}", 1:91-92),
            token!(Whitespace, "\t", 1:92-93),
            token!(Symbol, "\0\u{1}\0\0\0\u{2}\0\0\01\u{1}\u{2}\04\0\0\0", 1:93-110),
            token!(Tilde, "~", 1:110-111),
            token!(Symbol, "\0\0\02\u{1}\u{2}\0\u{1a}\0\0\0²\0\0\0", 1:111-126),
            token!(Star, "*", 1:126-127),
            token!(Symbol, "\u{88}", 1:127-128),
            token!(Whitespace, "\t", 1:128-129),
            token!(Symbol, "\0\u{1}\0\0\0úÿÿÿ\0\0\0\0H\0\0\0\u{1}\0\0\0H\0\0\0\u{1}\0\0\0Created", 1:129-165),
            token!(Whitespace, " ", 1:165-166),
            token!(Symbol, "with", 1:166-170),
            token!(Whitespace, " ", 1:170-171),
            token!(Symbol, "the", 1:171-174),
            token!(Whitespace, " ", 1:174-175),
            token!(Symbol, "Wolfram", 1:175-182),
            token!(Whitespace, " ", 1:182-183),
            token!(Symbol, "Language", 1:183-191),
            token!(Whitespace, " ", 1:191-192),
            token!(Colon, ":", 1:192-193),
            token!(Whitespace, " ", 1:193-194),
            token!(Symbol, "www", 1:194-197),
            token!(Dot, ".", 1:197-198),
            token!(Symbol, "wolfram", 1:198-205),
            token!(Dot, ".", 1:205-206),
            token!(Symbol, "com\02023", 1:206-214),
            token!(Colon, ":", 1:214-215),
            token!(Integer, "11", 1:215-217),
            token!(Colon, ":", 1:217-218),
            token!(Integer, "16", 1:218-220),
            token!(Whitespace, " ", 1:220-221),
            token!(Integer, "12", 1:221-223),
            token!(Colon, ":", 1:223-224),
            token!(Integer, "20", 1:224-226),
            token!(Colon, ":", 1:226-227),
            token!(Integer, "24", 1:227-229),
            token!(Minus, "-", 1:229-230),
            token!(Integer, "06", 1:230-232),
            token!(Colon, ":", 1:232-233),
            token!(Integer, "00", 1:233-235),
            token!(Symbol, "\0ÿÛ\0C\0\u{8}\u{6}\u{6}", 1:235-244),
            token!(Error_UnhandledCharacter, "\u{7}", 1:244-245),
            token!(Symbol, "\u{6}\u{5}\u{8}", 1:245-248),
            token!(Error_UnhandledCharacter, "\u{7}", 1:248-249),
            token!(Error_UnhandledCharacter, "\u{7}", 1:249-250),
            token!(Error_UnhandledCharacter, "\u{7}", 1:250-251),
            token!(Whitespace, "\t", 1:251-253),
            token!(Whitespace, "\t", 1:253-257),
            token!(Symbol, "\u{8}", 1:257-258),
            token!(ToplevelNewline, "\n", 1:258-2:1),
            token!(Whitespace, "\u{c}", 2:1-2),
            token!(Symbol, "\u{14}", 2:2-3),
            token!(ToplevelNewline, "\r", 2:3-3:1),
            token!(Whitespace, "\u{c}", 3:1-2),
            token!(Whitespace, "\u{b}", 3:2-3),
            token!(Whitespace, "\u{b}", 3:3-4),
            token!(Whitespace, "\u{c}", 3:4-5),
            token!(Symbol, "\u{19}\u{12}\u{13}\u{f}\u{14}\u{1d}\u{1a}\u{1f}\u{1e}\u{1d}\u{1a}\u{1c}\u{1c}", 3:5-18),
            token!(Whitespace, " ", 3:18-19),
            token!(Symbol, "$", 3:19-20),
            token!(Dot, ".", 3:20-21),
            token!(SingleQuote, "'", 3:21-22),
            token!(Whitespace, " ", 3:22-23),
            token!(String, "\",#\u{1c}\u{1c}(7),01444\u{1f}'9=82<.342ÿÛ\0C\u{1}\t\t\t\u{c}\u{b}\u{c}\u{18}\r\r\u{18}2!\u{1c}!22222222222222222222222222222222222222222222222222ÿÀ\0\u{11}\u{8}\0\u{11}\0\u{8}\u{3}\u{1}\"", 3:23-5:68),
            token!(Symbol, "\0\u{2}\u{11}\u{1}\u{3}\u{11}\u{1}ÿÄ\0\u{1f}\0\0\u{1}\u{5}\u{1}\u{1}\u{1}\u{1}\u{1}\u{1}\0\0\0\0\0\0\0\0\u{1}\u{2}\u{3}\u{4}\u{5}\u{6}", 5:68-103),
            token!(Error_UnhandledCharacter, "\u{7}", 5:103-104),
            token!(Symbol, "\u{8}", 5:104-105),
            token!(Whitespace, "\t", 5:105-109),
            token!(ToplevelNewline, "\n", 5:109-6:1),
            token!(Whitespace, "\u{b}", 6:1-2),
            token!(Symbol, "ÿÄ\0µ\u{10}\0\u{2}\u{1}\u{3}\u{3}\u{2}\u{4}\u{3}\u{5}\u{5}\u{4}\u{4}\0\0\u{1}", 6:2-22),
            token!(CloseCurly, "}", 6:22-23),
            token!(Symbol, "\u{1}\u{2}\u{3}\0\u{4}\u{11}\u{5}\u{12}", 6:23-31),
            token!(Bang, "!", 6:31-32),
            token!(Integer, "1", 6:32-33),
            token!(Symbol, "A\u{6}\u{13}Qa", 6:33-38),
            token!(Error_UnhandledCharacter, "\u{7}", 6:38-39),
            token!(String, "\"q\u{14}2\u{81}\u{91}¡\u{8}#B±Á\u{15}RÑð$3br\u{82}\t\n\u{16}\u{17}\u{18}\u{19}\u{1a}%&'()*456789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\u{83}\u{84}\u{85}\u{86}\u{87}\u{88}\u{89}\u{8a}\u{92}\u{93}\u{94}\u{95}\u{96}\u{97}\u{98}\u{99}\u{9a}¢£¤¥¦§¨©ª²³´µ¶·¸¹ºÂÃÄÅÆÇÈÉÊÒÓÔÕÖ×ØÙÚáâãäåæçèéêñòóôõö÷øùúÿÄ\0\u{1f}\u{1}\0\u{3}\u{1}\u{1}\u{1}\u{1}\u{1}\u{1}\u{1}\u{1}\u{1}\0\0\0\0\0\0\u{1}\u{2}\u{3}\u{4}\u{5}\u{6}\u{7}\u{8}\t\n\u{b}ÿÄ\0µ\u{11}\0\u{2}\u{1}\u{2}\u{4}\u{4}\u{3}\u{4}\u{7}\u{5}\u{4}\u{4}\0\u{1}\u{2}w\0\u{1}\u{2}\u{3}\u{11}\u{4}\u{5}!1\u{6}\u{12}AQ\u{7}aq\u{13}\"", 6:39-8:41),
            token!(Integer, "2", 8:41-42),
            token!(Symbol, "\u{81}\u{8}\u{14}B\u{91}¡", 8:42-48),
            token!(LongName_PlusMinus, "±", 8:48-49),
            token!(Symbol, "Á", 8:49-50),
            token!(Whitespace, "\t", 8:50-53),
            token!(Hash, "#", 8:53-54),
            token!(Integer, "3", 8:54-55),
            token!(Symbol, "Rð\u{15}brÑ", 8:55-61),
            token!(ToplevelNewline, "\n", 8:61-9:1),
            token!(Symbol, "\u{16}$4á", 9:1-5),
            token!(Percent, "%", 9:5-6),
            token!(Symbol, "ñ\u{17}\u{18}\u{19}\u{1a}", 9:6-11),
            token!(Amp, "&", 9:11-12),
            token!(SingleQuote, "'", 9:12-13),
            token!(OpenParen, "(", 9:13-14),
            token!(CloseParen, ")", 9:14-15),
            token!(Star, "*", 9:15-16),
            token!(Integer, "56789", 9:16-21),
            token!(Colon, ":", 9:21-22),
            token!(Symbol, "CDEFGHIJSTUVWXYZcdefghijstuvwxyz\u{82}\u{83}\u{84}\u{85}\u{86}\u{87}\u{88}\u{89}\u{8a}\u{92}\u{93}\u{94}\u{95}\u{96}\u{97}\u{98}\u{99}\u{9a}¢£¤¥¦§¨©ª²³´µ¶", 9:22-86),
            token!(LongName_CenterDot, "·", 9:86-87),
            token!(Symbol, "¸¹ºÂÃÄÅÆÇÈÉÊÒÓÔÕÖ", 9:87-104),
            token!(LongName_Times, "×", 9:104-105),
            token!(Symbol, "ØÙÚâãäåæçèéêòóôõö", 9:105-122),
            token!(LongName_Divide, "÷", 9:122-123),
            token!(Symbol, "øùúÿÚ\0", 9:123-129),
            token!(Whitespace, "\u{c}", 9:129-130),
            token!(Symbol, "\u{3}\u{1}\0\u{2}\u{11}\u{3}\u{11}\0", 9:130-138),
            token!(Question, "?", 9:138-139),
            token!(Symbol, "\0öhõÙ", 9:139-144),
            token!(OpenSquare, "[", 9:144-145),
            token!(Symbol, "ÅÒhRiï\u{12}\u{8b}Cu\u{1d}É\u{95}H\u{90}\u{6}U", 9:145-162),
            token!(Whitespace, " ", 9:162-163),
            token!(OpenParen, "(", 9:163-164),
            token!(Symbol, "ärÝý", 9:164-168),
            token!(Colon, ":", 9:168-169),
            token!(Symbol, "QY¯aâ3ã¨õqe¥", 9:169-181),
            token!(CloseCurly, "}", 9:181-182),
            token!(Symbol, "\u{81}", 9:182-183),
            token!(Minus, "-", 9:183-184),
            token!(Symbol, "ÚÏþ", 9:184-187),
            token!(Question, "?", 9:187-188),
            token!(Symbol, "äóLm", 9:188-192),
            token!(Error_UnterminatedString, "\"¶ý¾N7a~îì\u{7f}µE\0utQE\0\u{7f}ÿÙ", 9:192-214),
        ])
    )
}
