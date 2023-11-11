use std::{num::NonZeroU32, ops::Range};

use crate::{
    agg::AggNodeSeq,
    cst::{Cst, GroupMissingCloserNode, OperatorNode},
    parse::operators::GroupOperator,
    source::{
        BufferAndLength, CharacterSpan, LineColumn, LineColumnSpan, Location,
        Span, SpanKind,
    },
    tokenize::{Token, TokenKind, TokenStr},
    utils::{non_zero_u32_add, non_zero_u32_incr},
    NodeSeq,
};

pub(crate) fn reparse_unterminated<'i>(
    mut nodes: AggNodeSeq<TokenStr<'i>>,
    input: &'i str,
    tab_width: usize,
) -> AggNodeSeq<TokenStr<'i>> {
    // TODO(cleanup): Change function parameter to take tab width as u32.
    let tab_width = u32::try_from(tab_width).unwrap();

    nodes.visit_mut(&mut |node| {
        let Cst::Token(token) = node else { return };

        if token.tok.isError() && token.tok.isUnterminated() {
            reparse_unterminated_token_error_node(token, input, tab_width);
        }
    });

    nodes
}

// TODO(test): Add test cases that cover this code path.
pub(crate) fn reparse_unterminated_tokens<'i>(
    tokens: NodeSeq<Token<TokenStr<'i>>>,
    input: &'i str,
    tab_width: usize,
) -> NodeSeq<Token<TokenStr<'i>>> {
    // TODO(cleanup): Change function parameter to take tab width as u32.
    let tab_width = u32::try_from(tab_width).unwrap();

    let NodeSeq(mut tokens) = tokens;

    for token in &mut tokens {
        if token.tok.isError() && token.tok.isUnterminated() {
            reparse_unterminated_token_error_node(token, input, tab_width);
        }
    }

    NodeSeq(tokens)
}

//==========================================================
// Handle reparse of unterminated group nodes
//==========================================================

// return: better UnterminatedGroupNode
//
// Do not return the previous children, because they are useless any way.
//
// But return the opener to make ToString stuff easier
pub(crate) fn reparse_unterminated_group_node<'i>(
    (tag, children): (GroupOperator, NodeSeq<Cst<TokenStr<'i>>>),
    str: &'i str,
    tab_width: usize,
) -> GroupMissingCloserNode<TokenStr<'i>> {
    let src = children.get_source();

    // TODO(cleanup): Change function parameter to take tab width as u32.
    let tab_width = u32::try_from(tab_width).unwrap();

    let (_, _, better_src) =
        first_chunk_and_last_good_line(str, tab_width, src);

    // Flatten out children, because there may be parsing errors from missing bracket, and
    // we do not want to propagate
    //
    //   betterLeaves = Cases[
    //       children,
    //       (LeafNode|ErrorNode)[_, _, data_ /; srcMemberFunc[data[Source]]],
    //       Infinity
    //   ];

    let mut better_leaves: Vec<Cst<_>> = Vec::new();

    // Use original src Start, but readjust src End to be the EndOfLine of the
    // last good line of the chunk
    children.visit(&mut |node: &Cst<_>| match node {
        Cst::Token(Token {
            tok: _,
            input: _,
            src: node_src,
        }) => {
            if better_src.overlaps(*node_src) {
                better_leaves.push(node.clone());
            }
        },
        _ => (),
    });

    // Purposely only returning leaves that are in the "better" Source
    //
    // Rationale: there is not a useful purpose for returning the rest of the
    // file, which may be massive.
    let node = GroupMissingCloserNode(OperatorNode {
        op: tag,
        children: NodeSeq(better_leaves),
    });

    // FIXME: Renable this assertion and fix the resulting test failure(s)
    // assert_eq!(
    //     node.get_source(),
    //     better_src,
    //     "better_src = {better_src}, better node: {node:#?}"
    // );

    node
}

//==========================================================
// Handle reparse of unterminated tokens
//==========================================================

// return: better ErrorNode
//
// Do not return the previous children, because they are useless any way.
fn reparse_unterminated_token_error_node<'i>(
    error: &mut Token<TokenStr<'i>>,
    str: &'i str,
    tab_width: u32,
) {
    debug_assert!(error.tok.isError() && error.tok.isUnterminated());

    // TODO: Use `input` here to optimize the process_lines() calculation?
    let Token {
        tok: _,
        input: _,
        src,
    } = error;

    let (first_chunk, last_good_line_index, better_src) =
        first_chunk_and_last_good_line(str, tab_width, src.clone());

    // Use original src Start, but readjust src End to be the EndOfLine of the
    // last good line of the chunk
    let better_str = match better_src.kind() {
        SpanKind::LineColumnSpan(better_src) => {
            let mut components: Vec<&str> = Vec::new();

            components.push(
                first_chunk[0]
                    .index_columns(tab_width, better_src.start.column()..),
            );

            if first_chunk.len() > 1 {
                components.extend(
                    first_chunk[1..last_good_line_index]
                        .iter()
                        .map(|line: &Line| line.content),
                );
            }

            // let better_str: String = components.join("\n");

            let better_str2: &str = {
                let first: &str = components[0];
                let last1: &str = components[components.len() - 1];
                // This will be an empty string pointing at the very end of
                // `first_chunk`.
                let last = &last1[last1.len()..];

                debug_assert!(last.is_empty());

                let start_offset =
                    first.as_ptr() as usize - str.as_ptr() as usize;
                let end_offset = last.as_ptr() as usize - str.as_ptr() as usize;

                &str[start_offset..end_offset]
            };

            make_better_input(better_str2)
        },
        SpanKind::CharacterSpan(better_src) => {
            let better_str: &str = StringTake(str, better_src);

            make_better_input(better_str)
        },
    };

    error.input = better_str;
    error.src = better_src;
}

fn make_better_input<'i>(better: &'i str) -> TokenStr<'i> {
    TokenStr {
        buf: BufferAndLength {
            buf: better.as_bytes(),
        },
    }
}


//==========================================================
// Helpers
//==========================================================

fn first_chunk_and_last_good_line(
    input: &str,
    tab_width: u32,
    src: Span,
) -> (Vec<Line>, usize, Span) {
    let lines = to_lines_and_expand_tabs(input, tab_width);

    //------------------------------------------------------
    // Filter `lines` into the lines that overlap with `src`
    //------------------------------------------------------

    let (lines, char_ranges_of_lines): (Vec<Line>, Option<Vec<CharacterSpan>>) =
        match src.kind() {
            SpanKind::LineColumnSpan(src) => {
                // (*
                // lines of the node
                // *)
                // lines = lines[[src[[1, 1]];;src[[2, 1]]]];

                let start_line = src.start.line();
                let end_line = src.end.line();

                (
                    retain_range(
                        lines,
                        to_zero_index(start_line)..to_zero_index(end_line) + 1,
                    ),
                    None,
                )
            },
            SpanKind::CharacterSpan(src) => {
                let specs_of_lines = lines_start_and_end_char_indexes(lines);

                let CollectMultiple(lines, specs_of_lines): CollectMultiple<
                    Line,
                    CharacterSpan,
                > = specs_of_lines
                    .flat_map(|(line, pos): (Line, CharacterSpan)| {
                        // Only returns lines that intersect with the source character span.
                        if pos.intersects(src) {
                            Some((line, pos))
                        } else {
                            None
                        }
                    })
                    .collect();


                (lines, Some(specs_of_lines))

                /*
                    (*
                    Include the newline at the end
                    *)
                    takeSpecsOfLines = {
                        #[[1]] + 1,
                        #[[2]]
                    }& /@ Partition[
                        FoldList[#1 + StringLength[#2[[1]]] + StringLength[#2[[2]]]&, 0, lines],
                        2,
                        1
                    ];

                    test = (IntervalIntersection[Interval[#], Interval[src]] =!= Interval[])&;

                    poss = Position[takeSpecsOfLines, _?test, {1}, Heads -> False];

                    lines = Extract[lines, poss];
                */
            },
        };

    //--------------------------
    // Find first "useful" chunk
    //--------------------------

    let chunks = split_into_chunks(&lines);

    let first_chunk: &[Line] = chunks
        .iter()
        .next()
        .expect("unexpected empty chunk in unterminated group");

    debug_assert!(!first_chunk.is_empty());

    let mut last_good_line_index = first_chunk.len() - 1;
    let mut last_good_line: &Line = &first_chunk[last_good_line_index];
    while last_good_line.content == "" {
        last_good_line_index -= 1;
        last_good_line = &first_chunk[last_good_line_index];
    }

    //--------------------
    // Compute better Span
    //--------------------

    let better_src: Span = match src.kind() {
        SpanKind::LineColumnSpan(src) => {
            // This will NOT include newline at the end
            // FIXME?
            Span::from(LineColumnSpan {
                start: src.start,
                end: LineColumn(
                    src.start
                        .line()
                        .checked_add(
                            u32::try_from(last_good_line_index).unwrap(),
                        )
                        .expect("source line overflow u32"),
                    NonZeroU32::new(last_good_line.column_width(tab_width) + 1)
                        .expect("better source column is zero"),
                ),
            })
        },
        SpanKind::CharacterSpan(src) => {
            // This WILL include newline at the end
            // FIXME?

            let CharacterSpan(original_start, _) = src;

            let char_ranges_of_lines = char_ranges_of_lines.unwrap();

            // TODO(optimization): Make this a debug assert.
            // assert_eq!(char_ranges_of_lines.len(), first_chunk.len());

            let better_character_index_source_end =
                u32::try_from(char_ranges_of_lines[last_good_line_index].1)
                    .unwrap()
                    + 1;


            Span::from_character_span(
                original_start,
                better_character_index_source_end,
            )

            // betterSrc = {
            //     src[[1]],
            //     takeSpecsOfLines[[
            //         poss[[1, 1]] + lastGoodLineIndex - 1,
            //         2
            //     ]]
            // };
        },
    };

    // TODO(optimization): Refactor to avoid this to_vec() call.
    (first_chunk.to_vec(), last_good_line_index, better_src)
}

fn split_into_chunks<'s, 'i>(lines: &'s [Line<'i>]) -> Vec<&'s [Line<'i>]> {
    crate::utils::split_by_pairs(lines, |_, right| {
        !is_new_statement_line(right.content)
    })
}

/// Returns `true` if `line` looks like it might be a new top-level statement
/// in a Wolfram Language program.
fn is_new_statement_line(line: &str) -> bool {
    debug_assert!(!line.contains(&['\r', '\n']));

    let crate::NodeSeq(tokens) = crate::tokenize(line, &Default::default());

    let tokens: Vec<_> = tokens
        .into_iter()
        .filter(|tok| {
            if tok.tok == TokenKind::Comment {
                return true;
            }

            // Remove whitespace that isn't at the very start of the line.
            if tok.tok.isTrivia() {
                match tok.src.start() {
                    Location::LineColumn(LineColumn(_, NonZeroU32::MIN))
                    | Location::CharacterIndex(0) => return true,
                    _ => return false,
                }
            }

            true
        })
        .collect();

    match tokens.as_slice() {
        // <symbol>[..
        [Token {
            tok: TokenKind::Symbol,
            input: tok_input,
            src: _,
        }, Token {
            tok: TokenKind::OpenSquare,
            ..
        }, rest @ ..] => {
            if is_top_level_directive(tok_input.buf.as_str()) {
                /*
                Common functions like:

                Begin["`Foo`"]

                */
                // BeginPackage[..
                // Needs[..
                // etc.
                true
            } else {
                // foo[.. = ..
                // foo[.. := ..
                rest.iter()
                    .find(|tok| {
                        matches!(
                            tok.tok,
                            TokenKind::Equal | TokenKind::ColonEqual
                        )
                    })
                    .is_some()
            }
        },

        /*
        Assignments like:

        x = 1

        */
        // foo = ..
        // foo :=
        [Token {
            tok: TokenKind::Symbol,
            ..
        }, Token {
            tok: TokenKind::Equal | TokenKind::ColonEqual,
            ..
        }, ..] => true,

        // foo[
        //     arg1_,
        //     arg2_,
        // ] := ..       <= this line
        [Token {
            tok: TokenKind::CloseSquare,
            ..
        }, Token {
            tok: TokenKind::Equal | TokenKind::ColonEqual,
            ..
        }, ..] => true,

        // (* .. *)
        [Token {
            tok: TokenKind::Comment,
            input: tok_input,
            ..
        }, ..] => {
            let comment = tok_input.buf.as_str().trim_start_matches("(*");
            let comment = comment.trim_start();

            if comment.starts_with(':') {
                /*
                    Annotation Comments

                    ::Package::
                    etc.

                    https://mathematica.stackexchange.com/questions/76192/annotation-specifier-like-author-and-sectioning-like-section-in-package-co
                */
                true
            } else {
                false
            }
        },

        _ => false,
    }
}

fn is_top_level_directive(symbol: &str) -> bool {
    [
        "BeginPackage",
        "Begin",
        "Needs",
        "End",
        "EndPackage",
        "Clear",
        "ClearAll",
        "SetOptions",
        "SetAttributes",
        "System`Private`NewContextPath",
        "System`Private`RestoreContextPath",
        "Protect",
        "Unprotect",
        "Package",
        "PackageImport",
        "PackageScope",
        "PackageExport",
        "Get",
        "SetDelayed",
        "UpSetDelayed",
        "TagSetDelayed",
    ]
    .contains(&symbol)
}

fn to_lines_and_expand_tabs(input: &str, _tab_width: u32) -> Vec<Line> {
    //------------------------------------------------------------
    // Split `input` into lines and expand \t characters to spaces
    //------------------------------------------------------------

    Line::split(input)

    // FIXME: Expand tabs? Needed to make the CharacterIndex Source positions
    //        work with StringTake[..] in WL.
    //
    //        Though, this is the only place (at least in compiled code I've
    //        looked carefully at) where the original input string is modified.
    //        So it is a little strange that only error tokens get \t expanded
    //        into spaces.
    //
    //        The original intent of this tab expansion may have been simply to
    //        make the counting of columns easier, which we now accomplish with
    //        Line::column_width().
    //
    // let lines: Vec<String> = lines
    //     .into_iter()
    //     // FIXME: "\n" here should be the separator we split on.
    //     .map(|line| replace_tabs(line, 1, "\n", tab_width))
    //     .collect();
}

fn StringLength(s: &str) -> usize {
    // FIXME: StringLength is NOT equal to string byte count. StringLengh counts
    //        WL characters (e.g. \[Alpha] counts as 1 character), but
    //        String::len() counts bytes. This is wrong.
    s.len()
}

fn StringTake(s: &str, range: CharacterSpan) -> &str {
    // FIXME: This treats WL characters as bytes.
    &s[range.to_rust_range()]
}

//==========================================================
// Utility Functions
//==========================================================

// TODO: Add test for this, test with different line endings.
fn lines_start_and_end_char_indexes(
    // TODO(optimization): Make this take an `impl Iterator<..>` instead of
    //                     an allocated Vec.
    lines: Vec<Line>,
) -> impl Iterator<Item = (Line, CharacterSpan)> {
    // Cumulative character index.
    let mut current_character_index: u32 = 0;

    let fold_list = lines.into_iter().map(move |line: Line| {
        let Line { content, newline } = line;

        let start_index = current_character_index;

        current_character_index = current_character_index
            .checked_add(
                u32::try_from(StringLength(content) + StringLength(newline))
                    .unwrap(),
            )
            // Unlikely that this will fail, but could happen if
            // someone tries to parse a >4GB file, which is
            // conceivable for large WL notebook / data files.
            .expect("Wolfram character index value overflows u32");

        let end_index = current_character_index;

        (line, CharacterSpan(start_index, end_index))
    });

    fold_list
}

fn retain_range<T>(mut vec: Vec<T>, range: Range<usize>) -> Vec<T> {
    let Range { start, end } = range;

    vec.truncate(end);

    vec.drain(0..start);

    vec
}

fn to_zero_index(value: NonZeroU32) -> usize {
    usize::try_from(value.get()).unwrap() - 1
}

//--------------------------------------
// Source interval comparison
//--------------------------------------

//--------------------------------------
// Lines
//--------------------------------------

#[derive(Debug, Copy, Clone, PartialEq)]
struct Line<'i> {
    /// Line characters, not including terminating `\n`, `\r\n`, or `\r`
    /// characters.
    content: &'i str,

    /// If the line has a terminator, one of: `\n`, `\r\n`, or `\r`.
    ///
    /// If this is the last line and the input ended without a terminator, this
    /// will be an empty string.
    newline: &'i str,
}

impl<'i> Line<'i> {
    /// Split `input` into a list of [`Line`]s.
    fn split(input: &str) -> Vec<Line> {
        split_lines_keep_sep(input)
            .into_iter()
            .map(|(content, newline)| Line::new(content, newline))
            .collect()
    }

    fn new(content: &'i str, newline: &'i str) -> Self {
        Line { content, newline }
    }

    fn column_width(&self, tab_width: u32) -> u32 {
        let Line {
            content,
            newline: _,
        } = *self;

        line_column_width(content, tab_width)
    }

    /// Get a substring of this line by indexing using a column range.
    fn index_columns(
        &self,
        tab_width: u32,
        range: std::ops::RangeFrom<NonZeroU32>,
    ) -> &str {
        // Get the first character whose column range includes the
        // specified range start position.
        let index = self.char_indices_and_column_ranges(tab_width).find_map(
            |(_, index, column_range)| {
                if column_range.contains(range.start.get()) {
                    Some(index)
                } else {
                    None
                }
            },
        );

        let Line {
            content,
            newline: _,
        } = self;

        let Some(index) = index else {
            panic!(
                "column index {range:?} is out of bounds for line {:?}",
                content
            )
        };

        &content[index..]
    }

    /// Return iterator of `(char, byte index, column range)`.
    ///
    /// *byte index* — offset in bytes of the unicode character occupying the
    /// specified column.
    /// *column range* — half-open range of visual columns that the character
    /// occupies. E.g. a tab character will typically occupy 2 or 4 visual
    /// columns.
    //
    // TODO: What happens when a single char other than tab occupies multiple
    //       columns? For this to be (more) correct in general, do we need to
    //       count grapheme clusters?
    fn char_indices_and_column_ranges(
        &self,
        tab_width: u32,
    ) -> impl Iterator<Item = (char, usize, ColumnRange)> + 'i {
        let mut column: NonZeroU32 = NonZeroU32::MIN;

        self.content
            .char_indices()
            .map(move |(index, char): (usize, char)| {
                let start_column = column;

                column = next_column(column, tab_width, char);

                // For the vast majority of characters, this will be a one
                // element range.
                let column_range = ColumnRange(start_column..column);

                (char, index, column_range)
            })
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ColumnRange(std::ops::Range<NonZeroU32>);

impl ColumnRange {
    #[allow(dead_code)]
    /// Column range including only the specified column.
    ///
    /// # Panics
    ///
    /// Panics if `column` is 0.
    fn at(column: u32) -> Self {
        let column = NonZeroU32::new(column).expect(
            "unable to construct ColumnRange from invalid column value 0",
        );

        ColumnRange(
            column..column.checked_add(1).expect("Column overflows u32"),
        )
    }

    fn contains(&self, column: u32) -> bool {
        let ColumnRange(range) = self;

        let Some(column) = NonZeroU32::new(column) else {
            return false;
        };

        range.contains(&column)
    }
}

/// Split `input` into lines. Unlike [`str::lines()`](str::lines), `\r` on its
/// own is considered a valid line separator.
fn split_lines_keep_sep<'i>(input: &'i str) -> Vec<(&'i str, &'i str)> {
    if input == "" {
        return vec![("", "")];
    }

    //   lines = StringCases[
    //       str,
    //       Shortest[line:___ ~~ newline:("\r\n" | "\n" | "\r" | EndOfString)]
    //           :> {line, newline}
    //   ];

    let mut lines = Vec::new();
    let mut start = 0;

    let mut char_indices = input.char_indices().peekable();

    while let Some((index, char)) = char_indices.next() {
        match char {
            '\n' => {
                let line = &input[start..index];
                let terminator = &input[index..index + 1];
                lines.push((line, terminator));

                start = index + 1;
            },
            '\r' => match char_indices.peek() {
                Some(&(next_index, '\n')) => {
                    char_indices.next();

                    let line = &input[start..index];
                    let sep = &input[index..=next_index];
                    lines.push((line, sep));

                    start = next_index + 1;
                },
                // This \r is not followd by \n, which is still a valid line
                // separator in WL.
                _ => {
                    let line = &input[start..index];
                    let sep = &input[index..index + 1];
                    lines.push((line, sep));

                    start = index + 1;
                },
            },

            _ => continue,
        }
    }

    if start < input.len() {
        lines.push((&input[start..], ""));
    }

    lines
}

/// Get the rendered width of a line of text
fn line_column_width(line: &str, tab_width: u32) -> u32 {
    debug_assert!(!line.contains(&['\n', '\r']));

    let mut column = NonZeroU32::MIN;

    for char in line.chars() {
        column = next_column(column, tab_width, char)
    }

    // -1 because `column` is ordinal, and width is a cardinal number.
    column.get() - 1
}

fn next_column(column: NonZeroU32, tab_width: u32, char: char) -> NonZeroU32 {
    match char {
        '\t' => tab_next_column(column, tab_width),
        _ => non_zero_u32_incr(column),
    }
}

fn tab_next_column(column: NonZeroU32, tab_width: u32) -> NonZeroU32 {
    assert_ne!(tab_width, 0);

    let column = column.get();

    let current_tab_stop =
        NonZeroU32::new(tab_width * ((column - 1) / tab_width) + 1).unwrap();

    non_zero_u32_add(current_tab_stop, tab_width)
}

//--------------------------------------
// Line utility tests
//--------------------------------------

#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn test_unterminated_error_chunk_reparse() {
    let input = r#"
foo[
    (* unterminated comment
    x_Integer,
    x_Real
] := Module[{},
    x + 1
]

bar = baz

baaz[
    x_
] := x / 2

Begin["`Private`"]
"#;

    let lines = split_lines_keep_sep(input);

    assert_eq!(
        lines,
        &[
            ("", "\n"),
            ("foo[", "\n"),
            ("    (* unterminated comment", "\n"),
            ("    x_Integer,", "\n"),
            ("    x_Real", "\n"),
            ("] := Module[{},", "\n"),
            ("    x + 1", "\n"),
            ("]", "\n"),
            ("", "\n"),
            ("bar = baz", "\n"),
            ("", "\n"),
            ("baaz[", "\n"),
            ("    x_", "\n"),
            ("] := x / 2", "\n"),
            ("", "\n"),
            ("Begin[\"`Private`\"]", "\n"),
        ]
    );

    let lines: Vec<Line> = lines
        .into_iter()
        .map(|(line, sep)| Line {
            content: line,
            newline: sep,
        })
        .collect();

    let chunks: Vec<&[Line]> = split_into_chunks(&lines);

    #[rustfmt::skip]
    assert_eq!(
        chunks,
        vec![
            [
                Line::new("", "\n"),
                Line::new("foo[", "\n"),
                Line::new("    (* unterminated comment", "\n"),
                Line::new("    x_Integer,", "\n"),
                Line::new("    x_Real", "\n"),
            ].as_slice(),
            [
                Line::new("] := Module[{},", "\n"),
                Line::new("    x + 1", "\n"),
                Line::new("]", "\n"),
                Line::new("", "\n"),
            ]
            .as_slice(),
            [
                Line::new("bar = baz", "\n"),
                Line::new("", "\n"),
                Line::new("baaz[", "\n"),
                Line::new("    x_", "\n"),
            ].as_slice(),
            [
                Line::new("] := x / 2", "\n"),
                Line::new("", "\n"),
            ]
            .as_slice(),
            [
                Line::new("Begin[\"`Private`\"]", "\n"),
            ].as_slice()
        ]
    );
}

#[test]
fn test_is_new_statement_line() {
    assert!(is_new_statement_line("foo = bar"));
    assert!(is_new_statement_line("foo := bar"));
    assert!(is_new_statement_line("foo=bar"));
    assert!(is_new_statement_line("foo   =   bar"));
    assert!(is_new_statement_line("foo[x] = y"));
    assert!(is_new_statement_line("(* :Name: Foo *)"));
    assert!(is_new_statement_line("(*     :Name: Foo     *)"));
    assert!(is_new_statement_line("(* ::Package:: *)"));
    assert!(is_new_statement_line("(* ::Package::Bar:: *)"));
    assert!(is_new_statement_line("(* ::Package:: hmmm *)"));
    assert!(is_new_statement_line("Needs[\"Foo`\"]"));
    // Not technically the first line in a new statement, but this kind of thing
    // commonly appears in downvalue defs with lots of arguments.
    assert!(is_new_statement_line("] := "));

    assert!(!is_new_statement_line("Set[foo, bar]"));
    assert!(is_new_statement_line("SetDelayed[foo, bar]"));
    assert!(is_new_statement_line("SetDelayed["));

    assert!(!is_new_statement_line("    5,"));
    assert!(!is_new_statement_line("foo"));
    assert!(!is_new_statement_line(" foo = bar"));
    assert!(!is_new_statement_line(""));
    assert!(!is_new_statement_line("(* Normal comment *)"));
    // assert!(!is_new_statement_line("\n"));
    // assert!(!is_new_statement_line("\n\n"));
    assert!(!is_new_statement_line("foo[\"Foo`\"]"));
}

#[test]
fn test_split_lines_keep_sep() {
    assert_eq!(split_lines_keep_sep(""), vec![("", "")]);
    assert_eq!(split_lines_keep_sep("\r"), vec![("", "\r")]);
    assert_eq!(split_lines_keep_sep("\n"), vec![("", "\n")]);
    assert_eq!(split_lines_keep_sep("\r\n"), vec![("", "\r\n")]);
    assert_eq!(split_lines_keep_sep("\n\r"), vec![("", "\n"), ("", "\r")]);

    assert_eq!(
        split_lines_keep_sep("a\n\nc\n"),
        vec![("a", "\n"), ("", "\n"), ("c", "\n")]
    );
    assert_eq!(
        split_lines_keep_sep("a\r\rc\r"),
        vec![("a", "\r"), ("", "\r"), ("c", "\r")]
    );

    assert_eq!(
        split_lines_keep_sep("one\ntwo"),
        vec![("one", "\n"), ("two", "")]
    );
    assert_eq!(
        split_lines_keep_sep("one\ntwo\n"),
        vec![("one", "\n"), ("two", "\n")]
    );
    assert_eq!(
        split_lines_keep_sep("one\r\ntwo\n"),
        vec![("one", "\r\n"), ("two", "\n")]
    );
    assert_eq!(
        split_lines_keep_sep("one\rtwo\r"),
        vec![("one", "\r"), ("two", "\r")]
    );
    assert_eq!(
        split_lines_keep_sep("one\n\rthree\n"),
        vec![("one", "\n"), ("", "\r"), ("three", "\n")]
    );
    assert_eq!(
        split_lines_keep_sep("one\n\rthree\r\n"),
        vec![("one", "\n"), ("", "\r"), ("three", "\r\n")]
    );
}

#[test]
fn test_line_column_width() {
    assert_eq!(line_column_width("", 4), 0);
    assert_eq!(line_column_width("abc", 4), 3);

    assert_eq!(line_column_width("\t", 4), 4);
    assert_eq!(line_column_width("abc\t", 4), 4);
    assert_eq!(line_column_width("abc\t", 8), 8);
    assert_eq!(line_column_width("ab\tc", 4), 5);
    assert_eq!(line_column_width("ab\tc", 1), 4);
}

#[test]
fn test_tab_next_column() {
    assert_eq!(tab_next_column(nz(1), 4), nz(5));
    assert_eq!(tab_next_column(nz(2), 4), nz(5));
    assert_eq!(tab_next_column(nz(3), 4), nz(5));
    assert_eq!(tab_next_column(nz(4), 4), nz(5));
    assert_eq!(tab_next_column(nz(5), 4), nz(9));

    assert_eq!(tab_next_column(nz(1), 1), nz(2));
    assert_eq!(tab_next_column(nz(2), 1), nz(3));
    assert_eq!(tab_next_column(nz(3), 1), nz(4));
}

#[test]
fn test_char_indices_and_columns() {
    assert_eq!(
        Line::new("f[x]", "")
            .char_indices_and_column_ranges(4)
            .collect::<Vec<_>>(),
        vec![
            ('f', 0, ColumnRange::at(1)),
            ('[', 1, ColumnRange::at(2)),
            ('x', 2, ColumnRange::at(3)),
            (']', 3, ColumnRange::at(4)),
        ]
    );

    assert_eq!(
        Line::new("\tf[x]", "")
            .char_indices_and_column_ranges(4)
            .collect::<Vec<_>>(),
        vec![
            ('\t', 0, ColumnRange(nz(1)..nz(5))),
            ('f', 1, ColumnRange::at(5)),
            ('[', 2, ColumnRange::at(6)),
            ('x', 3, ColumnRange::at(7)),
            (']', 4, ColumnRange::at(8))
        ]
    );

    // ï is two bytes (0xC3 0xAF) in UTF-8, but only one character/column.
    assert_eq!(
        Line::new("f[ï]", "")
            .char_indices_and_column_ranges(4)
            .collect::<Vec<_>>(),
        vec![
            ('f', 0, ColumnRange::at(1)),
            ('[', 1, ColumnRange::at(2)),
            ('ï', 2, ColumnRange::at(3)),
            (']', 4, ColumnRange::at(4)),
        ]
    );
}

#[test]
fn test_index_line_by_column() {
    assert_eq!(Line::new("\tf[x]", "").index_columns(4, nz(1)..), "\tf[x]");
    assert_eq!(Line::new("\tf[x]", "").index_columns(4, nz(2)..), "\tf[x]");
    assert_eq!(Line::new("\tf[x]", "").index_columns(4, nz(3)..), "\tf[x]");
    assert_eq!(Line::new("\tf[x]", "").index_columns(4, nz(4)..), "\tf[x]");
    assert_eq!(Line::new("\tf[x]", "").index_columns(4, nz(5)..), "f[x]");
    assert_eq!(Line::new("\tf[x]", "").index_columns(4, nz(6)..), "[x]");
}

#[cfg(test)]
fn nz(value: u32) -> NonZeroU32 {
    NonZeroU32::new(value).unwrap()
}


struct CollectMultiple<A, B>(Vec<A>, Vec<B>);

impl<A, B> FromIterator<(A, B)> for CollectMultiple<A, B> {
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        let mut a = Vec::new();
        let mut b = Vec::new();

        for (a_elem, b_elem) in iter {
            a.push(a_elem);
            b.push(b_elem);
        }

        CollectMultiple(a, b)
    }
}
