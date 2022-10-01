use std::ops::Range;

use crate::{
    node::{Node, NodeSeq, OperatorNode, UnterminatedGroupNeedsReparseNode, UnterminatedGroupNode},
    source::{Buffer, BufferAndLength, CharacterRange},
    token::{BorrowedTokenInput, Token},
    Source, SourceConvention, SourceLocation,
};

use once_cell::sync::Lazy;
use regex::Regex;

const SPLIT_LINES_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new("\r\n|\n|\r|$").unwrap());

const CHUNK_PAT: Lazy<Regex> = Lazy::new(|| {
    /*
        Annotation Comments

        ::Package::
        etc.

        https://mathematica.stackexchange.com/questions/76192/annotation-specifier-like-author-and-sectioning-like-section-in-package-co
    */
    let annotation_pat = "^\\(\\* *:.*$";

    /*
    Common functions like:

    Begin["`Foo`"]

    */
    let directive_pat = "^(\
BeginPackage|\
Begin|\
Needs|\
End|\
EndPackage|\
Clear|\
ClearAll|\
SetOptions|\
SetAttributes|\
System`Private`NewContextPath|\
System`Private`RestoreContextPath|\
Protect|\
Unprotect|\
Package|\
PackageImport|\
PackageScope|\
PackageExport|\
Get|\
SetDelayed|\
UpSetDelayed|\
TagSetDelayed\
)\\[.*$";

    /*
    Assignments like:

    x = 1

    */
    let assignment_pat = "^[a-zA-Z$].*(:?=).*$";

    // chunkPat = RegularExpression["(?m)("<>annotationPat<>")|("<>directivePat<>")|("<>assignmentPat<>")"]
    Regex::new(&format!(
        "(?m)({annotation_pat})|({directive_pat})|({assignment_pat})"
    ))
    .unwrap()
});

pub(crate) fn reparse_unterminated<'i>(
    nodes: NodeSeq<BorrowedTokenInput<'i>>,
    input: &'i str,
    convention: SourceConvention,
    tab_width: usize,
) -> NodeSeq<BorrowedTokenInput<'i>> {
    // TODO: Combine these map_visit() calls into one? I'm unsure if that is
    //       semantically valid; are there any scenarios where the
    //       second transformation may depend on the first in some way?
    //
    //       Think this through and remove this comment, one way or the other.

    let nodes = nodes.map_visit(&mut |node| {
        match node {
            // UnterminatedGroupNeedsReparseNode => UnterminatedGroupNode
            Node::UnterminatedGroupNeedsReparse(group) => {
                let group = reparseUnterminatedGroupNode(group, input, convention, tab_width);

                Node::UnterminatedGroup(group)
            },
            other => other,
        }
    });

    nodes.map_visit(&mut |node| match node {
        Node::Token(token) if token.tok.isError() && token.tok.isUnterminated() => {
            let token = reparseUnterminatedTokenErrorNode(token, input, convention, tab_width);

            Node::Token(token)
        },
        other => other,
    })
}

//==========================================================
// Handle UnterminatedGroupNeedsReparse nodes
//==========================================================

// return: better UnterminatedGroupNode
//
// Do not return the previous children, because they are useless any way.
//
// But return the opener to make ToString stuff easier
fn reparseUnterminatedGroupNode<'i>(
    group: UnterminatedGroupNeedsReparseNode<BorrowedTokenInput<'i>>,
    str: &'i str,
    convention: SourceConvention,
    tab_width: usize,
) -> UnterminatedGroupNode<BorrowedTokenInput<'i>> {
    let UnterminatedGroupNeedsReparseNode(OperatorNode {
        op: tag,
        children,
        src,
    }) = group;

    let (_, _, better_src) = process_lines(str, tab_width, convention, src);

    // Use original src Start, but readjust src End to be the EndOfLine of the
    // last good line of the chunk
    let better_leaves = match convention {
        SourceConvention::LineColumn => {
            // Flatten out children, because there may be parsing errors from missing bracket, and
            // we do not want to propagate
            //
            //   betterLeaves = Cases[
            //       children,
            //       (LeafNode|ErrorNode)[_, _, data_ /; srcMemberFunc[data[Source]]],
            //       Infinity
            //   ];

            let mut better_leaves: Vec<Node<_>> = Vec::new();

            children.visit(&mut |node: &Node<_>| match node {
                Node::Token(Token {
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

            better_leaves
        },
        SourceConvention::CharacterIndex => {
            // Flatten out children, because there may be parsing errors from missing bracket, and
            // we do not want to propagate
            let mut better_leaves: Vec<Node<_>> = Vec::new();

            children.visit(&mut |node: &Node<_>| match node {
                Node::Token(Token {
                    tok: _,
                    input: _,
                    src: node_src,
                }) => {
                    if is_interval_member(
                        better_src.character_range().tuple(),
                        node_src.character_range().tuple(),
                    ) {
                        better_leaves.push(node.clone());
                    }
                },
                _ => (),
            });

            better_leaves
        },
    };

    // Purposely only returning leaves that are in the "better" Source
    //
    // Rationale: there is not a useful purpose for returning the rest of the
    // file, which may be massive.
    UnterminatedGroupNode(OperatorNode {
        op: tag,
        children: NodeSeq(better_leaves),
        src: better_src,
    })
}

//==========================================================
// Handle UnterminatedGroupNeedsReparse nodes
//==========================================================

// return: better ErrorNode
//
// Do not return the previous children, because they are useless any way.
fn reparseUnterminatedTokenErrorNode<'i>(
    error: Token<BorrowedTokenInput<'i>>,
    str: &'i str,
    convention: SourceConvention,
    tab_width: usize,
) -> Token<BorrowedTokenInput<'i>> {
    debug_assert!(error.tok.isError() && error.tok.isUnterminated());

    // TODO: Use `input` here to optimize the process_lines() calculation?
    let Token { tok, input: _, src } = error;

    let (first_chunk, last_good_line_index, better_src) =
        process_lines(str, tab_width, convention, src);

    // Use original src Start, but readjust src End to be the EndOfLine of the
    // last good line of the chunk
    let better_str = match convention {
        SourceConvention::LineColumn => {
            let mut components: Vec<&str> = Vec::new();

            components.push(
                &first_chunk[0].content[usize::try_from(better_src.start.second).unwrap() - 1..],
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

                let start_offset = first.as_ptr() as usize - str.as_ptr() as usize;
                let end_offset = last.as_ptr() as usize - str.as_ptr() as usize;

                &str[start_offset..end_offset]
            };

            make_better_input(str, better_str2)
        },
        SourceConvention::CharacterIndex => {
            let better_str: &str = StringTake(str, better_src.character_range());

            make_better_input(str, better_str)
        },
    };

    Token {
        tok,
        input: better_str,
        src: better_src,
    }
}

fn make_better_input<'i>(input: &str, better: &'i str) -> BorrowedTokenInput<'i> {
    let offset = better.as_ptr() as usize - input.as_ptr() as usize;

    BorrowedTokenInput {
        buf: BufferAndLength {
            buf: Buffer {
                slice: better.as_bytes(),
                offset,
            },
        },
    }
}


//==========================================================
// Helpers
//==========================================================

fn process_lines(
    input: &str,
    tab_width: usize,
    convention: SourceConvention,
    src: Source,
) -> (Vec<Line>, usize, Source) {
    let lines = to_lines_and_expand_tabs(input, tab_width);

    first_chunk_and_last_good_line(lines, tab_width, convention, src)
}

fn to_lines_and_expand_tabs(input: &str, _tab_width: usize) -> Vec<Line> {
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

fn first_chunk_and_last_good_line(
    lines: Vec<Line>,
    tab_width: usize,
    convention: SourceConvention,
    src: Source,
) -> (Vec<Line>, usize, Source) {
    //------------------------------------------------------
    // Filter `lines` into the lines that overlap with `src`
    //------------------------------------------------------

    let (lines, char_ranges_of_lines): (Vec<Line>, Option<Vec<CharacterRange>>) = match convention {
        SourceConvention::LineColumn => {
            // (*
            // lines of the node
            // *)
            // lines = lines[[src[[1, 1]];;src[[2, 1]]]];

            let start_line = src.start.first;
            let end_line = src.end.first;

            (
                retain_range(
                    lines,
                    to_zero_index(start_line)..to_zero_index(end_line) + 1,
                ),
                None,
            )
        },
        SourceConvention::CharacterIndex => {
            let specs_of_lines = lines_start_and_end_char_indexes(lines);

            let CollectMultiple(lines, specs_of_lines): CollectMultiple<Line, CharacterRange> =
                specs_of_lines
                    .flat_map(|(line, pos): (Line, CharacterRange)| {
                        // Only returns lines that intersect with the source character range.
                        if intersection(pos.tuple(), src.character_range().tuple()).is_some() {
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

    let mut chunks = lines.split_inclusive(|line: &Line| {
        let chunk_pat = &CHUNK_PAT;
        !chunk_pat.is_match(line.content)
    });

    let first_chunk: &[Line] = chunks
        .next()
        .expect("unexpected empty chunk in unterminated group");

    debug_assert!(!first_chunk.is_empty());

    let mut last_good_line_index = first_chunk.len() - 1;
    let mut last_good_line: &Line = &first_chunk[last_good_line_index];
    while last_good_line.content == "" {
        last_good_line_index -= 1;
        last_good_line = &first_chunk[last_good_line_index];
    }

    //-----------------------
    // Computer better Source
    //-----------------------

    let better_src: Source = match convention {
        SourceConvention::LineColumn => {
            // This will NOT include newline at the end
            // FIXME?
            Source {
                start: src.start,
                end: SourceLocation {
                    first: src.start.first + u32::try_from(last_good_line_index).unwrap(),
                    second: u32::try_from(last_good_line.column_width(tab_width)).unwrap() + 1,
                },
            }
        },
        SourceConvention::CharacterIndex => {
            // This WILL include newline at the end
            // FIXME?

            let CharacterRange(original_start, _) = src.character_range();

            let char_ranges_of_lines = char_ranges_of_lines.unwrap();

            // TODO(optimization): Make this a debug assert.
            // assert_eq!(char_ranges_of_lines.len(), first_chunk.len());

            let better_character_index_source_end =
                u32::try_from(char_ranges_of_lines[last_good_line_index].1).unwrap() + 1;


            Source::from_character_range(original_start, better_character_index_source_end)

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

fn StringLength(s: &str) -> usize {
    // FIXME: StringLength is NOT equal to string byte count. StringLengh counts
    //        WL characters (e.g. \[Alpha] counts as 1 character), but
    //        String::len() counts bytes. This is wrong.
    s.len()
}

fn StringTake(s: &str, range: CharacterRange) -> &str {
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
) -> impl Iterator<Item = (Line, CharacterRange)> {
    // Cumulative character index.
    let mut current_character_index: u32 = 0;

    let fold_list = lines.into_iter().map(move |line: Line| {
        let Line { content, newline } = line;

        let start_index = current_character_index;

        current_character_index = current_character_index
            .checked_add(u32::try_from(StringLength(content) + StringLength(newline)).unwrap())
            // Unlikely that this will fail, but could happen if
            // someone tries to parse a >4GB file, which is
            // conceivable for large WL notebook / data files.
            .expect("Wolfram character index value overflows u32");

        let end_index = current_character_index;

        (line, CharacterRange(start_index, end_index))
    });

    fold_list
}

fn retain_range<T>(mut vec: Vec<T>, range: Range<usize>) -> Vec<T> {
    let Range { start, end } = range;

    vec.truncate(end);

    vec.drain(0..start);

    vec
}

fn to_zero_index(value: u32) -> usize {
    debug_assert!(value > 0);

    usize::try_from(value).unwrap() - 1
}

//--------------------------------------
// Source interval comparison
//--------------------------------------

// TODO: Do more testing of this intersection function.
fn intersection(a: (u32, u32), b: (u32, u32)) -> Option<(u32, u32)> {
    use std::cmp::{max, min};

    let (a, b) = if a.0 < b.0 { (a, b) } else { (b, a) };

    let (a_start, a_end) = a;
    let (b_start, b_end) = b;

    assert!(a_start <= a_end);
    assert!(b_start <= b_end);

    debug_assert!(a_start <= b_start);

    let highest_start = max(a_start, b_start);
    let lowest_end = min(a_end, b_end);

    if highest_start > lowest_end {
        return None;
    }

    Some((highest_start, lowest_end))
}

/// Returns true if `b` is an interval that is completely contained inside `a`.
fn is_interval_member(a: (u32, u32), b: (u32, u32)) -> bool {
    let (a_start, a_end) = a;
    let (b_start, b_end) = b;

    b_start >= a_start && b_end <= a_end
}

#[test]
fn test_intersection() {
    assert_eq!(intersection((0, 0), (0, 0)), Some((0, 0)));
    assert_eq!(intersection((1, 3), (1, 3)), Some((1, 3)));
    assert_eq!(intersection((1, 4), (1, 3)), Some((1, 3)));
    assert_eq!(intersection((1, 3), (1, 4)), Some((1, 3)));
    assert_eq!(intersection((1, 2), (3, 4)), None);
    assert_eq!(intersection((3, 4), (1, 2)), None);
    assert_eq!(intersection((3, 4), (1, 3)), Some((3, 3)));
}

//--------------------------------------
// Lines
//--------------------------------------

#[derive(Debug, Copy, Clone)]
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
        //   lines = StringCases[
        //       str,
        //       Shortest[line:___ ~~ newline:("\r\n" | "\n" | "\r" | EndOfString)]
        //           :> {line, newline}
        //   ];
        let split_lines_regex: &Regex = &SPLIT_LINES_REGEX;

        split_terminator_keep(input, &split_lines_regex)
            .into_iter()
            .map(|(content, newline)| Line { content, newline })
            .collect()
    }

    fn column_width(&self, tab_width: usize) -> usize {
        let Line {
            content,
            newline: _,
        } = *self;

        line_column_width(content, tab_width)
    }
}

fn split_terminator_keep<'i>(input: &'i str, regex: &Regex) -> Vec<(&'i str, &'i str)> {
    let mut result = Vec::new();
    let mut last = 0;

    for match_ in regex.find_iter(input) {
        let index = match_.start();
        let terminator = match_.as_str();

        result.push((&input[last..index], terminator));

        last = index + terminator.len();
    }

    if last < input.len() {
        result.push((&input[last..], ""));
    }

    result
}

/// Get the rendered width of a line of tex
fn line_column_width(line: &str, tab_width: usize) -> usize {
    debug_assert!(!line.contains(&['\n', '\r']));

    let mut column = 1;

    for char in line.chars() {
        match char {
            '\t' => {
                let currentTabStop = tab_width * ((column - 1) / tab_width) + 1;

                column = currentTabStop + tab_width;
            },
            _ => column += 1,
        }
    }

    // -1 because `column` is ordinal, and width is a cardinal number.
    column - 1
}

#[test]
fn test_line_column_width() {
    assert_eq!(line_column_width("", 4), 0);
    assert_eq!(line_column_width("abc", 4), 3);
    assert_eq!(line_column_width("ab\tc", 4), 5);
    assert_eq!(line_column_width("ab\tc", 1), 4);
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
