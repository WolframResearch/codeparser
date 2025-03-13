use std::fmt::Debug;

use crate::{
    abstract_::expect_children,
    node::{BinaryNode, Node, Operator, OperatorNode},
    token::{Token, TokenInput, TokenKind as TK, TokenSource},
};

pub enum Quirk {
    InfixBinaryAt,
    FlattenTimes,
}

pub(crate) fn lookup_quirk_enabled(_quirk: Quirk, default: bool) -> bool {
    // FIXME: Implement this funtionality.
    //     Lookup[$Quirks, quirk, default]
    default
}

fn quirk_enabled(_quirk: Quirk) -> bool {
    // $Quirks[quirk]
    // FIXME: Implement this funtionality.
    true
}

pub(crate) fn processInfixBinaryAtQuirk<I: TokenInput + Debug, S: TokenSource + Debug>(
    node: Node<I, S>,
    symName: &str,
) -> Node<I, S> {
    match node {
        Node::Binary(BinaryNode(OperatorNode {
            op: Operator::CodeParser_BinaryAt,
            ref children,
            src: _,
        })) if quirk_enabled(Quirk::InfixBinaryAt) => {
            let [left, middle, rhs] = expect_children(children.clone());

            if !matches!(
                left,
                Node::Token(Token {
                    tok: TK::Symbol,
                    input,
                    ..
                }) if input.as_str() == symName
            ) {
                return node;
            }

            if !matches!(middle, Node::Token(Token { tok: TK::At, .. })) {
                todo!()
            }

            // let data = rhs.source();

            /* FIXME: Port this issue handling logic.
                issues = Lookup[data, AbstractSyntaxIssues, {}];

                synthesizedSource = {symData[[Key[Source], 1]], atData[[Key[Source], 2]]};

                AppendTo[
                    issues,
                    SyntaxIssue[
                        "InfixBinaryAtQuirk", "Unexpected parse.", "Remark",
                        <| Source -> synthesizedSource, ConfidenceLevel -> 1.0 |>
                    ]
                ];

                AssociateTo[data, AbstractSyntaxIssues -> issues];

                rhs[[3]] = data;
            */

            rhs
        },
        _ => node,
    }
}
