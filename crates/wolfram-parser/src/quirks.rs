use std::{fmt::Debug, sync::Mutex};

use once_cell::sync::Lazy;

use crate::{
    abstract_::expect_children,
    node::{BinaryNode, Node, Operator, OperatorNode},
    token::{Token, TokenInput, TokenKind as TK, TokenSource},
};

// TODO(cleanup): Don't store these settings using error-prone global state.
static QUIRK_SETTINGS: Lazy<Mutex<QuirkSettings>> =
    Lazy::new(|| Mutex::new(QuirkSettings::default()));

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct QuirkSettings {
    /// "InfixBinaryAt" quirk
    ///
    ///
    /// The kernel parses `a<>StringJoin@b` as `StringJoin[a, b]`
    ///
    /// Most infix operators can be used with this syntax.
    /// Notably, SameQ and UnsameQ do NOT work with this syntax.
    ///
    /// *Related bugs: 365013*
    pub infix_binary_at: bool,

    /// "FlattenTimes" quirk
    ///
    /// In 12.1 and before:
    ///
    /// * `a / b / c` is parsed as `Times[a, Power[b, -1], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[-1, a, Power[b, -1]]`
    ///
    /// In 12.2 and after:
    ///
    /// * `a / b / c` is parsed as `Times[Times[a, Power[b, -1]], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[Times[-1, a], Power[b, -1]]`
    ///
    /// TODO: when targeting v12.2 as a minimum, remove this quirk
    ///
    /// *Related bugs: 57064, 139531, 153875, 160919*
    pub flatten_times: bool,

    /// "OldAtAtAt" quirk
    ///
    /// Changed in 13.1: `@@@`
    ///
    /// In 13.0 and before:
    ///
    /// `a @@@ b` parsed as `Apply[a, b, {1}]`
    ///
    /// In 13.1 and after:
    ///
    /// `a @@@ b` parses as `MapApply[a, b]`
    pub old_at_at_at: bool,
}

pub enum Quirk {
    /// "InfixBinaryAt" quirk
    ///
    ///
    /// The kernel parses `a<>StringJoin@b` as `StringJoin[a, b]`
    ///
    /// Most infix operators can be used with this syntax.
    /// Notably, SameQ and UnsameQ do NOT work with this syntax.
    ///
    /// *Related bugs: 365013*
    InfixBinaryAt,

    /// "FlattenTimes" quirk
    ///
    /// In 12.1 and before:
    ///
    /// * `a / b / c` is parsed as `Times[a, Power[b, -1], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[-1, a, Power[b, -1]]`
    ///
    /// In 12.2 and after:
    ///
    /// * `a / b / c` is parsed as `Times[Times[a, Power[b, -1]], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[Times[-1, a], Power[b, -1]]`
    ///
    /// TODO: when targeting v12.2 as a minimum, remove this quirk
    ///
    /// *Related bugs: 57064, 139531, 153875, 160919*
    FlattenTimes,

    /// "OldAtAtAt" quirk
    ///
    /// Changed in 13.1: `@@@`
    ///
    /// In 13.0 and before:
    ///
    /// `a @@@ b` parsed as `Apply[a, b, {1}]`
    ///
    /// In 13.1 and after:
    ///
    /// `a @@@ b` parses as `MapApply[a, b]`
    OldAtAtAt,
}

impl Default for QuirkSettings {
    fn default() -> Self {
        Self {
            infix_binary_at: true,
            flatten_times: false,
            old_at_at_at: false,
        }
    }
}

pub fn set_quirks(quirks: QuirkSettings) {
    *QUIRK_SETTINGS.lock().unwrap() = quirks;
}

pub(crate) fn is_quirk_enabled(quirk: Quirk) -> bool {
    let settings = QUIRK_SETTINGS.lock().unwrap();

    match quirk {
        Quirk::InfixBinaryAt => settings.infix_binary_at,
        Quirk::FlattenTimes => settings.flatten_times,
        Quirk::OldAtAtAt => settings.old_at_at_at,
    }
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
        })) if is_quirk_enabled(Quirk::InfixBinaryAt) => {
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
