//! # Parser Design
//!
//! Each parse of a Wolfram input is managed by a [`ParserSession`] instance.
//!
//! Parsing logic is structured into individual "modules" calls *parselets*.
//!
//! There are two kinds of parselet:
//!
//! * [`PrefixParselet`] — invoked when there is no previous expression in the
//!   current context.
//! * [`InfixParselet`] — invoked when there is a previous expression in the
//!   current context.
//!
//! Every token is associated with one [`PrefixParselet`] instance
//! ([`ParseBuilder::with_prefix_parselet()`]) and one
//! [`InfixParselet`] instance ([`ParseBuilder::with_infix_parselet()`]), which are
//! invoked, respectively, when that token is encountered in "prefix" or "infix"
//! position.
//!
//! Parselet implementations will typically view the current or next token,
//! do a bit of logic checking for possible ways forward, and then continue the
//! parsing process by doing one of the following:
//!
//! * For simple parselets, like [`LeafParselet`], construct a parsed node from
//!   a single [*operand token*][term] and return it.
//!
//! * Call [`ParserSession::parse_prefix()`] on subsequent token(s) in the input
//!   to parse parselet-defined argument subexpression(s), followed by
//!   calling a [`reduce_*()` method][self#reduce-methods] to produce a new
//!   parsed node.
//!
//! * Call [`ParserSession::parse_infix()`] on a subsequent token
//!    in the input, passing in the immediately previously completed parsed
//!    sub-expression.
//!
//! In the majority of cases, parselet implementations should finish by calling
//! [`parse_climb()`][ParserSession::parse_climb] and passing in the completed
//! parsed node value.
//!
//! [term]: crate::parse#general-terminology
//!
//! # Parse Contexts
//!
//! The term "context" is used to refer to the state kept by the parser to
//! guide the parsing of a subexpression within the input.
//!
//! Context state is stored as [`Context`] value created by calls to
//! [`ParserSession::push_context()`]. A new parser context is typically created
//! when the parser begins processing a higher-precedence subexpression.
//!
//! The text diagram below roughly indicates the region of source code covered
//! by several contexts:
//!
//! ```text
//! a + b * foo[x / y]  |
//!             ^^!^^   | BinaryOperatorParselet, Precedence::SLASH,   reduce_binary()
//!         ^^^!^^^^^^  | CallParselet,           Precedence::HIGHEST, reduce_call()
//!     ^^!^^^^^^^^^^^  | InfixOperatorParselet,  Prececence::STAR,    reduce_infix()
//! ^^!^^^^^^^^^^^^^^^  | InfixOperatorParselet,  Precedence::PLUS,    reduce_infix()
//! ```
//!
//! From this diagram, a few corrolary statements about contexts follow:
//!
//! * Roughly speaking, one context exists for each logical subexpression in the
//!   input.
//!
//! * A parser context must always contain at least one node (its initial node).
//!
//!   *Note:* [`ParserSession::push_context()`] must only be called by a
//!   parselet implementation after a node has been pushed.
//!
//! * A parser context has an associated precedence value, typically the
//!   precedence of the operator that caused a new parsing context to begin.
//!
//! * At any given time during parsing, the current parsing contexts form a
//!   stack, with the latest (further along in the input) context at the top.
//!
//! Typically, though not always, parser contexts are created automatically
//! when [`ParserSession::parse_climb()`] detects that a subsequent token
//! in the input has a higher precedence than the current top context, and
//! begin a new context using [`push_context()`][ParserSession::push_context] to
//! contain the parsing of the higher-precedence subexpression.
//!
//! Parser contexts provide a bit of ambient information to guide the parser,
//! but they are not responsible for creating, storing or manipulating parsed
//! expressions.
//!
//!
//!



// Import items referenced in the module doc comment
#[allow(unused_imports)]
use crate::{
    cst::Cst,
    parse::{
        parselet::{InfixParselet, LeafParselet, PrefixParselet},
        Context, ParseBuilder, ParserSession,
    },
    read::Reader,
    tokenize::TokenKind,
};
