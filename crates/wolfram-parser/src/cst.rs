use crate::{node::Node, token::OwnedTokenInput, NodeSeq, Source};

pub type CstNode<I = OwnedTokenInput, S = Source> = Node<I, S>;

/// A sequence of concrete syntax tree nodes.
///
/// When parsing `a(**)+b`  we actually want to keep track of the comment.
/// But the comment does not affect the parsing: `a(**)` is still one "thing" to
/// the parser.
///
/// So pass around a structure that contains all of the nodes from the left,
/// including comments and whitespace.
pub type CstNodeSeq<I = OwnedTokenInput, S = Source> = NodeSeq<Node<I, S>>;
