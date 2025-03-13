//! Input form concrete syntax trees.
//!
//! [`Cst`] — root and element type in a concrete syntax tree.

use wolfram_expr::Expr;

use crate::{
    source::{Source, Span},
    tokenize::{OwnedTokenInput, Token, TokenInput, TokenKind, TokenSource},
    NodeSeq,
};

/// A sequence of concrete syntax tree nodes.
///
/// When parsing `a(**)+b`  we actually want to keep track of the comment.
/// But the comment does not affect the parsing: `a(**)` is still one "thing" to
/// the parser.
///
/// So pass around a structure that contains all of the nodes from the left,
/// including comments and whitespace.
// TODO(cleanup): CstSeq?
pub type CstNodeSeq<I = OwnedTokenInput, S = Span> = NodeSeq<Cst<I, S>>;

/// A concrete syntax tree (CST) node.
///
/// If this was parsed from well-formed input (i.e. has no internal syntax error
/// nodes), a single [`Cst`] represents a Wolfram Language expression.
///
/// A typical [`Cst`] is made up of further child syntax trees. A [`Cst`] tree
/// terminates at "leaf" variants such as [`Cst::Token(..)`].
#[derive(Debug, Clone, PartialEq)]
pub enum Cst<I = OwnedTokenInput, S = Span> {
    Token(Token<I, S>),
    Call(CallNode<I, S>),
    SyntaxError(SyntaxErrorNode<I, S>),
    Prefix(PrefixNode<I, S>),
    Infix(InfixNode<I, S>),
    Postfix(PostfixNode<I, S>),
    Binary(BinaryNode<I, S>),
    Ternary(TernaryNode<I, S>),
    PrefixBinary(PrefixBinaryNode<I, S>),
    Compound(CompoundNode<I, S>),
    Group(GroupNode<I, S>),
    GroupMissingCloser(GroupMissingCloserNode<I, S>),
    GroupMissingOpener(GroupMissingOpenerNode<I, S>),
    // TODO(cleanup): This variant is never constructed during concrete parsing.
    Box(BoxNode<I, S>),
    // TODO(cleanup): This variant is never constructed during concrete parsing.
    Code(CodeNode<S>),
}


#[derive(Debug, Clone, PartialEq)]
pub struct CodeNode<S = Span> {
    pub first: Expr,
    pub second: Expr,
    pub src: S,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoxNode<I = OwnedTokenInput, S = Span> {
    pub kind: BoxKind,
    pub children: CstNodeSeq<I, S>,
    pub src: S,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BoxKind {
    RowBox,
    GridBox,
    TagBox,
    TemplateBox,
    InterpretationBox,
    SubscriptBox,
    SuperscriptBox,
    StyleBox,
    NamespaceBox,
    OverscriptBox,
    SubsuperscriptBox,
}

/// Any kind of prefix, postfix, binary, or infix operator
#[derive(Debug, Clone, PartialEq)]
pub struct OperatorNode<I = OwnedTokenInput, S = Span, O = InfixOperator> {
    pub op: O,
    pub children: CstNodeSeq<I, S>,
    pub src: S,
}

/// `-a`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixNode<I = OwnedTokenInput, S = Span>(
    pub OperatorNode<I, S, PrefixOperator>,
);

/// `a @ b`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryNode<I = OwnedTokenInput, S = Span>(
    pub OperatorNode<I, S, BinaryOperator>,
);

/// `a + b + c`
#[derive(Debug, Clone, PartialEq)]
pub struct InfixNode<I = OwnedTokenInput, S = Span>(pub OperatorNode<I, S>);

/// `a /: b = c`
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryNode<I = OwnedTokenInput, S = Span>(
    pub OperatorNode<I, S, TernaryOperator>,
);

/// `a!`
#[derive(Debug, Clone, PartialEq)]
pub struct PostfixNode<I = OwnedTokenInput, S = Span>(
    pub OperatorNode<I, S, PostfixOperator>,
);

/// `\[Integral] f \[DifferentialD] x`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixBinaryNode<I = OwnedTokenInput, S = Span>(
    pub OperatorNode<I, S, PrefixBinaryOperator>,
);

/// `f[x]`
#[derive(Debug, Clone, PartialEq)]
pub struct CallNode<I = OwnedTokenInput, S = Span> {
    pub head: CallHead<I, S>,
    pub body: CallBody<I, S>,
    pub src: S,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallHead<I, S> {
    /// Concrete Call nodes can have more than one element in `head`, and
    /// serialize as `CallNode[{__}, ..]`
    ///
    /// Happens for e.g. `f [ x ]`, where the whitespace after `f` is a token
    /// associated with the head.
    Concrete(CstNodeSeq<I, S>),

    /// Aggregate and abstract Call nodes must have exactly one element in `head`,
    /// and serialize as `CallNode[node_, ..]`.
    Aggregate(Box<Cst<I, S>>),
}

/// Subset of [`Cst`] variants that are allowed as the body of a [`CallNode`].
#[derive(Debug, Clone, PartialEq)]
pub enum CallBody<I = OwnedTokenInput, S = Span> {
    Group(GroupNode<I, S, CallOperator>),
    GroupMissingCloser(GroupMissingCloserNode<I, S, CallOperator>),
}

/// `{x}`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupNode<I = OwnedTokenInput, S = Span, O = GroupOperator>(
    pub OperatorNode<I, S, O>,
);

/// Any "compound" of tokens:
///
/// * `a_`
/// * `_b`
/// * `a_.`
/// * `#a`
/// * `#abc`
/// * `##2`
/// * `%2`
#[derive(Debug, Clone, PartialEq)]
pub struct CompoundNode<I = OwnedTokenInput, S = Span>(
    pub OperatorNode<I, S, CompoundOperator>,
);

/// A syntax error that contains structure.
#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxErrorNode<I = OwnedTokenInput, S = Span> {
    pub err: SyntaxErrorKind,
    pub children: CstNodeSeq<I, S>,
    pub src: S,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SyntaxErrorKind {
    ExpectedSymbol,
    ExpectedSet,
    ExpectedTilde,
}

/// `{]`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingCloserNode<
    I = OwnedTokenInput,
    S = Span,
    O = GroupOperator,
>(pub OperatorNode<I, S, O>);

/// Only possible with boxes
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingOpenerNode<I = OwnedTokenInput, S = Span>(
    pub OperatorNode<I, S, GroupOperator>,
);

/// `{`
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnterminatedGroupNeedsReparseNode<
    I = OwnedTokenInput,
    S = Span,
>(pub OperatorNode<I, S, GroupOperator>);

/// Node representation of a token.
///
/// [`LeafNode`] instances are *not* constructed during concrete parsing.
///
/// They are only produced when abstracting a concrete parse tree into an
/// abstract syntax tree.
pub struct LeafNode {
    pub kind: TokenKind,
    pub input: String,
    pub src: Source,
}

//======================================
// Node convertions
//======================================

macro_rules! from_node {
    ($name:ident => Cst::$variant:ident) => {
        impl<I> From<$name> for Cst<I> {
            fn from(node: $name) -> Cst<I> {
                Cst::$variant(node)
            }
        }
    };

    ($name:ident<I> => Cst::$variant:ident) => {
        impl<I> From<$name<I>> for Cst<I> {
            fn from(node: $name<I>) -> Cst<I> {
                Cst::$variant(node)
            }
        }
    };

    ($name:ident<I, S> => Cst::$variant:ident) => {
        impl<I, S> From<$name<I, S>> for Cst<I, S> {
            fn from(node: $name<I, S>) -> Cst<I, S> {
                Cst::$variant(node)
            }
        }
    };
}

from_node!(CallNode<I, S> => Cst::Call);
from_node!(SyntaxErrorNode<I, S> => Cst::SyntaxError);
from_node!(BinaryNode<I, S> => Cst::Binary);
from_node!(TernaryNode<I, S> => Cst::Ternary);
from_node!(InfixNode<I, S> => Cst::Infix);
from_node!(PrefixNode<I, S> => Cst::Prefix);
from_node!(PostfixNode<I, S> => Cst::Postfix);
from_node!(PrefixBinaryNode<I, S> => Cst::PrefixBinary);
from_node!(CompoundNode<I, S> => Cst::Compound);
from_node!(GroupNode<I, S> => Cst::Group);
from_node!(GroupMissingCloserNode<I, S> => Cst::GroupMissingCloser);
from_node!(GroupMissingOpenerNode<I, S> => Cst::GroupMissingOpener);
from_node!(BoxNode<I, S> => Cst::Box);

impl<I, S> From<CodeNode<S>> for Cst<I, S> {
    fn from(code: CodeNode<S>) -> Self {
        Cst::Code(code)
    }
}

//==========================================================
// Impls
//==========================================================

//======================================
// NodeSeq
//======================================

impl<I, S> CstNodeSeq<I, S> {
    pub fn visit(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        let NodeSeq(elements) = self;

        for elem in elements {
            elem.visit(visit);
        }
    }

    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        let NodeSeq(elements) = self;

        let elements = elements
            .into_iter()
            .map(|elem| elem.map_visit(visit))
            .collect();

        NodeSeq(elements)
    }
}

impl<N> NodeSeq<N> {
    pub fn push<I: Into<N>>(&mut self, node: I) {
        let NodeSeq(vec) = self;

        let node = node.into();
        vec.push(node);
    }
}

impl<N> NodeSeq<N> {
    pub(crate) fn new() -> NodeSeq<N> {
        NodeSeq(Vec::new())
    }

    pub fn clear(&mut self) {
        let NodeSeq(vec) = self;

        vec.clear();
    }

    pub fn is_empty(&self) -> bool {
        let NodeSeq(vec) = self;
        return vec.is_empty();
    }

    pub fn len(&self) -> usize {
        let NodeSeq(vec) = self;
        return vec.len();
    }

    // PRE_COMMIT: impl Index?
    // const NodeVariant& NodeSeq::operator[](size_t index) const {
    //     return vec[index];
    // }

    fn first(&self) -> &N {
        let NodeSeq(vec) = self;
        vec.first().expect("NodeSeq::first(): vector is empty")
    }

    fn last(&self) -> &N {
        let NodeSeq(vec) = self;
        vec.last().expect("NodeSeq::last(): vector is empty")
    }
}

impl<I, S> CstNodeSeq<I, S> {
    pub(crate) fn check(&self) -> bool {
        let NodeSeq(vec) = self;

        for elem in vec {
            if !elem.check() {
                return false;
            }
        }

        return true;
    }
}

impl<I: TokenInput, S> CstNodeSeq<I, S> {
    pub(crate) fn into_owned_input(self) -> CstNodeSeq<OwnedTokenInput, S> {
        let NodeSeq(nodes) = self;

        let nodes = nodes.into_iter().map(Cst::into_owned_input).collect();

        NodeSeq(nodes)
    }
}

//==========================================================
// Nodes
//==========================================================

impl<I, S> Cst<I, S> {
    /// Visit this node and every child node, recursively.
    pub fn visit(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        // Visit the current node.
        visit(self);

        // Visit child nodes.
        match self {
            Cst::Token(_) => (),
            Cst::Call(CallNode { head, body, src: _ }) => {
                head.visit(visit);
                body.as_op().visit_children(visit);
            },
            Cst::SyntaxError(SyntaxErrorNode {
                err: _,
                children,
                src: _,
            }) => {
                children.visit(visit);
            },
            Cst::Prefix(PrefixNode(op)) => op.visit_children(visit),
            Cst::Infix(InfixNode(op)) => op.visit_children(visit),
            Cst::Postfix(PostfixNode(op)) => op.visit_children(visit),
            Cst::Binary(BinaryNode(op)) => op.visit_children(visit),
            Cst::Ternary(TernaryNode(op)) => op.visit_children(visit),
            Cst::PrefixBinary(PrefixBinaryNode(op)) => op.visit_children(visit),
            Cst::Compound(CompoundNode(op)) => op.visit_children(visit),
            Cst::Group(GroupNode(op))
            | Cst::GroupMissingCloser(GroupMissingCloserNode(op))
            | Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                op.visit_children(visit)
            },
            Cst::Box(BoxNode {
                kind: _,
                children,
                src: _,
            }) => {
                children.visit(visit);
            },
            // These node types have no child nodes.
            Cst::Code(_) => (),
        }
    }

    /// Transform this node tree by visiting this node and every child node, recursively.
    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        // Visit the current node.
        let self_ = visit(self);

        // Visit child nodes.
        let node: Cst<I, S> = match self_ {
            Cst::Token(_) => return self_,
            Cst::Call(CallNode { head, body, src }) => {
                let head = head.map_visit(visit);

                let body = body.map_op(|body_op: OperatorNode<_, _, _>| {
                    body_op.map_visit(visit)
                });

                Cst::Call(CallNode { head, body, src })
            },
            Cst::SyntaxError(SyntaxErrorNode { err, children, src }) => {
                let children = children.map_visit(visit);

                Cst::SyntaxError(SyntaxErrorNode { err, children, src })
            },

            Cst::Infix(InfixNode(op)) => {
                Cst::Infix(InfixNode(op.map_visit(visit)))
            },
            Cst::Prefix(PrefixNode(op)) => {
                Cst::Prefix(PrefixNode(op.map_visit(visit)))
            },
            Cst::Postfix(PostfixNode(op)) => {
                Cst::Postfix(PostfixNode(op.map_visit(visit)))
            },
            Cst::Binary(BinaryNode(op)) => {
                Cst::Binary(BinaryNode(op.map_visit(visit)))
            },
            Cst::Ternary(TernaryNode(op)) => {
                Cst::Ternary(TernaryNode(op.map_visit(visit)))
            },
            Cst::PrefixBinary(PrefixBinaryNode(op)) => {
                Cst::PrefixBinary(PrefixBinaryNode(op.map_visit(visit)))
            },
            Cst::Compound(CompoundNode(op)) => {
                Cst::Compound(CompoundNode(op.map_visit(visit)))
            },
            Cst::Group(GroupNode(op)) => {
                Cst::Group(GroupNode(op.map_visit(visit)))
            },
            Cst::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Cst::GroupMissingCloser(GroupMissingCloserNode(
                    op.map_visit(visit),
                ))
            },
            Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                Cst::GroupMissingOpener(GroupMissingOpenerNode(
                    op.map_visit(visit),
                ))
            },

            Cst::Box(BoxNode {
                kind,
                children,
                src,
            }) => {
                let children = children.map_visit(visit);

                Cst::Box(BoxNode {
                    kind,
                    children,
                    src,
                })
            },

            // These node types have no child nodes.
            node @ Cst::Code(_) => node,
        };

        node
    }
}

impl<I: TokenInput, S> Cst<I, S> {
    pub fn into_owned_input(self) -> Cst<OwnedTokenInput, S> {
        match self {
            Cst::Token(token) => Cst::Token(token.into_owned_input()),
            Cst::Call(CallNode { head, body, src }) => Cst::Call(CallNode {
                head: match head {
                    CallHead::Concrete(head) => {
                        CallHead::Concrete(head.into_owned_input())
                    },
                    CallHead::Aggregate(head) => CallHead::Aggregate(Box::new(
                        (*head).into_owned_input(),
                    )),
                },
                body: body.map_op(|body_op| body_op.into_owned_input()),
                src,
            }),
            Cst::SyntaxError(SyntaxErrorNode { err, children, src }) => {
                Cst::SyntaxError(SyntaxErrorNode {
                    err,
                    children: children.into_owned_input(),
                    src,
                })
            },
            Cst::Prefix(PrefixNode(op)) => {
                Cst::Prefix(PrefixNode(op.into_owned_input()))
            },
            Cst::Infix(InfixNode(op)) => {
                Cst::Infix(InfixNode(op.into_owned_input()))
            },
            Cst::Postfix(PostfixNode(op)) => {
                Cst::Postfix(PostfixNode(op.into_owned_input()))
            },
            Cst::Binary(BinaryNode(op)) => {
                Cst::Binary(BinaryNode(op.into_owned_input()))
            },
            Cst::Ternary(TernaryNode(op)) => {
                Cst::Ternary(TernaryNode(op.into_owned_input()))
            },
            Cst::PrefixBinary(PrefixBinaryNode(op)) => {
                Cst::PrefixBinary(PrefixBinaryNode(op.into_owned_input()))
            },
            Cst::Compound(CompoundNode(op)) => {
                Cst::Compound(CompoundNode(op.into_owned_input()))
            },
            Cst::Group(GroupNode(op)) => {
                Cst::Group(GroupNode(op.into_owned_input()))
            },
            Cst::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Cst::GroupMissingCloser(GroupMissingCloserNode(
                    op.into_owned_input(),
                ))
            },
            Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                Cst::GroupMissingOpener(GroupMissingOpenerNode(
                    op.into_owned_input(),
                ))
            },
            Cst::Box(BoxNode {
                kind,
                children,
                src,
            }) => Cst::Box(BoxNode {
                kind,
                children: children.into_owned_input(),
                src,
            }),
            Cst::Code(node) => Cst::Code(node),
        }
    }
}


impl<I, S: TokenSource> Cst<I, S> {
    // TODO(cleanup): Combine with getSource()
    pub(crate) fn source(&self) -> S {
        self.getSource()
    }

    pub(crate) fn getSource(&self) -> S {
        match self {
            Cst::Token(token) => token.src.clone(),
            Cst::Call(node) => node.getSource(),
            Cst::SyntaxError(node) => node.getSource(),
            Cst::Prefix(PrefixNode(op)) => op.getSource(),
            Cst::Infix(InfixNode(op)) => op.getSource(),
            Cst::Postfix(PostfixNode(op)) => op.getSource(),
            Cst::Binary(BinaryNode(op)) => op.getSource(),
            Cst::PrefixBinary(PrefixBinaryNode(op)) => op.getSource(),
            Cst::Ternary(TernaryNode(op)) => op.getSource(),
            Cst::Compound(CompoundNode(op)) => op.getSource(),
            Cst::Group(GroupNode(op)) => op.getSource(),
            Cst::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                op.getSource()
            },
            Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                op.getSource()
            },
            Cst::Box(BoxNode { src, .. }) => src.clone(),
            Cst::Code(node) => node.src.clone(),
        }
    }
}

impl<I, S> Cst<I, S> {
    // TODO(cleanup): Are these check() methods used anywhere? What do they even do?
    #[allow(dead_code)]
    fn check(&self) -> bool {
        match self {
            Cst::Token(token) => token.check(),
            Cst::Call(node) => node.check(),
            Cst::Prefix(PrefixNode(op)) => op.check(),
            Cst::Binary(BinaryNode(op)) => op.check(),
            Cst::Infix(InfixNode(op)) => op.check(),
            Cst::Ternary(TernaryNode(op)) => op.check(),
            Cst::Postfix(PostfixNode(op)) => op.check(),
            Cst::PrefixBinary(PrefixBinaryNode(op)) => op.check(),
            Cst::Compound(CompoundNode(op)) => op.check(),
            Cst::Group(GroupNode(op)) => op.check(),
            // FIXME: Is this `false` by default, since it's unterminated and
            //        therefore invalid syntax?
            Cst::GroupMissingCloser(node) => node.check(),
            Cst::GroupMissingOpener(node) => node.check(),
            Cst::SyntaxError(node) => node.check(),
            Cst::Box(BoxNode { children, .. }) => children.check(),
            Cst::Code(_) => panic!("unexpected CodeNode in Cst::check()"),
        }
    }
}

//======================================
// LeafNode
//======================================

impl LeafNode {
    // pub fn symbol(sym: Symbol) -> Self {
    //     LeafNode {
    //         kind: TokenKind::Symbol,
    //         input: sym.as_str().to_owned(),
    //     }
    // }
}

//======================================
// OperatorNode
//======================================

impl<I, O> OperatorNode<I, Span, O> {
    pub(crate) fn new(op: O, children: CstNodeSeq<I>) -> Self {
        assert!(!children.is_empty());

        let src = Span::new_from_source(
            children.first().source(),
            children.last().source(),
        );

        OperatorNode {
            op,
            children,
            src: src,
        }
    }
}

impl<I, S, O: Copy> OperatorNode<I, S, O> {
    pub(crate) fn check(&self) -> bool {
        return self.children.check();
    }
}

impl<I, S: TokenSource, O: Copy> OperatorNode<I, S, O> {
    pub fn getSource(&self) -> S {
        return self.src.clone();
    }
}

impl<I: TokenInput, S, O> OperatorNode<I, S, O> {
    fn into_owned_input(self) -> OperatorNode<OwnedTokenInput, S, O> {
        let OperatorNode { op, children, src } = self;

        OperatorNode {
            op,
            children: children.into_owned_input(),
            src,
        }
    }
}

impl<I, S, O> OperatorNode<I, S, O> {
    /// Visit this node and every child node, recursively.
    fn visit_children(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        let OperatorNode {
            op: _,
            children,
            src: _,
        } = self;

        children.visit(visit);
    }

    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        let OperatorNode { op, children, src } = self;

        let children = children.map_visit(visit);

        OperatorNode { op, children, src }
    }
}

//======================================
// Missing closer nodes
//======================================

impl<I, S> GroupMissingCloserNode<I, S> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }
}

impl<I, S> GroupMissingOpenerNode<I, S> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }
}

//======================================
// Operator sub-type nodes
//======================================

impl<I> PrefixNode<I> {
    pub(crate) fn new(op: PrefixOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixNodeCount);

        PrefixNode(OperatorNode::new(op, args))
    }
}

impl<I> BinaryNode<I> {
    pub(crate) fn new(op: BinaryOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_BinaryNodeCount);

        BinaryNode(OperatorNode::new(op, args))
    }
}

impl<I> InfixNode<I> {
    pub(crate) fn new(op: InfixOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_InfixNodeCount);

        InfixNode(OperatorNode::new(op, args))
    }
}

impl<I> TernaryNode<I> {
    pub(crate) fn new(op: TernaryOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_TernaryNodeCount);

        TernaryNode(OperatorNode::new(op, args))
    }
}

impl<I> PostfixNode<I> {
    pub(crate) fn new(op: PostfixOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PostfixNodeCount);

        PostfixNode(OperatorNode::new(op, args))
    }
}

impl<I> PrefixBinaryNode<I> {
    pub(crate) fn new(op: PrefixBinaryOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixBinaryNodeCount);

        PrefixBinaryNode(OperatorNode::new(op, args))
    }
}

//======================================
// GroudNode and CompoundNode
//======================================

impl<I> GroupNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupNodeCount);

        GroupNode(OperatorNode::new(op, args))
    }
}

impl<I> CompoundNode<I> {
    pub(crate) fn new(op: CompoundOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_CompoundNodeCount);

        CompoundNode(OperatorNode::new(op, args))
    }
}

impl<I> GroupMissingCloserNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupMissingCloserNodeCount);

        GroupMissingCloserNode(OperatorNode::new(op, args))
    }
}

impl<I> UnterminatedGroupNeedsReparseNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_UnterminatedGroupNeedsReparseNodeCount);

        UnterminatedGroupNeedsReparseNode(OperatorNode::new(op, args))
    }
}

//======================================
// CallNode
//======================================

impl<I> CallNode<I> {
    pub(crate) fn concrete(head: CstNodeSeq<I>, body: CallBody<I>) -> Self {
        debug_assert!(!head.is_empty());

        incr_diagnostic!(Node_CallNodeCount);

        let src = Span::new_from_source(
            head.first().source(),
            body.as_op().getSource(),
        );

        CallNode {
            head: CallHead::Concrete(head),
            body,
            src,
        }
    }
}

impl<I, S: TokenSource> CallNode<I, S> {
    // pub(crate) fn group(head: NodeVariant<I>, group: GroupNode<I>) -> Self {
    //     CallNode::new(NodeSeq(vec![head]), NodeVariant::Cst(Cst::Group(group)))
    // }

    fn getSource(&self) -> S {
        return self.src.clone();
    }
}

impl<I, S> CallNode<I, S> {
    pub(crate) fn check(&self) -> bool {
        let CallNode { head, body, src: _ } = self;

        // Sanity check that check() isn't used on aggregate / abstract nodes.
        debug_assert!(matches!(head, CallHead::Concrete(_)));

        return head.check() && body.as_op().check();
    }
}

impl<I, S> CallHead<I, S> {
    pub fn aggregate(node: Cst<I, S>) -> Self {
        CallHead::Aggregate(Box::new(node))
    }

    /// Visit this node and every child node, recursively.
    pub fn visit(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        match self {
            CallHead::Concrete(head) => head.visit(visit),
            CallHead::Aggregate(head) => head.visit(visit),
        }
    }

    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        match self {
            CallHead::Concrete(head) => {
                CallHead::Concrete(head.map_visit(visit))
            },
            CallHead::Aggregate(head) => {
                let head: Cst<I, S> = *head;
                CallHead::Aggregate(Box::new(head.map_visit(visit)))
            },
        }
    }

    pub(crate) fn check(&self) -> bool {
        match self {
            CallHead::Concrete(head) => head.check(),
            CallHead::Aggregate(head) => head.check(),
        }
    }
}

impl<I, S> CallBody<I, S> {
    pub fn as_op(&self) -> &OperatorNode<I, S, CallOperator> {
        match self {
            CallBody::Group(GroupNode(op)) => op,
            CallBody::GroupMissingCloser(GroupMissingCloserNode(op)) => op,
        }
    }

    pub fn map_op<F, I2, S2>(self, func: F) -> CallBody<I2, S2>
    where
        F: FnOnce(
            OperatorNode<I, S, CallOperator>,
        ) -> OperatorNode<I2, S2, CallOperator>,
    {
        match self {
            CallBody::Group(GroupNode(op)) => {
                CallBody::Group(GroupNode(func(op)))
            },
            CallBody::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                CallBody::GroupMissingCloser(GroupMissingCloserNode(func(op)))
            },
        }
    }
}

//======================================
// SyntaxErrorNode
//======================================

impl<I> SyntaxErrorNode<I> {
    pub(crate) fn new(err: SyntaxErrorKind, children: CstNodeSeq<I>) -> Self {
        assert!(!children.is_empty());

        incr_diagnostic!(Node_SyntaxErrorNodeCount);

        let src = Span::new_from_source(
            children.first().source(),
            children.last().source(),
        );

        SyntaxErrorNode { err, children, src }
    }
}

impl<I, S> SyntaxErrorNode<I, S> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }
}

impl<I, S: TokenSource> SyntaxErrorNode<I, S> {
    fn getSource(&self) -> S {
        return self.src.clone();
    }
}

//==========================================================
// Operator Enums
//==========================================================

// TODO(cleanup): Import these from parse directly instead of re-exporting here.
pub use crate::parse::operators::{
    BinaryOperator, CallOperator, CompoundOperator, GroupOperator,
    InfixOperator, PostfixOperator, PrefixBinaryOperator, PrefixOperator,
    TernaryOperator,
};

/// Marker denoting enums whose variants represent named operators, which
/// can be converted to or from a corresponding canonical symbol representation.
pub trait Operator: Sized + 'static {
    fn to_symbol(&self) -> crate::symbol::Symbol;

    fn try_from_symbol(symbol: wolfram_expr::symbol::SymbolRef)
        -> Option<Self>;
}

impl GroupOperator {
    // FIXME: Make this function unnecessary by removing the GroupOperator
    //        variants that overlap with CallOperator. This will require some
    //        refactoring of how the parser parsing of CallParselet works.
    pub(crate) fn try_to_call_operator(self) -> Option<CallOperator> {
        let op = match self {
            GroupOperator::CodeParser_GroupSquare => {
                CallOperator::CodeParser_GroupSquare
            },
            GroupOperator::CodeParser_GroupTypeSpecifier => {
                CallOperator::CodeParser_GroupTypeSpecifier
            },
            GroupOperator::CodeParser_GroupDoubleBracket => {
                CallOperator::CodeParser_GroupDoubleBracket
            },
            GroupOperator::Token_Comment
            | GroupOperator::CodeParser_GroupParen
            | GroupOperator::List
            | GroupOperator::Association
            | GroupOperator::AngleBracket
            | GroupOperator::Ceiling
            | GroupOperator::Floor
            | GroupOperator::BracketingBar
            | GroupOperator::DoubleBracketingBar
            | GroupOperator::CurlyQuote
            | GroupOperator::CurlyDoubleQuote => {
                panic!("GroupOperator::{self:?} cannot be converted to CallOperator")
            },
        };

        Some(op)
    }
}

//======================================

impl SyntaxErrorKind {
    #[doc(hidden)]
    pub fn to_symbol(&self) -> crate::symbol::Symbol {
        use crate::symbols as sym;

        match self {
            SyntaxErrorKind::ExpectedSymbol => sym::SyntaxError_ExpectedSymbol,
            SyntaxErrorKind::ExpectedSet => sym::SyntaxError_ExpectedSet,
            SyntaxErrorKind::ExpectedTilde => sym::SyntaxError_ExpectedTilde,
        }
    }

    #[doc(hidden)]
    pub fn from_str(string: &str) -> Option<Self> {
        let value = match string {
            "ExpectedSymbol" => SyntaxErrorKind::ExpectedSymbol,
            "ExpectedSet" => SyntaxErrorKind::ExpectedSet,
            "ExpectedTilde" => SyntaxErrorKind::ExpectedTilde,
            _ => return None,
        };

        Some(value)
    }
}

impl BoxKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            BoxKind::TagBox => "TagBox",
            BoxKind::SuperscriptBox => "SuperscriptBox",
            BoxKind::RowBox => "RowBox",
            BoxKind::GridBox => "GridBox",
            BoxKind::TemplateBox => "TemplateBox",
            BoxKind::InterpretationBox => "InterpretationBox",
            BoxKind::SubscriptBox => "SubscriptBox",
            BoxKind::StyleBox => "StyleBox",
            BoxKind::NamespaceBox => "NamespaceBox",
            BoxKind::OverscriptBox => "OverscriptBox",
            BoxKind::SubsuperscriptBox => "SubsuperscriptBox",
            // NOTE: When adding a case here, also update from_str().
        }
    }

    #[doc(hidden)]
    pub fn from_str(string: &str) -> Option<Self> {
        let value = match string {
            "TagBox" => BoxKind::TagBox,
            "SuperscriptBox" => BoxKind::SuperscriptBox,
            "RowBox" => BoxKind::RowBox,
            "GridBox" => BoxKind::GridBox,
            "TemplateBox" => BoxKind::TemplateBox,
            "InterpretationBox" => BoxKind::InterpretationBox,
            "SubscriptBox" => BoxKind::SubscriptBox,
            "StyleBox" => BoxKind::StyleBox,
            "NamespaceBox" => BoxKind::NamespaceBox,
            "OverscriptBox" => BoxKind::OverscriptBox,
            "SubsuperscriptBox" => BoxKind::SubsuperscriptBox,
            _ => return None,
        };

        Some(value)
    }
}
