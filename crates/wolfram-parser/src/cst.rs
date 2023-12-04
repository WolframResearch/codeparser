//! Input form concrete syntax trees.
//!
//! [`Cst`] — root and element type in a concrete syntax tree.

mod visit;

use std::fmt::Debug;

use wolfram_expr::{symbol::SymbolRef, Expr};

use crate::{
    parse::{
        operators::{
            BinaryOperator, CallOperator, CompoundOperator, GroupOperator,
            InfixOperator, PostfixOperator, PrefixBinaryOperator,
            PrefixOperator, TernaryOperator,
        },
        SyntaxErrorKind,
    },
    source::{Source, Span},
    tokenize::{Token, TokenInput, TokenKind, TokenSource, TokenString},
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
pub type CstSeq<I = TokenString, S = Span> = NodeSeq<Cst<I, S>>;

/// Sequence of non-semantically-meaningful tokens in the input, i.e. Whitespace
/// and Comment.
///
/// See also: [`TokenKind::isTrivia()`].
#[derive(Debug)]
pub struct TriviaSeq<I>(pub Vec<Token<I>>);

/// A concrete syntax tree (CST) node.
///
/// If this was parsed from well-formed input (i.e. has no internal syntax error
/// nodes), a single [`Cst`] represents a Wolfram Language expression.
///
/// A typical [`Cst`] is made up of further child syntax trees. A [`Cst`] tree
/// terminates at "leaf" variants such as [`Cst::Token`].
#[derive(Debug, Clone, PartialEq)]
pub enum Cst<I = TokenString, S = Span> {
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
pub struct BoxNode<I = TokenString, S = Span> {
    pub kind: BoxKind,
    pub children: CstSeq<I, S>,
    pub src: S,
}

#[derive(Debug, Clone, PartialEq)]
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
    GraphicsBox,
    // TODO(cleanup):
    //   Make this unnecessary? Try to represent _every_ box kind
    //   as a variant? Or represent all box kinds as a Symbol
    //   field?
    /// Must be a `` System` `` symbol that ends in "Box".
    Other(wolfram_expr::Symbol),
}

/// Any kind of prefix, postfix, binary, or infix operator
#[derive(Clone, PartialEq)]
pub struct OperatorNode<I = TokenString, S = Span, O = InfixOperator> {
    pub op: O,
    pub children: CstSeq<I, S>,
}

/// `-a`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixNode<I = TokenString, S = Span>(
    pub OperatorNode<I, S, PrefixOperator>,
);

/// `a @ b`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryNode<I = TokenString, S = Span>(
    pub OperatorNode<I, S, BinaryOperator>,
);

/// `a + b + c`
#[derive(Debug, Clone, PartialEq)]
pub struct InfixNode<I = TokenString, S = Span>(pub OperatorNode<I, S>);

/// `a /: b = c`
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryNode<I = TokenString, S = Span>(
    pub OperatorNode<I, S, TernaryOperator>,
);

/// `a!`
#[derive(Debug, Clone, PartialEq)]
pub struct PostfixNode<I = TokenString, S = Span>(
    pub OperatorNode<I, S, PostfixOperator>,
);

/// `\[Integral] f \[DifferentialD] x`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixBinaryNode<I = TokenString, S = Span>(
    pub OperatorNode<I, S, PrefixBinaryOperator>,
);

/// `f[x]`
#[derive(Debug, Clone, PartialEq)]
pub struct CallNode<I = TokenString, S = Span> {
    pub head: CallHead<I, S>,
    pub body: CallBody<I, S>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallHead<I, S> {
    /// Concrete Call nodes can have more than one element in `head`, and
    /// serialize as `CallNode[{__}, ..]`
    ///
    /// Happens for e.g. `f [ x ]`, where the whitespace after `f` is a token
    /// associated with the head.
    Concrete(CstSeq<I, S>),

    /// Aggregate and abstract Call nodes must have exactly one element in `head`,
    /// and serialize as `CallNode[node_, ..]`.
    Aggregate(Box<Cst<I, S>>),
}

/// Subset of [`Cst`] variants that are allowed as the body of a [`CallNode`].
#[derive(Debug, Clone, PartialEq)]
pub enum CallBody<I = TokenString, S = Span> {
    Group(GroupNode<I, S, CallOperator>),
    GroupMissingCloser(GroupMissingCloserNode<I, S, CallOperator>),
}

/// `{x}`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupNode<I = TokenString, S = Span, O = GroupOperator>(
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
pub struct CompoundNode<I = TokenString, S = Span>(
    pub OperatorNode<I, S, CompoundOperator>,
);

/// A syntax error that contains structure.
#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxErrorNode<I = TokenString, S = Span> {
    pub err: SyntaxErrorKind,
    pub children: CstSeq<I, S>,
}

/// `{]`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingCloserNode<I = TokenString, S = Span, O = GroupOperator>(
    pub OperatorNode<I, S, O>,
);

/// Only possible with boxes
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingOpenerNode<I = TokenString, S = Span>(
    pub OperatorNode<I, S, GroupOperator>,
);

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

impl<I, S> From<Token<I, S>> for Cst<I, S> {
    fn from(token: Token<I, S>) -> Self {
        Cst::Token(token)
    }
}

//==========================================================
// Impls
//==========================================================

//======================================
// NodeSeq
//======================================

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

impl<I, S: TokenSource> CstSeq<I, S> {
    pub(crate) fn get_source(&self) -> S {
        // FIXME: This recursive source getting might be slower than necessary
        //        because we get the full source for each child and then only
        //        use the start half and end half respectively.
        //        Enforce this by making Span::between() take Location instead
        //        (and then remove it because its redundant with Span::new at
        //        that point.)
        S::between(self.first().source(), self.last().source())
    }
}

impl<I: TokenInput, S> CstSeq<I, S> {
    pub(crate) fn into_owned_input(self) -> CstSeq<TokenString, S> {
        let NodeSeq(nodes) = self;

        let nodes = nodes.into_iter().map(Cst::into_owned_input).collect();

        NodeSeq(nodes)
    }
}

//==========================================================
// Nodes
//==========================================================

impl<I: TokenInput, S> Cst<I, S> {
    pub fn into_owned_input(self) -> Cst<TokenString, S> {
        match self {
            Cst::Token(token) => Cst::Token(token.into_owned_input()),
            Cst::Call(CallNode { head, body }) => Cst::Call(CallNode {
                head: match head {
                    CallHead::Concrete(head) => {
                        CallHead::Concrete(head.into_owned_input())
                    },
                    CallHead::Aggregate(head) => CallHead::Aggregate(Box::new(
                        (*head).into_owned_input(),
                    )),
                },
                body: body.map_op(|body_op| body_op.into_owned_input()),
            }),
            Cst::SyntaxError(SyntaxErrorNode { err, children }) => {
                Cst::SyntaxError(SyntaxErrorNode {
                    err,
                    children: children.into_owned_input(),
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
        self.get_source()
    }

    pub fn get_source(&self) -> S {
        match self {
            Cst::Token(token) => token.src.clone(),
            Cst::Call(node) => node.get_source(),
            Cst::SyntaxError(node) => node.get_source(),
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

impl<I, S, O> OperatorNode<I, S, O> {
    pub(crate) fn new(op: O, children: CstSeq<I, S>) -> Self {
        assert!(!children.is_empty());

        OperatorNode { op, children }
    }
}

impl<I, S: TokenSource, O> OperatorNode<I, S, O> {
    pub fn get_source(&self) -> S {
        let OperatorNode { op: _, children } = self;

        children.get_source()
    }

    pub(crate) fn getSource(&self) -> S {
        self.get_source()
    }
}

impl<I: TokenInput, S, O> OperatorNode<I, S, O> {
    fn into_owned_input(self) -> OperatorNode<TokenString, S, O> {
        let OperatorNode { op, children } = self;

        OperatorNode {
            op,
            children: children.into_owned_input(),
        }
    }
}

//======================================
// Operator sub-type nodes
//======================================

impl<I> PrefixNode<I> {
    pub(crate) fn new(op: PrefixOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixNodeCount);

        PrefixNode(OperatorNode::new(op, args))
    }
}

impl<I, S: TokenSource> PrefixNode<I, S> {
    pub fn get_source(&self) -> S {
        let PrefixNode(op) = self;

        op.get_source()
    }
}

impl<I> BinaryNode<I> {
    pub(crate) fn new(op: BinaryOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_BinaryNodeCount);

        BinaryNode(OperatorNode::new(op, args))
    }
}

impl<I, S: TokenSource> BinaryNode<I, S> {
    pub fn get_source(&self) -> S {
        let BinaryNode(op) = self;

        op.get_source()
    }
}

//======================================
// InfixNode
//======================================

impl<I> InfixNode<I> {
    pub(crate) fn new(op: InfixOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_InfixNodeCount);

        InfixNode(OperatorNode::new(op, args))
    }
}


impl<I, S: TokenSource> InfixNode<I, S> {
    pub fn get_source(&self) -> S {
        let InfixNode(op) = self;

        op.get_source()
    }
}

//======================================
// TernaryNode
//======================================

impl<I> TernaryNode<I> {
    pub(crate) fn new(op: TernaryOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_TernaryNodeCount);

        TernaryNode(OperatorNode::new(op, args))
    }
}

impl<I, S: TokenSource> TernaryNode<I, S> {
    pub fn get_source(&self) -> S {
        let TernaryNode(op) = self;

        op.get_source()
    }
}

//======================================
// PostfixNode
//======================================

impl<I> PostfixNode<I> {
    pub(crate) fn new(op: PostfixOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_PostfixNodeCount);

        PostfixNode(OperatorNode::new(op, args))
    }
}

impl<I, S: TokenSource> PostfixNode<I, S> {
    pub fn get_source(&self) -> S {
        let PostfixNode(op) = self;

        op.get_source()
    }
}

//======================================
// PrefixBinaryNode
//======================================

impl<I> PrefixBinaryNode<I> {
    pub(crate) fn new(op: PrefixBinaryOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixBinaryNodeCount);

        PrefixBinaryNode(OperatorNode::new(op, args))
    }
}

impl<I, S: TokenSource> PrefixBinaryNode<I, S> {
    pub fn get_source(&self) -> S {
        let PrefixBinaryNode(op) = self;

        op.get_source()
    }
}

//======================================
// GroupNode
//======================================

impl<I> GroupNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupNodeCount);

        GroupNode(OperatorNode::new(op, args))
    }
}

impl<I, S: TokenSource, O> GroupNode<I, S, O> {
    pub fn get_source(&self) -> S {
        let GroupNode(op) = self;

        op.get_source()
    }
}

//======================================
// GroupMissingCloserNode
//======================================

impl<I> GroupMissingCloserNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupMissingCloserNodeCount);

        GroupMissingCloserNode(OperatorNode::new(op, args))
    }
}

impl<I, S: TokenSource, O> GroupMissingCloserNode<I, S, O> {
    pub fn get_source(&self) -> S {
        let GroupMissingCloserNode(op) = self;

        op.get_source()
    }
}

//======================================
// GroupMissingOpenerNode
//======================================

impl<I, S: TokenSource> GroupMissingOpenerNode<I, S> {
    pub fn get_source(&self) -> S {
        let GroupMissingOpenerNode(op) = self;

        op.get_source()
    }
}

//======================================
// CompoundNode
//======================================

impl<I> CompoundNode<I> {
    pub(crate) fn new2(
        op: CompoundOperator,
        tok1: Token<I>,
        tok2: Token<I>,
    ) -> Self {
        CompoundNode(OperatorNode::new(
            op,
            NodeSeq(vec![Cst::Token(tok1), Cst::Token(tok2)]),
        ))
    }

    pub(crate) fn new3(
        op: CompoundOperator,
        arg1: Token<I>,
        arg2: Cst<I>,
    ) -> Self {
        CompoundNode(OperatorNode::new(
            op,
            NodeSeq(vec![Cst::Token(arg1), arg2]),
        ))
    }
}

impl<I, S: TokenSource> CompoundNode<I, S> {
    pub fn get_source(&self) -> S {
        let CompoundNode(op) = self;

        op.get_source()
    }
}

//======================================
// CallNode
//======================================

impl<I> CallNode<I> {
    pub(crate) fn concrete(head_seq: CstSeq<I>, body: CallBody<I>) -> Self {
        incr_diagnostic!(Node_CallNodeCount);

        CallNode {
            head: CallHead::Concrete(head_seq),
            body,
        }
    }
}

impl<I, S: TokenSource> CallNode<I, S> {
    // pub(crate) fn group(head: NodeVariant<I>, group: GroupNode<I>) -> Self {
    //     CallNode::new(NodeSeq(vec![head]), NodeVariant::Cst(Cst::Group(group)))
    // }

    pub fn get_source(&self) -> S {
        let CallNode { head, body } = self;

        let start = match head {
            CallHead::Concrete(head_seq) => head_seq.first().source(),
            CallHead::Aggregate(head_cst) => head_cst.source(),
        };

        let src = S::between(start, body.as_op().getSource());

        return src;
    }
}

impl<I, S> CallHead<I, S> {
    pub fn aggregate(node: Cst<I, S>) -> Self {
        CallHead::Aggregate(Box::new(node))
    }
}

impl<I, S> CallBody<I, S> {
    pub fn as_op(&self) -> &OperatorNode<I, S, CallOperator> {
        match self {
            CallBody::Group(GroupNode(op)) => op,
            CallBody::GroupMissingCloser(GroupMissingCloserNode(op)) => op,
        }
    }

    pub fn as_op_mut(&mut self) -> &mut OperatorNode<I, S, CallOperator> {
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

impl<I, S: TokenSource> CallBody<I, S> {
    pub fn get_source(&self) -> S {
        match self {
            CallBody::Group(group) => group.get_source(),
            CallBody::GroupMissingCloser(group) => group.get_source(),
        }
    }
}

//======================================
// SyntaxErrorNode
//======================================

impl<I> SyntaxErrorNode<I> {
    pub(crate) fn new(err: SyntaxErrorKind, children: CstSeq<I>) -> Self {
        assert!(!children.is_empty());

        incr_diagnostic!(Node_SyntaxErrorNodeCount);


        SyntaxErrorNode { err, children }
    }
}

impl<I, S: TokenSource> SyntaxErrorNode<I, S> {
    /// Compute the source span covered by this node.
    pub fn get_source(&self) -> S {
        let SyntaxErrorNode { err: _, children } = self;

        children.get_source()
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
    pub fn as_str(&self) -> &str {
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
            BoxKind::GraphicsBox => "GraphicsBox",
            // NOTE: When adding a case here, also update from_str().
            BoxKind::Other(name) => {
                if name.context().as_str() == "System`" {
                    name.symbol_name().as_str()
                } else {
                    panic!("BoxKind::Other(_) must be a System` symbol, but was: {name}");
                }
            }, // NOTE: When adding a case here, also update from_str().
        }
    }

    fn from_str(string: &str) -> Option<Self> {
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
            "GraphicsBox" => BoxKind::GraphicsBox,
            _ => return None,
        };

        Some(value)
    }

    pub fn from_symbol(symbol: SymbolRef) -> Option<Self> {
        let context = symbol.context().as_str();
        let symbol_name = symbol.symbol_name().as_str();

        if context != "System`" {
            todo!()
        }

        if !symbol_name.ends_with("Box") {
            return None;
        }

        Some(
            BoxKind::from_str(symbol_name)
                .unwrap_or_else(|| BoxKind::Other(symbol.to_symbol())),
        )
    }
}

//======================================
// Formatting Impls
//======================================

impl<I: Debug, S: Debug, O: Debug> Debug for OperatorNode<I, S, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OperatorNode")
            .field("op", &self.op)
            .field("children", &self.children)
            .finish()
    }
}

struct FmtAlternate<'a, T: Debug>(&'a T);

impl<'a, T: Debug> Debug for FmtAlternate<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAlternate(value) = self;

        write!(f, "{:#?}.into()", value)
    }
}
