//! Input form concrete syntax trees.
//!
//! [`Cst`] — root and element type in a concrete syntax tree.

mod visit;

use wolfram_expr::{symbol::SymbolRef, Expr};

use crate::{
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
    // TODO(cleanup):
    //   Make this unnecessary? Try to represent _every_ box kind
    //   as a variant? Or represent all box kinds as a Symbol
    //   field?
    /// Must be a `` System` `` symbol that ends in "Box".
    Other(wolfram_expr::Symbol),
}

/// Any kind of prefix, postfix, binary, or infix operator
#[derive(Debug, Clone, PartialEq)]
pub struct OperatorNode<I = TokenString, S = Span, O = InfixOperator> {
    pub op: O,
    pub children: CstSeq<I, S>,
    pub src: S,
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
    pub src: S,
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SyntaxErrorKind {
    ExpectedSymbol,
    ExpectedSet,
    ExpectedTilde,
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

/// `{`
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnterminatedGroupNeedsReparseNode<I = TokenString, S = Span>(
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
        self.getSource()
    }

    pub(crate) fn getSource(&self) -> S {
        match self {
            Cst::Token(token) => token.src.clone(),
            Cst::Call(node) => node.getSource(),
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

impl<I, O> OperatorNode<I, Span, O> {
    pub(crate) fn new(op: O, children: CstSeq<I>) -> Self {
        assert!(!children.is_empty());

        let src =
            Span::between(children.first().source(), children.last().source());

        OperatorNode {
            op,
            children,
            src: src,
        }
    }
}

impl<I, S: TokenSource, O: Copy> OperatorNode<I, S, O> {
    pub fn getSource(&self) -> S {
        return self.src.clone();
    }
}

impl<I: TokenInput, S, O> OperatorNode<I, S, O> {
    fn into_owned_input(self) -> OperatorNode<TokenString, S, O> {
        let OperatorNode { op, children, src } = self;

        OperatorNode {
            op,
            children: children.into_owned_input(),
            src,
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

    pub(crate) fn new2(
        op: PrefixOperator,
        tok1: Token<I>,
        TriviaSeq(trivia): TriviaSeq<I>,
        tok2: Token<I>,
    ) -> Self {
        let mut args: Vec<Cst<I>> = Vec::with_capacity(trivia.len() + 2);
        args.push(Cst::Token(tok1));
        args.extend(trivia.into_iter().map(Cst::Token));
        args.push(Cst::Token(tok2));

        PrefixNode(OperatorNode::new(op, NodeSeq(args)))
    }
}

impl<I> BinaryNode<I> {
    pub(crate) fn new(op: BinaryOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_BinaryNodeCount);

        BinaryNode(OperatorNode::new(op, args))
    }

    pub(crate) fn new2(
        op: BinaryOperator,
        // Operator
        op_token: Token<I>,
        TriviaSeq(trivia): TriviaSeq<I>,
        // Operand
        rand_token: Token<I>,
    ) -> Self {
        let mut args = Vec::with_capacity(trivia.len() + 2);
        args.push(Cst::Token(op_token));
        args.extend(trivia.into_iter().map(Cst::Token));
        args.push(Cst::Token(rand_token));

        BinaryNode(OperatorNode::new(op, NodeSeq(args)))
    }
}

impl<I> InfixNode<I> {
    pub(crate) fn new(op: InfixOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_InfixNodeCount);

        InfixNode(OperatorNode::new(op, args))
    }
}

impl<I> TernaryNode<I> {
    pub(crate) fn new(op: TernaryOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_TernaryNodeCount);

        TernaryNode(OperatorNode::new(op, args))
    }
}

impl<I> PostfixNode<I> {
    pub(crate) fn new(op: PostfixOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_PostfixNodeCount);

        PostfixNode(OperatorNode::new(op, args))
    }
}

impl<I> PrefixBinaryNode<I> {
    pub(crate) fn new(op: PrefixBinaryOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixBinaryNodeCount);

        PrefixBinaryNode(OperatorNode::new(op, args))
    }
}

//======================================
// GroudNode and CompoundNode
//======================================

impl<I> GroupNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupNodeCount);

        GroupNode(OperatorNode::new(op, args))
    }
}

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

impl<I> GroupMissingCloserNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupMissingCloserNodeCount);

        GroupMissingCloserNode(OperatorNode::new(op, args))
    }
}

impl<I> UnterminatedGroupNeedsReparseNode<I> {
    pub(crate) fn new(op: GroupOperator, args: CstSeq<I>) -> Self {
        incr_diagnostic!(Node_UnterminatedGroupNeedsReparseNodeCount);

        UnterminatedGroupNeedsReparseNode(OperatorNode::new(op, args))
    }
}

//======================================
// CallNode
//======================================

impl<I> CallNode<I> {
    pub(crate) fn concrete(head: CstSeq<I>, body: CallBody<I>) -> Self {
        debug_assert!(!head.is_empty());

        incr_diagnostic!(Node_CallNodeCount);

        let src =
            Span::between(head.first().source(), body.as_op().getSource());

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

        // FIXME: This recursive source getting might be slower than necessary
        //        because we get the full source for each child and then only
        //        use the start half and end half respectively.
        //        Enforce this by making Span::between() take Location instead
        //        (and then remove it because its redundant with Span::new at
        //        that point.)
        S::between(children.first().source(), children.last().source())
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
            BoxKind::Other(name) => {
                if name.context().as_str() == "System`" {
                    name.symbol_name().as_str()
                } else {
                    todo!("PRECOMMIT")
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
