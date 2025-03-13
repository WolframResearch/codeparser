//! Input form concrete syntax trees.

use wolfram_expr::Expr;

use crate::{
    source::{GeneralSource, Source},
    token::{OwnedTokenInput, Token, TokenInput, TokenKind, TokenSource},
    NodeSeq,
};

pub use crate::parselet_registration::{
    CompoundOperator, GroupOperator, Operator, PrefixBinaryOperator,
};

// TODO: #[deprecated(note = "Use CstNode instead")]
pub(crate) type Node<I = OwnedTokenInput, S = Source> = CstNode<I, S>;

/// A sequence of concrete syntax tree nodes.
///
/// When parsing `a(**)+b`  we actually want to keep track of the comment.
/// But the comment does not affect the parsing: `a(**)` is still one "thing" to
/// the parser.
///
/// So pass around a structure that contains all of the nodes from the left,
/// including comments and whitespace.
pub type CstNodeSeq<I = OwnedTokenInput, S = Source> = NodeSeq<Node<I, S>>;

/// An expression representing a node in the syntax tree
#[derive(Debug, Clone, PartialEq)]
pub enum CstNode<I = OwnedTokenInput, S = Source> {
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
pub struct CodeNode<S = Source> {
    pub first: Expr,
    pub second: Expr,
    pub src: S,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoxNode<I = OwnedTokenInput, S = Source> {
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
pub struct OperatorNode<I = OwnedTokenInput, S = Source, O = Operator> {
    pub op: O,
    pub children: CstNodeSeq<I, S>,
    pub src: S,
}

/// `-a`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `a @ b`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// InfixNode
///
/// `a + b + c`
#[derive(Debug, Clone, PartialEq)]
pub struct InfixNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `a /: b = c`
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `a!`
#[derive(Debug, Clone, PartialEq)]
pub struct PostfixNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// PrefixBinaryNode
///
/// `\[Integral] f \[DifferentialD] x`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixBinaryNode<I = OwnedTokenInput, S = Source>(
    pub OperatorNode<I, S, PrefixBinaryOperator>,
);

/// `f[x]`
#[derive(Debug, Clone, PartialEq)]
pub struct CallNode<I = OwnedTokenInput, S = Source> {
    pub head: CstNodeSeq<I, S>,
    pub body: CallBody<I, S>,
    pub src: S,
    // Concrete Call nodes can have more than one element in `head`, and
    // serialize as `CallNode[{__}, ..]`
    //
    // Aggregate and abstract Call nodes must have exactly one element in `head`,
    // and serialize as `CallNode[node_, ..]`.
    pub is_concrete: bool,
}

/// Subset of [`CstNode`] variants that are allowed as the body of a [`CallNode`].
#[derive(Debug, Clone, PartialEq)]
pub enum CallBody<I = OwnedTokenInput, S = Source> {
    Group(GroupNode<I, S>),
    GroupMissingCloser(GroupMissingCloserNode<I, S>),
}

/// `{x}`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S, GroupOperator>);

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
pub struct CompoundNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S, CompoundOperator>);

/// A syntax error that contains structure.
#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxErrorNode<I = OwnedTokenInput, S = Source> {
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
pub struct GroupMissingCloserNode<I = OwnedTokenInput, S = Source>(
    pub OperatorNode<I, S, GroupOperator>,
);

/// Only possible with boxes
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingOpenerNode<I = OwnedTokenInput, S = Source>(
    pub OperatorNode<I, S, GroupOperator>,
);

/// `{`
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnterminatedGroupNeedsReparseNode<I = OwnedTokenInput, S = Source>(
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
    pub src: GeneralSource,
}

//======================================
// Node convertions
//======================================

macro_rules! from_node {
    ($name:ident => Node::$variant:ident) => {
        impl<I> From<$name> for Node<I> {
            fn from(node: $name) -> Node<I> {
                Node::$variant(node)
            }
        }
    };

    ($name:ident<I> => Node::$variant:ident) => {
        impl<I> From<$name<I>> for Node<I> {
            fn from(node: $name<I>) -> Node<I> {
                Node::$variant(node)
            }
        }
    };

    ($name:ident<I, S> => Node::$variant:ident) => {
        impl<I, S> From<$name<I, S>> for Node<I, S> {
            fn from(node: $name<I, S>) -> Node<I, S> {
                Node::$variant(node)
            }
        }
    };
}

from_node!(CallNode<I, S> => Node::Call);
from_node!(SyntaxErrorNode<I, S> => Node::SyntaxError);
from_node!(BinaryNode<I, S> => Node::Binary);
from_node!(TernaryNode<I, S> => Node::Ternary);
from_node!(InfixNode<I, S> => Node::Infix);
from_node!(PrefixNode<I, S> => Node::Prefix);
from_node!(PostfixNode<I, S> => Node::Postfix);
from_node!(PrefixBinaryNode<I, S> => Node::PrefixBinary);
from_node!(CompoundNode<I, S> => Node::Compound);
from_node!(GroupNode<I, S> => Node::Group);
from_node!(GroupMissingCloserNode<I, S> => Node::GroupMissingCloser);
from_node!(GroupMissingOpenerNode<I, S> => Node::GroupMissingOpener);
from_node!(BoxNode<I, S> => Node::Box);

impl<I, S> From<CodeNode<S>> for Node<I, S> {
    fn from(code: CodeNode<S>) -> Self {
        Node::Code(code)
    }
}

//==========================================================
// Impls
//==========================================================

//======================================
// NodeSeq
//======================================

impl<I, S> CstNodeSeq<I, S> {
    pub fn visit(&self, visit: &mut dyn FnMut(&Node<I, S>)) {
        let NodeSeq(elements) = self;

        for elem in elements {
            elem.visit(visit);
        }
    }

    pub fn map_visit(self, visit: &mut dyn FnMut(Node<I, S>) -> Node<I, S>) -> Self {
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

    // TODO: impl Display
    // void NodeSeq::print(std::ostream& s) const {

    //     SYMBOL_LIST.print(s);
    //     s << "[";

    //     for (auto& C : vec) {
    //         std::visit(PrintVisitor{s}, C);
    //         s << ", ";
    //     }

    //     s << "]";
    // }
}

impl<I, S: TokenSource> CstNodeSeq<I, S> {
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

        let nodes = nodes.into_iter().map(Node::into_owned_input).collect();

        NodeSeq(nodes)
    }
}

//==========================================================
// Nodes
//==========================================================

impl<I, S> Node<I, S> {
    /// Visit this node and every child node, recursively.
    pub fn visit(&self, visit: &mut dyn FnMut(&Node<I, S>)) {
        // Visit the current node.
        visit(self);

        // Visit child nodes.
        match self {
            Node::Token(_) => (),
            Node::Call(CallNode {
                head,
                body,
                src: _,
                is_concrete: _,
            }) => {
                head.visit(visit);
                body.as_op().visit_children(visit);
            },
            Node::SyntaxError(SyntaxErrorNode {
                err: _,
                children,
                src: _,
            }) => {
                children.visit(visit);
            },
            Node::Prefix(PrefixNode(op))
            | Node::Infix(InfixNode(op))
            | Node::Postfix(PostfixNode(op))
            | Node::Binary(BinaryNode(op))
            | Node::Ternary(TernaryNode(op)) => op.visit_children(visit),
            Node::PrefixBinary(PrefixBinaryNode(op)) => op.visit_children(visit),
            Node::Compound(CompoundNode(op)) => op.visit_children(visit),
            Node::Group(GroupNode(op))
            | Node::GroupMissingCloser(GroupMissingCloserNode(op))
            | Node::GroupMissingOpener(GroupMissingOpenerNode(op)) => op.visit_children(visit),
            Node::Box(BoxNode {
                kind: _,
                children,
                src: _,
            }) => {
                children.visit(visit);
            },
            // These node types have no child nodes.
            Node::Code(_) => (),
        }
    }

    /// Transform this node tree by visiting this node and every child node, recursively.
    pub fn map_visit(self, visit: &mut dyn FnMut(Node<I, S>) -> Node<I, S>) -> Self {
        // Visit the current node.
        let self_ = visit(self);

        // Visit child nodes.
        let node: Node<I, S> = match self_ {
            Node::Token(_) => return self_,
            Node::Call(CallNode {
                head,
                body,
                src,
                is_concrete,
            }) => {
                let head = head.map_visit(visit);

                let body = body.map_op(|body_op: OperatorNode<_, _, _>| body_op.map_visit(visit));

                Node::Call(CallNode {
                    head,
                    body,
                    src,
                    is_concrete,
                })
            },
            Node::SyntaxError(SyntaxErrorNode { err, children, src }) => {
                let children = children.map_visit(visit);

                Node::SyntaxError(SyntaxErrorNode { err, children, src })
            },

            Node::Infix(InfixNode(op)) => Node::Infix(InfixNode(op.map_visit(visit))),
            Node::Prefix(PrefixNode(op)) => Node::Prefix(PrefixNode(op.map_visit(visit))),
            Node::Postfix(PostfixNode(op)) => Node::Postfix(PostfixNode(op.map_visit(visit))),
            Node::Binary(BinaryNode(op)) => Node::Binary(BinaryNode(op.map_visit(visit))),
            Node::Ternary(TernaryNode(op)) => Node::Ternary(TernaryNode(op.map_visit(visit))),
            Node::PrefixBinary(PrefixBinaryNode(op)) => {
                Node::PrefixBinary(PrefixBinaryNode(op.map_visit(visit)))
            },
            Node::Compound(CompoundNode(op)) => Node::Compound(CompoundNode(op.map_visit(visit))),
            Node::Group(GroupNode(op)) => Node::Group(GroupNode(op.map_visit(visit))),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Node::GroupMissingCloser(GroupMissingCloserNode(op.map_visit(visit)))
            },
            Node::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                Node::GroupMissingOpener(GroupMissingOpenerNode(op.map_visit(visit)))
            },

            Node::Box(BoxNode {
                kind,
                children,
                src,
            }) => {
                let children = children.map_visit(visit);

                Node::Box(BoxNode {
                    kind,
                    children,
                    src,
                })
            },

            // These node types have no child nodes.
            node @ Node::Code(_) => node,
        };

        node
    }
}

impl<I: TokenInput, S> Node<I, S> {
    pub fn into_owned_input(self) -> Node<OwnedTokenInput, S> {
        match self {
            Node::Token(token) => Node::Token(token.into_owned_input()),
            Node::Call(CallNode {
                head,
                body,
                src,
                is_concrete,
            }) => Node::Call(CallNode {
                head: head.into_owned_input(),
                body: body.map_op(|body_op| body_op.into_owned_input()),
                src,
                is_concrete,
            }),
            Node::SyntaxError(SyntaxErrorNode { err, children, src }) => {
                Node::SyntaxError(SyntaxErrorNode {
                    err,
                    children: children.into_owned_input(),
                    src,
                })
            },
            Node::Prefix(PrefixNode(op)) => Node::Prefix(PrefixNode(op.into_owned_input())),
            Node::Infix(InfixNode(op)) => Node::Infix(InfixNode(op.into_owned_input())),
            Node::Postfix(PostfixNode(op)) => Node::Postfix(PostfixNode(op.into_owned_input())),
            Node::Binary(BinaryNode(op)) => Node::Binary(BinaryNode(op.into_owned_input())),
            Node::Ternary(TernaryNode(op)) => Node::Ternary(TernaryNode(op.into_owned_input())),
            Node::PrefixBinary(PrefixBinaryNode(op)) => {
                Node::PrefixBinary(PrefixBinaryNode(op.into_owned_input()))
            },
            Node::Compound(CompoundNode(op)) => Node::Compound(CompoundNode(op.into_owned_input())),
            Node::Group(GroupNode(op)) => Node::Group(GroupNode(op.into_owned_input())),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Node::GroupMissingCloser(GroupMissingCloserNode(op.into_owned_input()))
            },
            Node::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                Node::GroupMissingOpener(GroupMissingOpenerNode(op.into_owned_input()))
            },
            Node::Box(BoxNode {
                kind,
                children,
                src,
            }) => Node::Box(BoxNode {
                kind,
                children: children.into_owned_input(),
                src,
            }),
            Node::Code(node) => Node::Code(node),
        }
    }
}


impl<I, S: TokenSource> Node<I, S> {
    // TODO(cleanup): Combine with getSource()
    pub(crate) fn source(&self) -> S {
        self.getSource()
    }

    pub(crate) fn getSource(&self) -> S {
        match self {
            Node::Token(token) => token.src.clone(),
            Node::Call(node) => node.getSource(),
            Node::SyntaxError(node) => node.getSource(),
            Node::Prefix(PrefixNode(op)) => op.getSource(),
            Node::Infix(InfixNode(op)) => op.getSource(),
            Node::Postfix(PostfixNode(op)) => op.getSource(),
            Node::Binary(BinaryNode(op)) => op.getSource(),
            Node::PrefixBinary(PrefixBinaryNode(op)) => op.getSource(),
            Node::Ternary(TernaryNode(op)) => op.getSource(),
            Node::Compound(CompoundNode(op)) => op.getSource(),
            Node::Group(GroupNode(op)) => op.getSource(),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => op.getSource(),
            Node::GroupMissingOpener(GroupMissingOpenerNode(op)) => op.getSource(),
            Node::Box(BoxNode { src, .. }) => src.clone(),
            Node::Code(node) => node.src.clone(),
        }
    }

    // TODO(cleanup): Are these check() methods used anywhere? What do they even do?
    #[allow(dead_code)]
    fn check(&self) -> bool {
        match self {
            Node::Token(token) => token.check(),
            Node::Call(node) => node.check(),
            Node::Prefix(PrefixNode(op)) => op.check(),
            Node::Binary(BinaryNode(op)) => op.check(),
            Node::Infix(InfixNode(op)) => op.check(),
            Node::Ternary(TernaryNode(op)) => op.check(),
            Node::Postfix(PostfixNode(op)) => op.check(),
            Node::PrefixBinary(PrefixBinaryNode(op)) => op.check(),
            Node::Compound(CompoundNode(op)) => op.check(),
            Node::Group(GroupNode(op)) => op.check(),
            // FIXME: Is this `false` by default, since it's unterminated and
            //        therefore invalid syntax?
            Node::GroupMissingCloser(node) => node.check(),
            Node::GroupMissingOpener(node) => node.check(),
            Node::SyntaxError(node) => node.check(),
            Node::Box(BoxNode { children, .. }) => children.check(),
            Node::Code(_) => panic!("unexpected CodeNode in Node::check()"),
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

impl<I, O> OperatorNode<I, Source, O> {
    pub(crate) fn new(op: O, children: CstNodeSeq<I>) -> Self {
        assert!(!children.is_empty());

        let src = Source::new_from_source(children.first().source(), children.last().source());

        OperatorNode {
            op,
            children,
            src: src,
        }
    }
}

impl<I, S: TokenSource, O: Copy> OperatorNode<I, S, O> {
    pub fn getOp(&self) -> O {
        return self.op;
    }

    pub fn getSource(&self) -> S {
        return self.src.clone();
    }

    pub(crate) fn check(&self) -> bool {
        return self.children.check();
    }

    // TODO: impl Display
    // void print(std::ostream& s) const {

    //     MakeSym.print(s);
    //     s << "[";

    //     Op.print(s);
    //     s << ", ";

    //     children.print(s);
    //     s << ", ";

    //     src.print(s);

    //     s << "]";
    // }
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
    fn visit_children(&self, visit: &mut dyn FnMut(&Node<I, S>)) {
        let OperatorNode {
            op: _,
            children,
            src: _,
        } = self;

        children.visit(visit);
    }

    pub fn map_visit(self, visit: &mut dyn FnMut(Node<I, S>) -> Node<I, S>) -> Self {
        let OperatorNode { op, children, src } = self;

        let children = children.map_visit(visit);

        OperatorNode { op, children, src }
    }
}

//======================================
// Missing closer nodes
//======================================

impl<I, S: TokenSource> GroupMissingCloserNode<I, S> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }
}

impl<I, S: TokenSource> GroupMissingOpenerNode<I, S> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }
}

//======================================
// Operator sub-type nodes
//======================================

impl<I> PrefixNode<I> {
    pub(crate) fn new(op: Operator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixNodeCount);

        PrefixNode(OperatorNode::new(op, args))
    }
}

impl<I> BinaryNode<I> {
    pub(crate) fn new(op: Operator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_BinaryNodeCount);

        BinaryNode(OperatorNode::new(op, args))
    }
}

impl<I> InfixNode<I> {
    pub(crate) fn new(op: Operator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_InfixNodeCount);

        InfixNode(OperatorNode::new(op, args))
    }
}

impl<I> TernaryNode<I> {
    pub(crate) fn new(op: Operator, args: CstNodeSeq<I>) -> Self {
        incr_diagnostic!(Node_TernaryNodeCount);

        TernaryNode(OperatorNode::new(op, args))
    }
}

impl<I> PostfixNode<I> {
    pub(crate) fn new(op: Operator, args: CstNodeSeq<I>) -> Self {
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

        let src = Source::new_from_source(head.first().source(), body.as_op().getSource());

        CallNode {
            head,
            body,
            src,
            is_concrete: true,
        }
    }
}

impl<I, S: TokenSource> CallNode<I, S> {
    // pub(crate) fn group(head: NodeVariant<I>, group: GroupNode<I>) -> Self {
    //     CallNode::new(NodeSeq(vec![head]), NodeVariant::Node(Node::Group(group)))
    // }

    fn getSource(&self) -> S {
        return self.src.clone();
    }

    // TODO: Display
    // void print(std::ostream& s) const {

    //     SYMBOL_CODEPARSER_CALLNODE.print(s);
    //     s << "[";

    //     Head.print(s);
    //     s << ", ";

    //     std::visit(PrintVisitor{s}, Body);
    //     s << ", ";

    //     src.print(s);

    //     s << "]";
    // }

    pub(crate) fn check(&self) -> bool {
        let CallNode {
            head,
            body,
            src: _,
            is_concrete,
        } = self;

        // Sanity check that check() isn't used on aggregate / abstract nodes.
        debug_assert!(is_concrete);

        return head.check() && body.as_op().check();
    }
}

impl<I, S> CallBody<I, S> {
    pub fn as_op(&self) -> &OperatorNode<I, S, GroupOperator> {
        match self {
            CallBody::Group(GroupNode(op)) => op,
            CallBody::GroupMissingCloser(GroupMissingCloserNode(op)) => op,
        }
    }

    pub fn map_op<F, I2, S2>(self, func: F) -> CallBody<I2, S2>
    where
        F: FnOnce(OperatorNode<I, S, GroupOperator>) -> OperatorNode<I2, S2, GroupOperator>,
    {
        match self {
            CallBody::Group(GroupNode(op)) => CallBody::Group(GroupNode(func(op))),
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

        let src = Source::new_from_source(children.first().source(), children.last().source());

        SyntaxErrorNode { err, children, src }
    }
}

impl<I, S: TokenSource> SyntaxErrorNode<I, S> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }

    fn getSource(&self) -> S {
        return self.src.clone();
    }

    // TODO: Display
    // void SyntaxErrorNode::print(std::ostream& s) const {

    //     SYMBOL_CODEPARSER_SYNTAXERRORNODE.print(s);
    //     s << "[";

    //     s << Err.name;
    //     s << ", ";

    //     children.print(s);
    //     s << ", ";

    //     src.print(s);

    //     s << "]";
    // }
}

//======================================

impl SyntaxErrorKind {
    #[doc(hidden)]
    pub fn to_symbol(&self) -> crate::symbol::Symbol {
        use crate::symbol_registration as sym;

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
