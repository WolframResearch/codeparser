use wolfram_expr::Expr;

use crate::{
    source::{GeneralSource, Source},
    token::{BorrowedTokenInput, OwnedTokenInput, Token, TokenKind, TokenRef},
    tokenizer::Tokenizer,
};

pub use crate::parselet_registration::Operator;

//
// Used mainly for collecting trivia that has been eaten
//
#[derive(Debug)]
pub(crate) struct TriviaSeq<'i> {
    pub vec: Vec<Token<BorrowedTokenInput<'i>>>,
}

//
// A sequence of Nodes
//
// When parsing  a(**)+b  we actually want to keep track of the comment.
// But the comment does not affect the parsing: a(**) is still 1 "thing" to the parser
//
// So pass around a structure that contains all of the nodes from the left, including comments and whitespace.
//
#[derive(Debug, Clone, PartialEq)]
pub struct NodeSeq<I = OwnedTokenInput, S = Source>(pub Vec<Node<I, S>>);

/// An expression representing a node in the syntax tree
#[derive(Debug, Clone, PartialEq)]
pub enum Node<I = OwnedTokenInput, S = Source> {
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
    // TODO(cleanup): This variant is never constructed during concrete parsing.
    UnterminatedGroup(UnterminatedGroupNode<I, S>),
    GroupMissingCloser(GroupMissingCloserNode<I, S>),
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
    pub children: NodeSeq<I, S>,
    pub src: S,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BoxKind {
    Tag,
    Superscript,
}

/// Any kind of prefix, postfix, binary, or infix operator
#[derive(Debug, Clone, PartialEq)]
pub struct OperatorNode<I = OwnedTokenInput, S = Source> {
    pub(crate) op: Operator,
    pub(crate) children: NodeSeq<I, S>,
    pub(crate) src: S,
}

/// `-a`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `a @ b`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

//
// InfixNode
//
// a + b + c
//
#[derive(Debug, Clone, PartialEq)]
pub struct InfixNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `a /: b = c`
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `a!`
#[derive(Debug, Clone, PartialEq)]
pub struct PostfixNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

//
// PrefixBinaryNode
//
// \[Integral] f \[DifferentialD] x
//
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixBinaryNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `f[x]`
#[derive(Debug, Clone, PartialEq)]
pub struct CallNode<I = OwnedTokenInput, S = Source> {
    pub head: NodeSeq<I, S>,
    pub body: Box<Node<I, S>>,
    pub src: S,
    // Concrete Call nodes can have more than one element in `head`, and
    // serialize as `CallNode[{__}, ..]`
    //
    // Aggregate and abstract Call nodes must have exactly one element in `head`,
    // and serialize as `CallNode[node_, ..]`.
    pub is_concrete: bool,
}

/// `{x}`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

// TODO: This type is constructed as part of the abstraction step. Should there
//       be different CstNode and AstNode types so that this node is not part of
//       CstNode?
#[derive(Debug, Clone, PartialEq)]
pub struct UnterminatedGroupNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

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
pub struct CompoundNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// A syntax error that contains structure.
#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxErrorNode<I = OwnedTokenInput, S = Source> {
    pub err: SyntaxErrorKind,
    pub children: NodeSeq<I, S>,
    pub src: S,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxErrorKind {
    ExpectedSymbol,
    ExpectedSet,
    ExpectedTilde,
}

/// `{]`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingCloserNode<I = OwnedTokenInput, S = Source>(pub OperatorNode<I, S>);

/// `{`
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnterminatedGroupNeedsReparseNode<I = OwnedTokenInput, S = Source>(
    pub OperatorNode<I, S>,
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
from_node!(UnterminatedGroupNode<I, S> => Node::UnterminatedGroup);
from_node!(GroupMissingCloserNode<I, S> => Node::GroupMissingCloser);
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

impl<I> NodeSeq<I> {
    pub fn visit(&self, visit: &mut dyn FnMut(&Node<I>)) {
        let NodeSeq(elements) = self;

        for elem in elements {
            elem.visit(visit);
        }
    }

    pub fn map_visit(self, visit: &mut dyn FnMut(Node<I>) -> Node<I>) -> Self {
        let NodeSeq(elements) = self;

        let elements = elements
            .into_iter()
            .map(|elem| elem.map_visit(visit))
            .collect();

        NodeSeq(elements)
    }
}

impl<I> NodeSeq<I> {
    pub(crate) fn new() -> NodeSeq<I> {
        NodeSeq(Vec::new())
    }

    pub fn push<N: Into<Node<I>>>(&mut self, node: N) {
        let NodeSeq(vec) = self;

        let node = node.into();
        vec.push(node);
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

    fn first(&self) -> &Node<I> {
        let NodeSeq(vec) = self;
        vec.first().expect("NodeSeq::first(): vector is empty")
    }

    fn last(&self) -> &Node<I> {
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

impl NodeSeq<BorrowedTokenInput<'_>> {
    pub(crate) fn into_owned_input(self) -> NodeSeq {
        let NodeSeq(nodes) = self;

        let nodes = nodes.into_iter().map(Node::into_owned_input).collect();

        NodeSeq(nodes)
    }
}

//======================================
// TriviaSeq
//======================================

impl<'i> TriviaSeq<'i> {
    pub(crate) fn new() -> Self {
        TriviaSeq { vec: Vec::new() }
    }

    pub fn reset(&mut self, session: &mut Tokenizer) {
        let TriviaSeq { vec } = self;

        //
        // Just need to reset the global buffer to the buffer of the first token in the sequence
        //

        if vec.is_empty() {
            return;
        }

        let T = &vec[0];

        session.offset = T.input.byte_span().offset;
        session.SrcLoc = T.src.start;

        vec.clear();
    }

    pub fn push(&mut self, token: TokenRef<'i>) {
        self.vec.push(token);
    }

    pub fn is_empty(&self) -> bool {
        return self.vec.is_empty();
    }

    pub fn clear(&mut self) {
        let TriviaSeq { vec } = self;

        vec.clear();
    }
}

//==========================================================
// Nodes
//==========================================================

impl<I> Node<I> {
    /// Visit this node and every child node, recursively.
    pub fn visit(&self, visit: &mut dyn FnMut(&Node<I>)) {
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

                body.visit(visit);
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
            | Node::Ternary(TernaryNode(op))
            | Node::PrefixBinary(PrefixBinaryNode(op))
            | Node::Compound(CompoundNode(op))
            | Node::Group(GroupNode(op))
            | Node::GroupMissingCloser(GroupMissingCloserNode(op))
            | Node::UnterminatedGroup(UnterminatedGroupNode(op)) => {
                let OperatorNode {
                    op: _,
                    children,
                    src: _,
                } = op;

                children.visit(visit);
            },
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
    pub fn map_visit(self, visit: &mut dyn FnMut(Node<I>) -> Node<I>) -> Self {
        // Visit the current node.
        let self_ = visit(self);

        // Visit child nodes.
        let node: Node<I> = match self_ {
            Node::Token(_) => return self_,
            Node::Call(CallNode {
                head,
                body,
                src,
                is_concrete,
            }) => {
                let head = head.map_visit(visit);

                let body = body.map_visit(visit);

                Node::Call(CallNode {
                    head,
                    body: Box::new(body),
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
            Node::UnterminatedGroup(UnterminatedGroupNode(op)) => {
                Node::UnterminatedGroup(UnterminatedGroupNode(op.map_visit(visit)))
            },
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Node::GroupMissingCloser(GroupMissingCloserNode(op.map_visit(visit)))
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

impl Node<BorrowedTokenInput<'_>> {
    pub fn into_owned_input(self) -> Node {
        match self {
            Node::Token(token) => Node::Token(token.into_owned_input()),
            Node::Call(CallNode {
                head,
                body,
                src,
                is_concrete,
            }) => Node::Call(CallNode {
                head: head.into_owned_input(),
                body: Box::new(body.into_owned_input()),
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
            Node::UnterminatedGroup(UnterminatedGroupNode(op)) => {
                Node::UnterminatedGroup(UnterminatedGroupNode(op.into_owned_input()))
            },
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Node::GroupMissingCloser(GroupMissingCloserNode(op.into_owned_input()))
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

impl<I> Node<I> {
    // TODO(cleanup): Combine with getSource()
    fn source(&self) -> Source {
        self.getSource()
    }

    pub(crate) fn getSource(&self) -> Source {
        match self {
            Node::Token(token) => token.src,
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
            Node::UnterminatedGroup(UnterminatedGroupNode(op)) => op.getSource(),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => op.getSource(),
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
            Node::UnterminatedGroup(UnterminatedGroupNode(op)) => op.check(),
            Node::GroupMissingCloser(node) => node.check(),
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

impl<I> OperatorNode<I> {
    pub(crate) fn new(op: Operator, children: NodeSeq<I>) -> Self {
        assert!(!children.is_empty());

        let src = Source::new_from_source(children.first().source(), children.last().source());

        OperatorNode {
            op,
            children,
            src: src,
        }
    }

    pub fn getOp(&self) -> Operator {
        return self.op;
    }

    pub fn getSource(&self) -> Source {
        return self.src;
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

impl OperatorNode<BorrowedTokenInput<'_>> {
    fn into_owned_input(self) -> OperatorNode {
        let OperatorNode { op, children, src } = self;

        OperatorNode {
            op,
            children: children.into_owned_input(),
            src,
        }
    }
}

impl<I> OperatorNode<I> {
    pub fn map_visit(self, visit: &mut dyn FnMut(Node<I>) -> Node<I>) -> Self {
        let OperatorNode { op, children, src } = self;

        let children = children.map_visit(visit);

        OperatorNode { op, children, src }
    }
}

//======================================
// Missing closer nodes
//======================================

impl<I> GroupMissingCloserNode<I> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }
}

//======================================
// Operator sub-type nodes
//======================================

impl<I> PrefixNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixNodeCount);

        PrefixNode(OperatorNode::new(op, args))
    }
}

impl<I> BinaryNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_BinaryNodeCount);

        BinaryNode(OperatorNode::new(op, args))
    }
}

impl<I> InfixNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_InfixNodeCount);

        InfixNode(OperatorNode::new(op, args))
    }
}

impl<I> TernaryNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_TernaryNodeCount);

        TernaryNode(OperatorNode::new(op, args))
    }
}

impl<I> PostfixNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PostfixNodeCount);

        PostfixNode(OperatorNode::new(op, args))
    }
}

impl<I> PrefixBinaryNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixBinaryNodeCount);

        PrefixBinaryNode(OperatorNode::new(op, args))
    }
}

//======================================
// GroudNode and CompoundNode
//======================================

impl<I> GroupNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupNodeCount);

        GroupNode(OperatorNode::new(op, args))
    }
}

impl<I> CompoundNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_CompoundNodeCount);

        CompoundNode(OperatorNode::new(op, args))
    }
}

impl<I> GroupMissingCloserNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupMissingCloserNodeCount);

        GroupMissingCloserNode(OperatorNode::new(op, args))
    }
}

impl<I> UnterminatedGroupNeedsReparseNode<I> {
    pub(crate) fn new(op: Operator, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_UnterminatedGroupNeedsReparseNodeCount);

        UnterminatedGroupNeedsReparseNode(OperatorNode::new(op, args))
    }
}

//======================================
// CallNode
//======================================

impl<I> CallNode<I> {
    pub(crate) fn concrete(head: NodeSeq<I>, body: Node<I>) -> Self {
        debug_assert!(!head.is_empty());

        incr_diagnostic!(Node_CallNodeCount);

        let src = Source::new_from_source(head.first().source(), body.source());

        CallNode {
            head,
            body: Box::new(body),
            src,
            is_concrete: true,
        }
    }

    // pub(crate) fn group(head: NodeVariant<I>, group: GroupNode<I>) -> Self {
    //     CallNode::new(NodeSeq(vec![head]), NodeVariant::Node(Node::Group(group)))
    // }

    fn getSource(&self) -> Source {
        return self.src;
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

        return head.check() && body.check();
    }
}

//======================================
// SyntaxErrorNode
//======================================

impl<I> SyntaxErrorNode<I> {
    pub(crate) fn new(err: SyntaxErrorKind, children: NodeSeq<I>) -> Self {
        assert!(!children.is_empty());

        incr_diagnostic!(Node_SyntaxErrorNodeCount);

        let src = Source::new_from_source(children.first().source(), children.last().source());

        SyntaxErrorNode { err, children, src }
    }

    pub(crate) fn check(&self) -> bool {
        return false;
    }

    fn getSource(&self) -> Source {
        return self.src;
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
    pub(crate) fn from_str(string: &str) -> Option<Self> {
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
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            BoxKind::Tag => "Tag",
            BoxKind::Superscript => "Superscript",
            // NOTE: When adding a case here, also update from_str().
        }
    }

    pub(crate) fn from_str(string: &str) -> Option<Self> {
        let value = match string {
            "Tag" => BoxKind::Tag,
            "Superscript" => BoxKind::Superscript,
            _ => return None,
        };

        Some(value)
    }
}
