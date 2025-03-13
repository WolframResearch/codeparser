use std::collections::HashSet;

use crate::{
    source::{IssuePtrSet, Source, SourceLocation},
    symbol::Symbol,
    symbol_registration::*,
    token::{BorrowedTokenInput, OwnedTokenInput, Token, TokenRef},
    tokenizer::{Tokenizer, UnsafeCharacterEncoding},
};

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
pub struct NodeSeq<I = OwnedTokenInput>(pub Vec<Node<I>>);

/// An expression representing a node in the syntax tree
#[derive(Debug, Clone, PartialEq)]
pub enum Node<I = OwnedTokenInput> {
    Token(Token<I>),
    Call(CallNode<I>),
    SyntaxError(SyntaxErrorNode<I>),
    Prefix(PrefixNode<I>),
    Infix(InfixNode<I>),
    Postfix(PostfixNode<I>),
    Binary(BinaryNode<I>),
    Ternary(TernaryNode<I>),
    PrefixBinary(PrefixBinaryNode<I>),
    Compound(CompoundNode<I>),
    Group(GroupNode<I>),
    CollectedExpressions(CollectedExpressionsNode<I>),
    CollectedSourceLocations(CollectedSourceLocationsNode),
    CollectedIssues(CollectedIssuesNode),
    MissingBecauseUnsafeCharacterEncoding(MissingBecauseUnsafeCharacterEncodingNode),
    SafeString(SafeStringNode),
    GroupMissingCloser(GroupMissingCloserNode<I>),
    UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode<I>),
}

/// Any kind of prefix, postfix, binary, or infix operator
#[derive(Debug, Clone, PartialEq)]
pub struct OperatorNode<I = OwnedTokenInput> {
    pub(crate) op: Symbol,
    pub(crate) make_sym: Symbol,
    pub(crate) children: NodeSeq<I>,
    pub(crate) src: Source,
}

/// `-a`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixNode<I = OwnedTokenInput>(pub OperatorNode<I>);

/// `a @ b`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryNode<I = OwnedTokenInput>(pub OperatorNode<I>);

//
// InfixNode
//
// a + b + c
//
#[derive(Debug, Clone, PartialEq)]
pub struct InfixNode<I = OwnedTokenInput>(pub OperatorNode<I>);

/// `a /: b = c`
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryNode<I = OwnedTokenInput>(pub OperatorNode<I>);

/// `a!`
#[derive(Debug, Clone, PartialEq)]
pub struct PostfixNode<I = OwnedTokenInput>(pub OperatorNode<I>);

//
// PrefixBinaryNode
//
// \[Integral] f \[DifferentialD] x
//
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixBinaryNode<I = OwnedTokenInput>(pub OperatorNode<I>);

/// `f[x]`
#[derive(Debug, Clone, PartialEq)]
pub struct CallNode<I = OwnedTokenInput> {
    pub head: NodeSeq<I>,
    pub body: Box<Node<I>>,
    pub src: Source,
}

/// `{x}`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupNode<I = OwnedTokenInput>(pub OperatorNode<I>);

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
pub struct CompoundNode<I = OwnedTokenInput>(pub OperatorNode<I>);

/// A syntax error that contains structure.
#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxErrorNode<I = OwnedTokenInput> {
    pub err: Symbol,
    pub children: NodeSeq<I>,
    pub src: Source,
}

/// `{]`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingCloserNode<I = OwnedTokenInput>(pub OperatorNode<I>);

/// `{`
#[derive(Debug, Clone, PartialEq)]
pub struct UnterminatedGroupNeedsReparseNode<I = OwnedTokenInput>(pub OperatorNode<I>);

#[derive(Debug, Clone, PartialEq)]
pub struct CollectedExpressionsNode<I = OwnedTokenInput> {
    pub(crate) exprs: NodeSeq<I>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CollectedIssuesNode(pub IssuePtrSet);

#[derive(Debug, Clone, PartialEq)]
pub struct CollectedSourceLocationsNode {
    pub source_locs: HashSet<SourceLocation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MissingBecauseUnsafeCharacterEncodingNode {
    pub(crate) flag: UnsafeCharacterEncoding,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SafeStringNode {
    // pub bufAndLen: BufferAndLength,
    pub bufAndLen: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeContainer<I = OwnedTokenInput> {
    pub nodes: NodeSeq<I>,
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

    ($name:ident<> => Node::$variant:ident) => {
        impl<I> From<$name<I>> for Node<I> {
            fn from(node: $name<I>) -> Node<I> {
                Node::$variant(node)
            }
        }
    };
}

from_node!(CollectedExpressionsNode<> => Node::CollectedExpressions);
from_node!(CollectedSourceLocationsNode => Node::CollectedSourceLocations);
from_node!(CollectedIssuesNode => Node::CollectedIssues);
from_node!(MissingBecauseUnsafeCharacterEncodingNode => Node::MissingBecauseUnsafeCharacterEncoding);
from_node!(SafeStringNode => Node::SafeString);
from_node!(CompoundNode<> => Node::Compound);
from_node!(BinaryNode<> => Node::Binary);
from_node!(TernaryNode<> => Node::Ternary);
from_node!(SyntaxErrorNode<> => Node::SyntaxError);
from_node!(CallNode<> => Node::Call);
from_node!(InfixNode<> => Node::Infix);
from_node!(PrefixNode<> => Node::Prefix);
from_node!(PostfixNode<> => Node::Postfix);
from_node!(GroupNode<> => Node::Group);
from_node!(GroupMissingCloserNode<> => Node::GroupMissingCloser);
from_node!(UnterminatedGroupNeedsReparseNode<> => Node::UnterminatedGroupNeedsReparse);
from_node!(PrefixBinaryNode<> => Node::PrefixBinary);


//==========================================================
// Impls
//==========================================================

//======================================
// NodeSeq
//======================================

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

impl Node<BorrowedTokenInput<'_>> {
    pub fn into_owned_input(self) -> Node {
        match self {
            Node::Token(token) => Node::Token(token.into_owned_input()),
            Node::Call(CallNode { head, body, src }) => Node::Call(CallNode {
                head: head.into_owned_input(),
                body: Box::new(body.into_owned_input()),
                src,
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
            Node::Compound(CompoundNode(op)) => Node::Compound(CompoundNode(op.into_owned_input())),
            Node::Group(GroupNode(op)) => Node::Group(GroupNode(op.into_owned_input())),
            Node::PrefixBinary(PrefixBinaryNode(op)) => {
                Node::PrefixBinary(PrefixBinaryNode(op.into_owned_input()))
            },
            Node::CollectedExpressions(CollectedExpressionsNode { exprs }) => {
                Node::CollectedExpressions(CollectedExpressionsNode {
                    exprs: exprs.into_owned_input(),
                })
            },
            Node::CollectedSourceLocations(node) => Node::CollectedSourceLocations(node),
            Node::CollectedIssues(node) => Node::CollectedIssues(node),
            Node::MissingBecauseUnsafeCharacterEncoding(node) => {
                Node::MissingBecauseUnsafeCharacterEncoding(node)
            },
            Node::SafeString(node) => Node::SafeString(node),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Node::GroupMissingCloser(GroupMissingCloserNode(op.into_owned_input()))
            },
            Node::UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode(op)) => {
                Node::UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode(
                    op.into_owned_input(),
                ))
            },
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
            Node::CollectedExpressions(node) => node.getSource(),
            Node::CollectedSourceLocations(node) => node.getSource(),
            Node::CollectedIssues(node) => node.getSource(),
            Node::MissingBecauseUnsafeCharacterEncoding(node) => node.getSource(),
            Node::SafeString(node) => node.getSource(),
            Node::Prefix(PrefixNode(op)) => op.getSource(),
            Node::Infix(InfixNode(op)) => op.getSource(),
            Node::Postfix(PostfixNode(op)) => op.getSource(),
            Node::Binary(BinaryNode(op)) => op.getSource(),
            Node::PrefixBinary(PrefixBinaryNode(op)) => op.getSource(),
            Node::Ternary(TernaryNode(op)) => op.getSource(),
            Node::Compound(CompoundNode(op)) => op.getSource(),
            Node::Group(GroupNode(op)) => op.getSource(),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => op.getSource(),
            Node::UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode(op)) => {
                op.getSource()
            },
        }
    }

    // TODO(cleanup): Are these check() methods used anywhere? What do they even do?
    #[allow(dead_code)]
    fn check(&self) -> bool {
        match self {
            Node::Token(token) => token.check(),
            Node::Prefix(PrefixNode(op)) => op.check(),
            Node::Binary(BinaryNode(op)) => op.check(),
            Node::Infix(InfixNode(op)) => op.check(),
            Node::Ternary(TernaryNode(op)) => op.check(),
            Node::Postfix(PostfixNode(op)) => op.check(),
            Node::PrefixBinary(PrefixBinaryNode(op)) => op.check(),
            Node::Compound(CompoundNode(op)) => op.check(),
            Node::Group(GroupNode(op)) => op.check(),
            Node::CollectedExpressions(node) => node.check(),
            Node::CollectedSourceLocations(_) => true,
            Node::CollectedIssues(node) => node.check(),
            Node::MissingBecauseUnsafeCharacterEncoding(node) => node.check(),
            Node::SafeString(_) => true,
            Node::Call(node) => node.check(),
            Node::SyntaxError(node) => node.check(),
            Node::GroupMissingCloser(node) => node.check(),
            Node::UnterminatedGroupNeedsReparse(node) => node.check(),
        }
    }
}

//======================================
// OperatorNode
//======================================

impl<I> OperatorNode<I> {
    pub(crate) fn new(op: Symbol, make_sym: Symbol, children: NodeSeq<I>) -> Self {
        assert!(!children.is_empty());

        let src = Source::new_from_source(children.first().source(), children.last().source());

        OperatorNode {
            op,
            make_sym,
            children,
            src: src,
        }
    }

    pub fn getOp(&self) -> Symbol {
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
        let OperatorNode {
            op,
            make_sym,
            children,
            src,
        } = self;

        OperatorNode {
            op,
            make_sym,
            children: children.into_owned_input(),
            src,
        }
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

impl<I> UnterminatedGroupNeedsReparseNode<I> {
    pub(crate) fn check(&self) -> bool {
        return false;
    }
}

//======================================
// Operator sub-type nodes
//======================================

impl<I> PrefixNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixNodeCount);

        PrefixNode(OperatorNode::new(op, SYMBOL_CODEPARSER_PREFIXNODE, args))
    }
}

impl<I> BinaryNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_BinaryNodeCount);

        BinaryNode(OperatorNode::new(op, SYMBOL_CODEPARSER_BINARYNODE, args))
    }
}

impl<I> InfixNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_InfixNodeCount);

        InfixNode(OperatorNode::new(op, SYMBOL_CODEPARSER_INFIXNODE, args))
    }
}

impl<I> TernaryNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_TernaryNodeCount);

        TernaryNode(OperatorNode::new(op, SYMBOL_CODEPARSER_TERNARYNODE, args))
    }
}

impl<I> PostfixNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PostfixNodeCount);

        PostfixNode(OperatorNode::new(op, SYMBOL_CODEPARSER_POSTFIXNODE, args))
    }
}

impl<I> PrefixBinaryNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_PrefixBinaryNodeCount);

        PrefixBinaryNode(OperatorNode::new(
            op,
            SYMBOL_CODEPARSER_PREFIXBINARYNODE,
            args,
        ))
    }
}

//======================================
// GroudNode and CompoundNode
//======================================

impl<I> GroupNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupNodeCount);

        GroupNode(OperatorNode::new(op, SYMBOL_CODEPARSER_GROUPNODE, args))
    }
}

impl<I> CompoundNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_CompoundNodeCount);

        CompoundNode(OperatorNode::new(op, SYMBOL_CODEPARSER_COMPOUNDNODE, args))
    }
}

impl<I> GroupMissingCloserNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_GroupMissingCloserNodeCount);

        GroupMissingCloserNode(OperatorNode::new(
            op,
            SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE,
            args,
        ))
    }
}

impl<I> UnterminatedGroupNeedsReparseNode<I> {
    pub(crate) fn new(op: Symbol, args: NodeSeq<I>) -> Self {
        incr_diagnostic!(Node_UnterminatedGroupNeedsReparseNodeCount);

        UnterminatedGroupNeedsReparseNode(OperatorNode::new(
            op,
            SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE,
            args,
        ))
    }
}

//======================================
// CallNode
//======================================

impl<I> CallNode<I> {
    pub(crate) fn new(head: NodeSeq<I>, body: Node<I>) -> Self {
        debug_assert!(!head.is_empty());

        incr_diagnostic!(Node_CallNodeCount);

        let src = Source::new_from_source(head.first().source(), body.source());

        CallNode {
            head,
            body: Box::new(body),
            src,
        }
    }

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
        let CallNode { head, body, src: _ } = self;

        return head.check() && body.check();
    }
}

//======================================
// SyntaxErrorNode
//======================================

impl<I> SyntaxErrorNode<I> {
    pub(crate) fn new(err: Symbol, children: NodeSeq<I>) -> Self {
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
// CollectedExpressionsNode
//======================================

impl<I> CollectedExpressionsNode<I> {
    #[allow(dead_code)]
    pub(crate) fn new(exprs: NodeSeq<I>) -> Self {
        CollectedExpressionsNode { exprs }
    }

    // TODO: Display
    // fn CollectedExpressionsNode::print(std::ostream& s) const {
    //     Exprs.print(s);
    // }

    pub(crate) fn check(&self) -> bool {
        let CollectedExpressionsNode { exprs } = self;
        return exprs.check();
    }

    fn getSource(&self) -> Source {
        panic!("illegal access of getSource() on CollectedExpressionsNode");
    }
}

//======================================
// CollectedIssuesNode
//======================================

impl CollectedIssuesNode {
    pub fn CollectedIssuesNode(issues: IssuePtrSet) -> Self {
        CollectedIssuesNode(issues)
    }

    // TODO: Display
    // fn CollectedIssuesNode::print(std::ostream& s) const {

    //     SYMBOL_LIST.print(s);
    //     s << "[";

    //     for (auto& I : Issues) {
    //         I->print(s);
    //         s << ", ";
    //     }

    //     s << "]";
    // }

    pub(crate) fn check(&self) -> bool {
        let CollectedIssuesNode(issues) = self;

        for issue in issues {
            if !issue.check() {
                return false;
            }
        }

        return true;
    }

    fn getSource(&self) -> Source {
        panic!("illegal access of getSource() on CollectedIssuesNode");
    }
}

//======================================
// CollectedSourceLocationsNode
//======================================

impl CollectedSourceLocationsNode {
    #[allow(dead_code)]
    pub(crate) fn new(source_locs: HashSet<SourceLocation>) -> Self {
        CollectedSourceLocationsNode { source_locs }
    }

    fn getSource(&self) -> Source {
        panic!("illegal access of getSource() on CollectedSourceLocationsNode");
    }

    // TODO: Display
    // void CollectedSourceLocationsNode::print(std::ostream& s) const {

    //     SYMBOL_LIST.print(s);
    //     s << "[";

    //     for (auto& L : SourceLocs) {
    //         L.print(s);
    //         s << ", ";
    //     }

    //     s << "]";
    // }
}

#[cfg(feature = "USE_MATHLINK")]
pub(crate) fn unsafeCharacterEncodingReason(flag: UnsafeCharacterEncoding) -> crate::my_string::MyString {
    use crate::my_string_registration::*;

    match flag {
        UnsafeCharacterEncoding::IncompleteUTF8Sequence => {
            STRING_UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE
        },
        UnsafeCharacterEncoding::StraySurrogate => STRING_UNSAFECHARACTERENCODING_STRAYSURROGATE,
        UnsafeCharacterEncoding::BOM => STRING_UNSAFECHARACTERENCODING_BOM,
    }
}

//======================================
// MissingBecauseUnsafeCharacterEncodingNode
//======================================

impl MissingBecauseUnsafeCharacterEncodingNode {
    pub(crate) fn new(flag: UnsafeCharacterEncoding) -> Self {
        MissingBecauseUnsafeCharacterEncodingNode { flag }
    }

    fn getSource(&self) -> Source {
        panic!("illegal access of getSource() on MissingBecauseUnsafeCharacterEncodingNode");
    }

    pub(crate) fn check(&self) -> bool {
        return false;
    }

    // TODO: Display
    // void MissingBecauseUnsafeCharacterEncodingNode::print(std::ostream& s) const {

    //     auto reason = unsafeCharacterEncodingReason(flag);

    //     SYMBOL_MISSING.print(s);
    //     s << "[";

    //     reason.print(s);

    //     s << "]";
    // }
}

//======================================
// SafeStringNode
//======================================

impl SafeStringNode {
    pub(crate) fn new(bufAndLen: String) -> Self {
        SafeStringNode { bufAndLen }
    }

    fn getSource(&self) -> Source {
        panic!("illegal access of getSource() on SafeStringNode");
    }

    // TODO: Display
    // void SafeStringNode::print(std::ostream& s) const {
    //     bufAndLen.print(s);
    // }
}

impl<I> NodeContainer<I> {
    pub(crate) fn new(nodes: NodeSeq<I>) -> Self {
        NodeContainer { nodes }
    }

    // TODO: Display
    // void print(std::ostream& s) const {
    //     Nodes.print(s);
    // }

    #[allow(dead_code)]
    pub(crate) fn check(&self) -> bool {
        let NodeContainer { nodes } = self;
        return nodes.check();
    }
}
