use std::collections::HashSet;

use crate::{
    source::{IssuePtrSet, Source, SourceLocation},
    symbol::Symbol,
    symbol_registration::*,
    token::Token,
    tokenizer::{Tokenizer, UnsafeCharacterEncoding},
};

//
// Used mainly for collecting trivia that has been eaten
//
#[derive(Debug)]
pub struct TriviaSeq {
    pub vec: Vec<Token>,
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
pub struct NodeSeq {
    pub vec: Vec<Node>,
}

/// An expression representing a node in the syntax tree
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Token(Token),
    Call(CallNode),
    SyntaxError(SyntaxErrorNode),
    Prefix(PrefixNode),
    Infix(InfixNode),
    Postfix(PostfixNode),
    Binary(BinaryNode),
    Ternary(TernaryNode),
    CollectedExpressions(CollectedExpressionsNode),
    CollectedSourceLocations(CollectedSourceLocationsNode),
    CollectedIssues(CollectedIssuesNode),
    MissingBecauseUnsafeCharacterEncoding(MissingBecauseUnsafeCharacterEncodingNode),
    SafeString(SafeStringNode),
    Compound(CompoundNode),
    Group(GroupNode),
    GroupMissingCloser(GroupMissingCloserNode),
    UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode),
    PrefixBinary(PrefixBinaryNode),
}

/// Any kind of prefix, postfix, binary, or infix operator
#[derive(Debug, Clone, PartialEq)]
pub struct OperatorNode {
    pub(crate) Op: Symbol,
    pub(crate) MakeSym: Symbol,
    pub(crate) Children: NodeSeq,
    pub(crate) Src: Source,
}

/// `-a`
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixNode {
    pub op: OperatorNode,
}

/// `a @ b`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryNode {
    pub op: OperatorNode,
}

//
// InfixNode
//
// a + b + c
//
#[derive(Debug, Clone, PartialEq)]
pub struct InfixNode {
    pub op: OperatorNode,
}

/// `a /: b = c`
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryNode {
    pub op: OperatorNode,
}

/// `a!`
#[derive(Debug, Clone, PartialEq)]
pub struct PostfixNode {
    pub op: OperatorNode,
}

//
// PrefixBinaryNode
//
// \[Integral] f \[DifferentialD] x
//
#[derive(Debug, Clone, PartialEq)]
pub struct PrefixBinaryNode {
    pub op: OperatorNode,
}

/// `f[x]`
#[derive(Debug, Clone, PartialEq)]
pub struct CallNode {
    pub head: NodeSeq,
    pub body: Box<Node>,
    pub src: Source,
}

/// `{x}`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupNode {
    pub op: OperatorNode,
}

/// Any "compound" of tokens:
///
/// a_
/// _b
/// a_.
/// #a
/// #abc
/// ##2
/// %2
#[derive(Debug, Clone, PartialEq)]
pub struct CompoundNode {
    pub op: OperatorNode,
}

/// A syntax error that contains structure.
#[derive(Debug, Clone, PartialEq)]
pub struct SyntaxErrorNode {
    pub err: Symbol,
    pub children: NodeSeq,
    pub src: Source,
}

/// `{]`
#[derive(Debug, Clone, PartialEq)]
pub struct GroupMissingCloserNode {
    pub op: OperatorNode,
}

/// `{`
#[derive(Debug, Clone, PartialEq)]
pub struct UnterminatedGroupNeedsReparseNode {
    pub op: OperatorNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CollectedExpressionsNode {
    pub(crate) exprs: NodeSeq,
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
pub struct NodeContainer {
    pub(crate) nodes: NodeSeq,
}

//======================================
// Node convertions
//======================================

macro_rules! from_node {
    ($name:ident => Node::$variant:ident) => {
        impl From<$name> for Node {
            fn from(node: $name) -> Node {
                Node::$variant(node)
            }
        }
    };
}

from_node!(CollectedExpressionsNode => Node::CollectedExpressions);
from_node!(CollectedSourceLocationsNode => Node::CollectedSourceLocations);
from_node!(CollectedIssuesNode => Node::CollectedIssues);
from_node!(MissingBecauseUnsafeCharacterEncodingNode => Node::MissingBecauseUnsafeCharacterEncoding);
from_node!(SafeStringNode => Node::SafeString);
from_node!(CompoundNode => Node::Compound);
from_node!(BinaryNode => Node::Binary);
from_node!(TernaryNode => Node::Ternary);
from_node!(SyntaxErrorNode => Node::SyntaxError);
from_node!(CallNode => Node::Call);
from_node!(InfixNode => Node::Infix);
from_node!(PrefixNode => Node::Prefix);
from_node!(PostfixNode => Node::Postfix);
from_node!(GroupNode => Node::Group);
from_node!(GroupMissingCloserNode => Node::GroupMissingCloser);
from_node!(UnterminatedGroupNeedsReparseNode => Node::UnterminatedGroupNeedsReparse);
from_node!(PrefixBinaryNode => Node::PrefixBinary);

impl From<Token> for Node {
    fn from(token: Token) -> Self {
        Node::Token(token)
    }
}

//==========================================================
// Impls
//==========================================================

//======================================
// NodeSeq
//======================================

impl NodeSeq {
    pub(crate) fn new() -> NodeSeq {
        NodeSeq { vec: Vec::new() }
    }

    pub fn push<N: Into<Node>>(&mut self, node: N) {
        let node = node.into();
        self.vec.push(node);
    }

    pub fn clear(&mut self) {
        let NodeSeq { vec } = self;

        vec.clear();
    }

    pub fn is_empty(&self) -> bool {
        return self.vec.is_empty();
    }

    pub fn len(&self) -> usize {
        return self.vec.len();
    }

    // PRE_COMMIT: impl Index?
    // const NodeVariant& NodeSeq::operator[](size_t index) const {
    //     return vec[index];
    // }

    fn first(&self) -> &Node {
        self.vec.first().expect("NodeSeq::first(): vector is empty")
    }

    fn last(&self) -> &Node {
        self.vec.last().expect("NodeSeq::last(): vector is empty")
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

    pub fn check(&self) -> bool {
        let NodeSeq { vec } = self;

        for elem in vec {
            if !elem.check() {
                return false;
            }
        }

        return true;
    }
}

//======================================
// TriviaSeq
//======================================

impl TriviaSeq {
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

        session.offset = T.span.offset;
        session.SrcLoc = T.src.start;

        vec.clear();
    }

    pub fn push(&mut self, token: Token) {
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

impl Node {
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
            Node::Prefix(PrefixNode { op }) => op.getSource(),
            Node::Infix(InfixNode { op }) => op.getSource(),
            Node::Postfix(PostfixNode { op }) => op.getSource(),
            Node::Binary(BinaryNode { op }) => op.getSource(),
            Node::PrefixBinary(PrefixBinaryNode { op }) => op.getSource(),
            Node::Ternary(TernaryNode { op }) => op.getSource(),
            Node::Compound(CompoundNode { op }) => op.getSource(),
            Node::Group(GroupNode { op }) => op.getSource(),
            Node::GroupMissingCloser(GroupMissingCloserNode { op }) => op.getSource(),
            Node::UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode { op }) => {
                op.getSource()
            },
        }
    }
    fn check(&self) -> bool {
        match self {
            Node::Token(token) => token.check(),
            Node::Prefix(PrefixNode { op }) => op.check(),
            Node::Binary(BinaryNode { op }) => op.check(),
            Node::Infix(InfixNode { op }) => op.check(),
            Node::Ternary(TernaryNode { op }) => op.check(),
            Node::Postfix(PostfixNode { op }) => op.check(),
            Node::PrefixBinary(PrefixBinaryNode { op }) => op.check(),
            Node::Compound(CompoundNode { op }) => op.check(),
            Node::Group(GroupNode { op }) => op.check(),
            Node::CollectedExpressions(node) => node.check(),
            Node::CollectedSourceLocations(_) => true,
            Node::CollectedIssues(node) => node.check(),
            Node::MissingBecauseUnsafeCharacterEncoding(node) => node.check(),
            Node::SafeString(_) => true,
            Node::Call(node) => node.check(),
            Node::SyntaxError(_) => todo!(),
            Node::GroupMissingCloser(_) => todo!(),
            Node::UnterminatedGroupNeedsReparse(_) => todo!(),
        }
    }
}

//======================================
// OperatorNode
//======================================

impl OperatorNode {
    pub(crate) fn new(Op: Symbol, MakeSym: Symbol, Children: NodeSeq) -> Self {
        assert!(!Children.is_empty());

        let src = Source::new_from_source(Children.first().source(), Children.last().source());

        OperatorNode {
            Op,
            MakeSym,
            Children,
            Src: src,
        }
    }

    pub fn getOp(&self) -> Symbol {
        return self.Op;
    }

    pub fn getSource(&self) -> Source {
        return self.Src;
    }

    pub fn check(&self) -> bool {
        return self.Children.check();
    }

    // TODO: impl Display
    // void print(std::ostream& s) const {

    //     MakeSym.print(s);
    //     s << "[";

    //     Op.print(s);
    //     s << ", ";

    //     Children.print(s);
    //     s << ", ";

    //     Src.print(s);

    //     s << "]";
    // }
}

//======================================
// Missing closer nodes
//======================================

impl GroupMissingCloserNode {
    pub fn check(&self) -> bool {
        return false;
    }
}

impl UnterminatedGroupNeedsReparseNode {
    pub fn check(&self) -> bool {
        return false;
    }
}

//======================================
// Operator sub-type nodes
//======================================

impl PrefixNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_PrefixNodeCount);

        PrefixNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_PREFIXNODE, Args),
        }
    }
}

impl BinaryNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_BinaryNodeCount);

        BinaryNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_BINARYNODE, Args),
        }
    }
}

impl InfixNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_InfixNodeCount);

        InfixNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_INFIXNODE, Args),
        }
    }
}

impl TernaryNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_TernaryNodeCount);

        TernaryNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_TERNARYNODE, Args),
        }
    }
}

impl PostfixNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_PostfixNodeCount);

        PostfixNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_POSTFIXNODE, Args),
        }
    }
}

impl PrefixBinaryNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_PrefixBinaryNodeCount);

        PrefixBinaryNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_PREFIXBINARYNODE, Args),
        }
    }
}

//======================================
// GroudNode and CompoundNode
//======================================

impl GroupNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_GroupNodeCount);

        GroupNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_GROUPNODE, Args),
        }
    }
}

impl CompoundNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_CompoundNodeCount);

        CompoundNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_COMPOUNDNODE, Args),
        }
    }
}

impl GroupMissingCloserNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_GroupMissingCloserNodeCount);

        GroupMissingCloserNode {
            op: OperatorNode::new(Op, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE, Args),
        }
    }
}

impl UnterminatedGroupNeedsReparseNode {
    pub(crate) fn new(Op: Symbol, Args: NodeSeq) -> Self {
        incr_diagnostic!(Node_UnterminatedGroupNeedsReparseNodeCount);

        UnterminatedGroupNeedsReparseNode {
            op: OperatorNode::new(
                Op,
                SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE,
                Args,
            ),
        }
    }
}

//======================================
// CallNode
//======================================

impl CallNode {
    pub(crate) fn new(head: NodeSeq, body: Node) -> Self {
        assert!(!head.is_empty());

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

    //     Src.print(s);

    //     s << "]";
    // }

    pub fn check(&self) -> bool {
        let CallNode { head, body, src: _ } = self;

        return head.check() && body.check();
    }
}

//======================================
// SyntaxErrorNode
//======================================

impl SyntaxErrorNode {
    pub(crate) fn new(err: Symbol, children: NodeSeq) -> Self {
        assert!(!children.is_empty());

        incr_diagnostic!(Node_SyntaxErrorNodeCount);

        let src = Source::new_from_source(children.first().source(), children.last().source());

        SyntaxErrorNode { err, children, src }
    }

    pub fn check(&self) -> bool {
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

    //     Children.print(s);
    //     s << ", ";

    //     Src.print(s);

    //     s << "]";
    // }
}

//======================================
// CollectedExpressionsNode
//======================================

impl CollectedExpressionsNode {
    pub(crate) fn new(exprs: NodeSeq) -> Self {
        CollectedExpressionsNode { exprs }
    }

    // TODO: Display
    // fn CollectedExpressionsNode::print(std::ostream& s) const {
    //     Exprs.print(s);
    // }

    pub fn check(&self) -> bool {
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

    pub fn check(&self) -> bool {
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
        UnsafeCharacterEncoding::Ok => {
            panic!("unable to generate unsafe character encoding reason for OK case");
        },
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

    pub fn check(&self) -> bool {
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

impl NodeContainer {
    pub(crate) fn new(nodes: NodeSeq) -> Self {
        NodeContainer { nodes }
    }

    // TODO: Display
    // void print(std::ostream& s) const {
    //     Nodes.print(s);
    // }

    pub fn check(&self) -> bool {
        let NodeContainer { nodes } = self;
        return nodes.check();
    }
}
