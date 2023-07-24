//! Abstract syntax trees.

use crate::{
    cst::{BoxKind, CodeNode, GroupOperator, SyntaxErrorKind},
    issue::Issue,
    source::GeneralSource,
    tokenize::{OwnedTokenInput, TokenKind, TokenSource},
    Source,
};

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    /// `LeafNode[...]`
    Leaf {
        kind: TokenKind,
        input: OwnedTokenInput,
        data: AstMetadata,
    },
    /// `ErrorNode[..]`
    Error {
        kind: TokenKind,
        input: OwnedTokenInput,
        data: AstMetadata,
    },
    /// `CallNode[...]`
    Call {
        head: Box<AstNode>,
        args: Vec<AstNode>,
        data: AstMetadata,
    },
    /// `CallMissingCloserNode[...]`
    CallMissingCloser {
        head: Box<AstNode>,
        args: Vec<AstNode>,
        data: AstMetadata,
    },
    /// `UnterminatedCallNode[...]`
    UnterminatedCall {
        head: Box<AstNode>,
        args: Vec<AstNode>,
        data: AstMetadata,
    },
    /// `SyntaxErrorNode[...]`
    SyntaxError {
        kind: SyntaxErrorKind,
        children: Vec<AstNode>,
        data: AstMetadata,
    },
    /// `AbstractSyntaxErrorNode[..]`
    AbstractSyntaxError {
        kind: AbstractSyntaxError,
        args: Vec<AstNode>,
        data: AstMetadata,
    },
    Box {
        kind: BoxKind,
        args: Vec<AstNode>,
        data: AstMetadata,
    },
    /// `CodeNode[_, _, _]`
    Code {
        first: Expr,
        second: Expr,
        data: AstMetadata,
    },
    // Used in `BoxNode[_, {.., GroupNode[kind, {...}, data], ..}, _]`.
    #[allow(non_camel_case_types)]
    Group {
        kind: GroupOperator,
        children: Box<(
            AstNode, // Opener
            AstNode, // Body
            AstNode, // Closer
        )>,
        data: AstMetadata,
    },
    GroupMissingCloser {
        kind: GroupOperator,
        children: Vec<AstNode>,
        data: AstMetadata,
    },
    GroupMissingOpener {
        kind: GroupOperator,
        children: Vec<AstNode>,
        data: AstMetadata,
    },
    // TODO: Store these in abstracted form?
    #[allow(non_camel_case_types)]
    TagBox_GroupParen {
        group: Box<(AstNode, AstNode, AstNode, GeneralSource)>,
        tag: CodeNode<GeneralSource>,
        data: AstMetadata,
    },
    // FIXME: Handle linear syntax
    /// `PrefixNode[PrefixLinearSyntaxBang, {operator_, operand_}, data_]`
    #[allow(non_camel_case_types)]
    PrefixNode_PrefixLinearSyntaxBang(Box<[AstNode; 2]>, AstMetadata),
}

// TODO(cleanup): Combine this with `Metadata`?
#[derive(Debug, Clone, PartialEq)]
pub struct AstMetadata {
    pub source: GeneralSource,
    pub issues: Vec<Issue>,
}

// TODO(cleanup): Add `Kind` suffix to this name? Or remove `Kind` suffix from
//                other enums like this?
#[derive(Debug, Clone, PartialEq)]
pub enum AbstractSyntaxError {
    CommaTopLevel,
    OpenParen,
    OpenSquare,
    ColonColonOpenSquare,
    LeftDoubleBracket,
    NonAssociativePatternTest,
    LinearSyntaxBang,
    ExpectedTilde,
}

//======================================

pub(crate) struct AstCall {
    pub head: Box<AstNode>,
    pub args: Vec<AstNode>,
    pub data: GeneralSource,
}

//======================================
// Impls
//======================================

impl AstNode {
    pub(crate) fn into_children_and_source(self) -> (Vec<AstNode>, GeneralSource) {
        match self {
            AstNode::Leaf { .. } | AstNode::Error { .. } => panic!(
                "AstNode::into_children_and_source(): AstNode variant has no children: {self:?}"
            ),
            AstNode::Call {
                head: _,
                args,
                data,
            } => (args, data.source),
            AstNode::CallMissingCloser {
                head: _,
                args,
                data,
            } => (args, data.source),
            AstNode::UnterminatedCall {
                head: _,
                args,
                data,
            } => (args, data.source),
            AstNode::SyntaxError {
                kind: _,
                children,
                data,
            } => (children, data.source),
            AstNode::AbstractSyntaxError {
                kind: _,
                args,
                data,
            } => (args, data.source),
            AstNode::Box {
                kind: _,
                args,
                data,
            } => (args, data.source),
            AstNode::Code { .. } => todo!(),
            AstNode::Group { .. } => todo!(),
            AstNode::GroupMissingCloser { .. } => todo!(),
            AstNode::GroupMissingOpener { .. } => todo!(),
            AstNode::TagBox_GroupParen { .. } => todo!(),
            AstNode::PrefixNode_PrefixLinearSyntaxBang(children, data) => {
                (Vec::from(*children), data.source)
            },
        }
    }

    pub fn source(&self) -> Source {
        let general_source = &self.metadata().source;

        match general_source {
            GeneralSource::String(source) => *source,
            GeneralSource::BoxPosition(_) | GeneralSource::After(_) => {
                todo!("non-typical source: {general_source:?}")
            },
        }
    }

    pub(crate) fn metadata(&self) -> &AstMetadata {
        match self {
            AstNode::Leaf { data, .. } | AstNode::Error { data, .. } => data,
            AstNode::Call { data, .. } => data,
            AstNode::CallMissingCloser { data, .. } => data,
            AstNode::UnterminatedCall { data, .. } => data,
            AstNode::SyntaxError { data, .. } => data,
            AstNode::AbstractSyntaxError { data, .. } => data,
            AstNode::Box { data, .. } => data,
            AstNode::Code { data, .. } => data,
            AstNode::Group { data, .. } => data,
            AstNode::GroupMissingCloser { data, .. } => data,
            AstNode::GroupMissingOpener { data, .. } => data,
            AstNode::TagBox_GroupParen { data, .. } => data,
            AstNode::PrefixNode_PrefixLinearSyntaxBang(_, data) => data,
        }
    }
}

impl AstMetadata {
    pub fn from_src<S: TokenSource>(src: S) -> Self {
        let src = src.into_general();

        AstMetadata {
            source: src,
            issues: Vec::new(),
        }
    }

    /// `<||>`
    pub fn empty() -> Self {
        AstMetadata {
            source: GeneralSource::unknown(),
            issues: Vec::new(),
        }
    }
}

//======================================
// Conversion Impls
//======================================

impl From<AstCall> for AstNode {
    fn from(call: AstCall) -> Self {
        let AstCall { head, args, data } = call;

        AstNode::Call {
            head,
            args,
            data: AstMetadata::from_src(data),
        }
    }
}

impl<S: TokenSource> From<S> for AstMetadata {
    fn from(source: S) -> Self {
        AstMetadata {
            source: source.into_general(),
            issues: Vec::new(),
        }
    }
}

impl AbstractSyntaxError {
    pub fn as_str(&self) -> &'static str {
        match self {
            AbstractSyntaxError::CommaTopLevel => "CommaTopLevel",
            AbstractSyntaxError::OpenParen => "OpenParen",
            AbstractSyntaxError::OpenSquare => "OpenSquare",
            AbstractSyntaxError::ColonColonOpenSquare => "ColonColonOpenSquare",
            AbstractSyntaxError::LeftDoubleBracket => "LeftDoubleBracket",
            AbstractSyntaxError::NonAssociativePatternTest => "NonAssociativePatternTest",
            AbstractSyntaxError::LinearSyntaxBang => "LinearSyntaxBang",
            AbstractSyntaxError::ExpectedTilde => "ExpectedTilde",
            // NOTE: When adding a case here, also update from_str().
        }
    }
}

//======================================
// Macros
//======================================

/// Transforms Wolfram Language syntax for representing nodes into Rust code to
/// construct the equivalent [`AstNode`].
macro_rules! WL {
    //========================
    // ToNode[..]
    //========================

    (ToNode[$sym:ident]) => {{
        let sym: $crate::symbol::Symbol = $crate::symbol_registration::$sym;

        $crate::abstract_::ToNode_Symbol(sym)
    }};

    //========================
    // LeafNode
    //========================

    (LeafNode[$token_kind:ident, $input:expr, <||>]) => {{
        let input: String = String::from($input);

        let node = $crate::ast::AstNode::Leaf {
            kind: $crate::tokenize::TokenKind::$token_kind,
            input: $crate::tokenize::OwnedTokenInput {
                buf: input.into_bytes(),
            },
            data: AstMetadata::empty(),
        };

        node
    }};

    (LeafNode[$token_kind:ident, $input:expr, $data:expr]) => {{
        let input: String = String::from($input);
        let src: $crate::source::GeneralSource = $data.into_general();

        let node = $crate::ast::AstNode::Leaf {
            kind: $crate::tokenize::TokenKind::$token_kind,
            input: $crate::tokenize::OwnedTokenInput {
                buf: input.into_bytes(),
            },
            data: AstMetadata::from_src(src),
        };

        node
    }};

    //========================
    // CallNode
    //========================

    (CallNode[ToNode[$token_kind:ident], { $($args:expr),* }, <||>]) => {
        WL!( CallNode[ToNode[$token_kind], { $($args),* }, S::unknown()])
    };
    (CallNode[ToNode[$token_kind:ident], { $($args:expr),* }, $data:expr]) => {{
        $crate::ast::AstNode::Call {
            head: Box::new($crate::ast::WL!(ToNode[$token_kind])),
            args: vec![$($args),*],
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    (CallNode[ToNode[$token_kind:ident], $args:expr, <||>]) => {{
        $crate::ast::AstNode::Call {
            head: Box::new($crate::ast::WL!(ToNode[$token_kind])),
            args: $args,
            data: $crate::ast::AstMetadata::empty(),
        }
    }};
    (CallNode[ToNode[$token_kind:ident], $args:expr, $data:expr]) => {{
        $crate::ast::AstNode::Call {
            head: Box::new($crate::ast::WL!(ToNode[$token_kind])),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    (CallNode[$head:expr, { $($args:expr),* }, <||>]) => {{
        WL!( CallNode[$head, { $($args),* }, S::unknown()] )
    }};
    (CallNode[$head:expr, { $($args:expr),* }, $data:expr]) => {{
        $crate::ast::AstNode::Call {
            head: Box::new($head),
            args: vec![$($args),*],
            data: $crate::ast::AstMetadata::from($data),
        }
    }};
    (CallNode[$head:expr, $args:expr, $data:expr]) => {{
        $crate::ast::AstNode::Call {
            head: Box::new($head),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    //========================
    // CallMissingCloserNode
    //========================

    (CallMissingCloserNode[$head:expr, $args:expr, $data:expr]) => {{
        $crate::ast::AstNode::CallMissingCloser {
            head: Box::new($head),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    //========================
    // UnterminatedCallNode
    //========================

    (UnterminatedCallNode[$head:expr, $args:expr, $data:expr]) => {{
        $crate::ast::AstNode::UnterminatedCall {
            head: Box::new($head),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    //========================
    // SyntaxErrorNode
    //========================

    (SyntaxErrorNode[$err_kind:ident, { $($args:expr),* }, $data:expr]) => {
        $crate::ast::AstNode::SyntaxError {
            kind: $crate::cst::SyntaxErrorKind::$err_kind,
            children: vec![$($args),*],
            data: $crate::ast::AstMetadata::from_src($data),
        }
    };

    //========================
    // AbstractSyntaxErrorNode
    //========================

    (AbstractSyntaxErrorNode[$err_kind:ident, { $($args:expr),* }, $data:expr]) => {
        WL!( AbstractSyntaxErrorNode[$err_kind, vec![$($args),*], $data] )
    };
    (AbstractSyntaxErrorNode[$err_kind:ident, $args:expr, $data:expr]) => {
        $crate::ast::AstNode::AbstractSyntaxError {
            kind: $crate::ast::AbstractSyntaxError::$err_kind,
            args: $args,
            data: $crate::ast::AstMetadata::from_src($data),
        }
    };

    //========================
    // BoxNode
    //========================

    // (BoxNode[TagBox, {$content:expr, $tag:expr}, $data:expr]) => {
    //     $crate::ast::AstNode::TagBox {
    //         content: $content,
    //         tag: $tag,
    //         data: $crate::ast::AstMetadata::from_src($data),
    //     }
    // };
    (BoxNode[$kind:ident, $children:expr, $data:expr]) => {
        $crate::ast::AstNode::Box {
            kind: BoxKind::$kind,
            args: $children,
            data: $crate::ast::AstMetadata::from_src($data),
        }
    };
    (BoxNode[$kind:expr, $children:expr, $data:expr]) => {
        $crate::ast::AstNode::Box {
            kind: $kind,
            args: $children,
            data: $crate::ast::AstMetadata::from_src($data),
        }
    };
}

use wolfram_expr::Expr;
pub(crate) use WL;
