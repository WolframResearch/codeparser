//! Abstract syntax trees.

use std::fmt::Debug;

use crate::{
    cst::{BoxKind, CodeNode, GroupOperator, SyntaxErrorKind},
    issue::Issue,
    source::{LineColumnSpan, Source, Span},
    tokenize::{TokenKind, TokenSource, TokenString},
};

/// An abstract syntax tree (AST) node.
#[derive(Clone, PartialEq)]
pub enum Ast {
    /// `LeafNode[...]`
    Leaf {
        kind: TokenKind,
        input: TokenString,
        data: AstMetadata,
    },
    /// `ErrorNode[..]`
    Error {
        kind: TokenKind,
        input: TokenString,
        data: AstMetadata,
    },
    /// `CallNode[...]`
    Call {
        head: Box<Ast>,
        args: Vec<Ast>,
        data: AstMetadata,
    },
    /// `CallMissingCloserNode[...]`
    CallMissingCloser {
        head: Box<Ast>,
        args: Vec<Ast>,
        data: AstMetadata,
    },
    /// `UnterminatedCallNode[...]`
    UnterminatedCall {
        head: Box<Ast>,
        args: Vec<Ast>,
        data: AstMetadata,
    },
    /// `SyntaxErrorNode[...]`
    SyntaxError {
        kind: SyntaxErrorKind,
        children: Vec<Ast>,
        data: AstMetadata,
    },
    /// `AbstractSyntaxErrorNode[..]`
    AbstractSyntaxError {
        kind: AbstractSyntaxError,
        args: Vec<Ast>,
        data: AstMetadata,
    },
    Box {
        kind: BoxKind,
        args: Vec<Ast>,
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
            Ast, // Opener
            Ast, // Body
            Ast, // Closer
        )>,
        data: AstMetadata,
    },
    GroupMissingCloser {
        kind: GroupOperator,
        children: Vec<Ast>,
        data: AstMetadata,
    },
    GroupMissingOpener {
        kind: GroupOperator,
        children: Vec<Ast>,
        data: AstMetadata,
    },
    // TODO: Store these in abstracted form?
    #[allow(non_camel_case_types)]
    TagBox_GroupParen {
        group: Box<(Ast, Ast, Ast, Source)>,
        tag: CodeNode<Source>,
        data: AstMetadata,
    },
    // FIXME: Handle linear syntax
    /// `PrefixNode[PrefixLinearSyntaxBang, {operator_, operand_}, data_]`
    #[allow(non_camel_case_types)]
    PrefixNode_PrefixLinearSyntaxBang(Box<[Ast; 2]>, AstMetadata),
}

// TODO(cleanup): Combine this with `Metadata`?
#[derive(Clone, PartialEq)]
pub struct AstMetadata {
    pub source: Source,
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
    pub head: Box<Ast>,
    pub args: Vec<Ast>,
    pub data: Source,
}

//======================================
// Impls
//======================================

impl Ast {
    pub(crate) fn into_children_and_source(self) -> (Vec<Ast>, Source) {
        match self {
            Ast::Leaf { .. } | Ast::Error { .. } => panic!(
                "Ast::into_children_and_source(): Ast variant has no children: {self:?}"
            ),
            Ast::Call {
                head: _,
                args,
                data,
            } => (args, data.source),
            Ast::CallMissingCloser {
                head: _,
                args,
                data,
            } => (args, data.source),
            Ast::UnterminatedCall {
                head: _,
                args,
                data,
            } => (args, data.source),
            Ast::SyntaxError {
                kind: _,
                children,
                data,
            } => (children, data.source),
            Ast::AbstractSyntaxError {
                kind: _,
                args,
                data,
            } => (args, data.source),
            Ast::Box {
                kind: _,
                args,
                data,
            } => (args, data.source),
            Ast::Code { .. } => todo!(),
            Ast::Group { .. } => todo!(),
            Ast::GroupMissingCloser { .. } => todo!(),
            Ast::GroupMissingOpener { .. } => todo!(),
            Ast::TagBox_GroupParen { .. } => todo!(),
            Ast::PrefixNode_PrefixLinearSyntaxBang(children, data) => {
                (Vec::from(*children), data.source)
            },
        }
    }

    // TODO(cleanup): Document panic, add separate source() method.
    pub fn span(&self) -> Span {
        let general_source = &self.metadata().source;

        match general_source {
            Source::Span(span) => *span,
            Source::BoxPosition(_) | Source::After(_) | Source::Unknown => {
                todo!("non-typical source: {general_source:?}")
            },
        }
    }

    pub(crate) fn metadata(&self) -> &AstMetadata {
        match self {
            Ast::Leaf { data, .. } | Ast::Error { data, .. } => data,
            Ast::Call { data, .. } => data,
            Ast::CallMissingCloser { data, .. } => data,
            Ast::UnterminatedCall { data, .. } => data,
            Ast::SyntaxError { data, .. } => data,
            Ast::AbstractSyntaxError { data, .. } => data,
            Ast::Box { data, .. } => data,
            Ast::Code { data, .. } => data,
            Ast::Group { data, .. } => data,
            Ast::GroupMissingCloser { data, .. } => data,
            Ast::GroupMissingOpener { data, .. } => data,
            Ast::TagBox_GroupParen { data, .. } => data,
            Ast::PrefixNode_PrefixLinearSyntaxBang(_, data) => data,
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
            source: Source::unknown(),
            issues: Vec::new(),
        }
    }
}

//======================================
// Conversion Impls
//======================================

impl From<AstCall> for Ast {
    fn from(call: AstCall) -> Self {
        let AstCall { head, args, data } = call;

        Ast::Call {
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

impl From<LineColumnSpan> for AstMetadata {
    fn from(value: LineColumnSpan) -> Self {
        AstMetadata {
            source: Source::Span(Span::from(value)),
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
            AbstractSyntaxError::NonAssociativePatternTest => {
                "NonAssociativePatternTest"
            },
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
/// construct the equivalent [`Ast`].
macro_rules! WL {
    //========================
    // ToNode[..]
    //========================

    (ToNode[$sym:ident]) => {{
        let sym: $crate::symbol::Symbol = $crate::symbols::$sym;

        $crate::abstract_cst::ToNode_Symbol(sym)
    }};

    //========================
    // LeafNode
    //========================

    (LeafNode[$token_kind:ident, $input:expr, <||>]) => {{
        let input: String = String::from($input);

        let node = $crate::ast::Ast::Leaf {
            kind: $crate::tokenize::TokenKind::$token_kind,
            input: $crate::tokenize::TokenString {
                buf: input.into_bytes(),
            },
            data: AstMetadata::empty(),
        };

        node
    }};

    (LeafNode[$token_kind:ident, $input:expr, $data:expr]) => {{
        let input: String = String::from($input);
        let src: $crate::source::Source = $data.into_general();

        let node = $crate::ast::Ast::Leaf {
            kind: $crate::tokenize::TokenKind::$token_kind,
            input: $crate::tokenize::TokenString {
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
        WL!( CallNode[ToNode[$token_kind], { $($args),* }, $crate::ast::AstMetadata::empty() ])
    };
    (CallNode[ToNode[$token_kind:ident], { $($args:expr),* }, $data:expr]) => {{
        $crate::ast::Ast::Call {
            head: Box::new($crate::ast::WL!(ToNode[$token_kind])),
            args: vec![$($args),*],
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    (CallNode[ToNode[$token_kind:ident], $args:expr, <||>]) => {{
        $crate::ast::Ast::Call {
            head: Box::new($crate::ast::WL!(ToNode[$token_kind])),
            args: $args,
            data: $crate::ast::AstMetadata::empty(),
        }
    }};
    (CallNode[ToNode[$token_kind:ident], $args:expr, $data:expr]) => {{
        $crate::ast::Ast::Call {
            head: Box::new($crate::ast::WL!(ToNode[$token_kind])),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    (CallNode[$head:expr, { $($args:expr),* }, <||>]) => {{
        WL!( CallNode[$head, { $($args),* }, $crate::ast::AstMetadata::empty()] )
    }};
    (CallNode[$head:expr, { $($args:expr),* }, $data:expr]) => {{
        $crate::ast::Ast::Call {
            head: Box::new($head),
            args: vec![$($args),*],
            data: $crate::ast::AstMetadata::from($data),
        }
    }};
    (CallNode[$head:expr, $args:expr, $data:expr]) => {{
        $crate::ast::Ast::Call {
            head: Box::new($head),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    //========================
    // CallMissingCloserNode
    //========================

    (CallMissingCloserNode[$head:expr, $args:expr, $data:expr]) => {{
        $crate::ast::Ast::CallMissingCloser {
            head: Box::new($head),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    //========================
    // UnterminatedCallNode
    //========================

    (UnterminatedCallNode[$head:expr, $args:expr, $data:expr]) => {{
        $crate::ast::Ast::UnterminatedCall {
            head: Box::new($head),
            args: $args,
            data: $crate::ast::AstMetadata::from($data),
        }
    }};

    //========================
    // SyntaxErrorNode
    //========================

    (SyntaxErrorNode[$err_kind:ident, { $($args:expr),* }, $data:expr]) => {
        $crate::ast::Ast::SyntaxError {
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
        $crate::ast::Ast::AbstractSyntaxError {
            kind: $crate::ast::AbstractSyntaxError::$err_kind,
            args: $args,
            data: $crate::ast::AstMetadata::from_src($data),
        }
    };

    //========================
    // BoxNode
    //========================

    // (BoxNode[TagBox, {$content:expr, $tag:expr}, $data:expr]) => {
    //     $crate::ast::Ast::TagBox {
    //         content: $content,
    //         tag: $tag,
    //         data: $crate::ast::AstMetadata::from_src($data),
    //     }
    // };
    (BoxNode[$kind:ident, $children:expr, $data:expr]) => {
        $crate::ast::Ast::Box {
            kind: BoxKind::$kind,
            args: $children,
            data: $crate::ast::AstMetadata::from_src($data),
        }
    };
    (BoxNode[$kind:expr, $children:expr, $data:expr]) => {
        $crate::ast::Ast::Box {
            kind: $kind,
            args: $children,
            data: $crate::ast::AstMetadata::from_src($data),
        }
    };
}

use wolfram_expr::Expr;
pub(crate) use WL;

//======================================
// Format Impls
//======================================

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Leaf { kind, input, data } => {
                // Format this as the `leaf!(..)` invocation to construct this Leaf.
                write!(f, "leaf!({kind:?}, {input}, {data:?})")
            },
            Self::Error { kind, input, data } => f
                .debug_struct("Error")
                .field("kind", kind)
                .field("input", input)
                .field("data", data)
                .finish(),
            Self::Call { head, args, data } => f
                .debug_struct("Call")
                .field("head", head)
                .field("args", args)
                .field("data", data)
                .finish(),
            Self::CallMissingCloser { head, args, data } => f
                .debug_struct("CallMissingCloser")
                .field("head", head)
                .field("args", args)
                .field("data", data)
                .finish(),
            Self::UnterminatedCall { head, args, data } => f
                .debug_struct("UnterminatedCall")
                .field("head", head)
                .field("args", args)
                .field("data", data)
                .finish(),
            Self::SyntaxError {
                kind,
                children,
                data,
            } => f
                .debug_struct("SyntaxError")
                .field("kind", kind)
                .field("children", children)
                .field("data", data)
                .finish(),
            Self::AbstractSyntaxError { kind, args, data } => f
                .debug_struct("AbstractSyntaxError")
                .field("kind", kind)
                .field("args", args)
                .field("data", data)
                .finish(),
            Self::Box { kind, args, data } => f
                .debug_struct("Box")
                .field("kind", kind)
                .field("args", args)
                .field("data", data)
                .finish(),
            Self::Code {
                first,
                second,
                data,
            } => f
                .debug_struct("Code")
                .field("first", first)
                .field("second", second)
                .field("data", data)
                .finish(),
            Self::Group {
                kind,
                children,
                data,
            } => f
                .debug_struct("Group")
                .field("kind", kind)
                .field("children", children)
                .field("data", data)
                .finish(),
            Self::GroupMissingCloser {
                kind,
                children,
                data,
            } => f
                .debug_struct("GroupMissingCloser")
                .field("kind", kind)
                .field("children", children)
                .field("data", data)
                .finish(),
            Self::GroupMissingOpener {
                kind,
                children,
                data,
            } => f
                .debug_struct("GroupMissingOpener")
                .field("kind", kind)
                .field("children", children)
                .field("data", data)
                .finish(),
            Self::TagBox_GroupParen { group, tag, data } => f
                .debug_struct("TagBox_GroupParen")
                .field("group", group)
                .field("tag", tag)
                .field("data", data)
                .finish(),
            Self::PrefixNode_PrefixLinearSyntaxBang(arg0, arg1) => f
                .debug_tuple("PrefixNode_PrefixLinearSyntaxBang")
                .field(arg0)
                .field(arg1)
                .finish(),
        }
    }
}

impl Debug for AstMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let AstMetadata { source, issues } = self;

        if issues.is_empty() {
            if source.is_unknown() {
                return write!(f, "<||>");
            } else {
                return write!(f, "{}", source);
            }
        }

        f.debug_struct("AstMetadata")
            .field("source", source)
            .field("issues", issues)
            .finish()
    }
}
