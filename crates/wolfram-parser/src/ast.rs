//! Abstract syntax trees.

use std::fmt::Debug;

use crate::{
    cst::{BoxKind, CodeNode},
    issue::Issue,
    parse::{operators::GroupOperator, SyntaxErrorKind},
    source::{BoxPosition, LineColumnSpan, Source, Span},
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
            Source::Box(_) | Source::Unknown => {
                todo!("non-typical source: {general_source:?}")
            },
        }
    }

    pub(crate) fn metadata(&self) -> &AstMetadata {
        match self {
            Ast::Leaf { data, .. } | Ast::Error { data, .. } => data,
            Ast::Call { data, .. } => data,
            Ast::CallMissingCloser { data, .. } => data,
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

    //==================================
    // Convenience constructor methods
    //==================================

    pub(crate) fn symbol(sym: SymbolRef) -> Self {
        Ast::symbol_with_data(sym, AstMetadata::empty())
    }

    pub(crate) fn symbol_with_data(
        sym: SymbolRef,
        data: impl Into<AstMetadata>,
    ) -> Self {
        // TODO(optimization): We only have to convert this to an allocated Symbol
        //                     because SymbolRef doesn't currently have context()
        //                     and symbol_name() methods. Add those methods to
        //                     SymbolRef in the wolfram-expr crate, and update this
        //                     to avoid the allocation.
        let sym: wolfram_expr::Symbol = sym.to_symbol();

        let input = if sym.context().as_str() == "System`" {
            sym.symbol_name().as_str()
        } else {
            // Play it safe for now and fully qualify any non-System` symbol
            sym.as_str()
        };

        Ast::Leaf {
            kind: TokenKind::Symbol,
            input: TokenString::from_string(input.to_owned()),
            data: data.into(),
        }
    }

    pub(crate) fn i64(int: i64) -> Self {
        Ast::i64_with_data(int, AstMetadata::empty())
    }

    pub(crate) fn i64_with_data(
        int: i64,
        data: impl Into<AstMetadata>,
    ) -> Self {
        Ast::Leaf {
            kind: TokenKind::Integer,
            input: TokenString::from_string(int.to_string()),
            data: data.into(),
        }
    }

    pub(crate) fn usize(int: usize) -> Self {
        Ast::Leaf {
            kind: TokenKind::Integer,
            input: TokenString::from_string(int.to_string()),
            data: AstMetadata::empty(),
        }
    }

    pub(crate) fn string(string: String, data: impl Into<AstMetadata>) -> Self {
        Ast::Leaf {
            kind: TokenKind::String,
            input: TokenString::from_string(string),
            data: data.into(),
        }
    }

    pub(crate) fn call(
        head: SymbolRef,
        args: Vec<Ast>,
        data: impl Into<AstMetadata>,
    ) -> Self {
        Ast::Call {
            head: Box::new(Ast::symbol(head)),
            args,
            data: data.into(),
        }
    }

    pub(crate) fn call2(
        head: Ast,
        args: Vec<Ast>,
        data: impl Into<AstMetadata>,
    ) -> Self {
        Ast::Call {
            head: Box::new(head),
            args,
            data: data.into(),
        }
    }

    pub(crate) fn call_missing_closer(
        head: Ast,
        args: Vec<Ast>,
        data: impl Into<AstMetadata>,
    ) -> Self {
        Ast::CallMissingCloser {
            head: Box::new(head),
            args,
            data: data.into(),
        }
    }

    pub(crate) fn syntax_error(
        kind: SyntaxErrorKind,
        args: Vec<Ast>,
        data: impl Into<AstMetadata>,
    ) -> Self {
        Ast::SyntaxError {
            kind,
            children: args,
            data: data.into(),
        }
    }

    pub(crate) fn abstract_syntax_error(
        kind: AbstractSyntaxError,
        args: Vec<Ast>,
        data: impl Into<AstMetadata>,
    ) -> Self {
        Ast::AbstractSyntaxError {
            kind,
            args,
            data: data.into(),
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

impl From<BoxPosition> for AstMetadata {
    fn from(value: BoxPosition) -> Self {
        AstMetadata {
            source: Source::Box(value),
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

use wolfram_expr::{symbol::SymbolRef, Expr};

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
