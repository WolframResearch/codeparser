use std::{cell::Cell, num::NonZeroU32};

use wolfram_library_link::expr::{
    symbol::SymbolRef, Expr, ExprKind, Normal, Number, Symbol,
};

use wolfram_parser::{
    cst::CstSeq,
    cst::{
        BinaryNode, BoxKind, BoxNode, CallBody, CallHead, CallNode, CodeNode,
        CompoundNode, Cst, GroupMissingCloserNode, GroupMissingOpenerNode,
        GroupNode, InfixNode, LeafNode, OperatorNode, PostfixNode,
        PrefixBinaryNode, PrefixNode, SyntaxErrorNode, TernaryNode,
    },
    issue::{CodeAction, CodeActionKind, Issue, IssueTag, Severity},
    parse::{
        operators::{
            BinaryOperator, CallOperator, CompoundOperator, GroupOperator,
            InfixOperator, Operator, PostfixOperator, PrefixBinaryOperator,
            PrefixOperator, TernaryOperator,
        },
        SyntaxErrorKind,
    },
    quirks::QuirkSettings,
    source::{BoxPosition, LineColumn, Source, Span},
    symbols as sym,
    tokenize::{Token, TokenKind, TokenString},
    Container, ContainerBody, ContainerKind, ContainerMissingReason, Metadata,
    NodeSeq, UnsafeCharacterEncoding,
};

pub(crate) trait FromExpr: Sized {
    fn from_expr(expr: &Expr) -> Result<Self, String>;
}

thread_local! {
    /// Global used to guide the implementation of [`Source::from_expr()`].
    ///
    /// See also [`from_expr_with_container_kind()`].
    static CONTAINER_KIND: Cell<Option<ContainerKind>> = Cell::new(None);
}

/// Parse `T` from an [`Expr`], using the specified [`ContainerKind`] to guide
/// the interpretation of `Source` values.
///
/// The `kind` [`ContainerKind`] value is used to guide the implementation of
/// [`Source::from_expr()`] calls made during the execution of this function.
///
/// Knowing `kind` is necessary to resolve an ambiguity: A node with metadata of
/// `<| Source -> {1, 2} |>` could either be a
/// [`CharacterSpan`][wolfram_parser::source::CharacterSpan] or a
/// [`Source::BoxPosition(_)`] specification, and there is no way to know
/// just by locally examining that expression.
///
/// To resolve the ambiguity, we must know what the [`ContainerKind`] value
/// was from the top-level `Container[kind, ...]` that is currently being
/// processed.
///
/// codeparser-wll public library functions should either use
/// [`Container::from_expr()`] or [`from_expr_with_container_kind()`] at the
/// top-level to ensure that this [`CONTAINER_KIND`] global is set during
/// the expr processing.
pub(crate) fn from_expr_with_container_kind<T: FromExpr>(
    expr: &Expr,
    kind: ContainerKind,
) -> Result<T, String> {
    CONTAINER_KIND.set(Some(kind));

    let result = T::from_expr(expr);

    CONTAINER_KIND.set(None);

    result
}

//==========================================================
// FromExpr impls
//==========================================================

macro_rules! compare_declared_and_computed_src {
    ($node_name:literal, $node:expr, $declared_src:expr) => {'ret_macro: {
        use wolfram_parser::source::BoxPosition;

        if !crate::codeparser_debug() {
            break 'ret_macro;
        }

        let computed_src  = $node.get_source();

        if computed_src == $declared_src {
            break 'ret_macro;
        }

        if $node_name == "GroupNode" && matches!(computed_src, Source::Box(BoxPosition::Spanning { .. })) {
            break 'ret_macro;
        }

        panic!(
            "declared {} source differs from computed: declared = {}, computed = {}",
            $node_name, $declared_src, computed_src
        );
    }}
}

impl FromExpr for Container<Cst<TokenString, Source>> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements =
            try_normal_with_head(expr, sym::CodeParser_ContainerNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let kind = match ContainerKind::from_expr(&elements[0]) {
            Ok(kind) => kind,
            Err(err) => {
                return Err(format!("invalid Container: {expr}: {err}"))
            },
        };

        CONTAINER_KIND.set(Some(kind));

        let body = ContainerBody::from_expr(&elements[1]).expect("PRE_COMMIT");

        let metadata = Metadata::from_expr(&elements[2])?;

        CONTAINER_KIND.set(None);

        Ok(Container {
            kind,
            body,
            metadata,
        })
    }
}

impl FromExpr for ContainerKind {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => todo!(),
        };

        let kind = match sym.as_str() {
            "System`String" => ContainerKind::String,
            "System`File" => ContainerKind::File,
            "System`Byte" => ContainerKind::Byte,
            "System`Box" => ContainerKind::Box,
            "System`Cell" => ContainerKind::Cell,
            "System`Hold" => ContainerKind::Hold,
            other => return Err(format!("invalid ContainerKind: {other}")),
        };

        Ok(kind)
    }
}

impl FromExpr for ContainerBody<Cst<TokenString, Source>> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        // Check for Missing["reason"]
        if let Ok(elements) = try_normal_with_head(expr, sym::List) {
            if elements.len() == 1 {
                if let Ok(missing_args) =
                    try_normal_with_head(&elements[0], sym::Missing)
                {
                    let Ok([missing_reason]): Result<&[_; 1], _> =
                        missing_args.try_into()
                    else {
                        return Err(format!("expected ContainerNode body Missing[..] to have 1 argument, got: {expr}"));
                    };

                    // Container[_, {Missing["EmptyInput"]}, _]
                    match missing_reason.try_as_str() {
                        Some("EmptyInput") => {
                            return Ok(ContainerBody::Missing(
                                ContainerMissingReason::EmptyInput,
                            ));
                        },
                        Some(other) => match UnsafeCharacterEncoding::from_missing_reason(other) {
                            Some(kind) => return Ok(ContainerBody::Missing(
                                ContainerMissingReason::UnsafeCharacterEncoding(kind)
                            )),
                            None => return Err(format!("invalid ContainerNode Missing reason: {missing_reason}"))
                        },
                        None => return Err(format!("invalid ContainerNode Missing reason: {missing_reason}")),
                    }
                }
            }
        }

        let nodes = NodeSeq::from_expr(expr)?;

        Ok(ContainerBody::Nodes(nodes))
    }
}

impl<N: FromExpr> FromExpr for NodeSeq<N> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::List)?;

        let nodes = elements
            .into_iter()
            .map(N::from_expr)
            .collect::<Result<Vec<_>, String>>()?;

        Ok(NodeSeq(nodes))
    }
}

impl FromExpr for Cst<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        if let Ok(LeafNode { kind, input, src }) = LeafNode::from_expr(expr) {
            let token = Token {
                tok: kind,
                src,
                input: TokenString {
                    buf: input.into_bytes(),
                },
            };
            return Ok(Cst::Token(token));
        }


        if let Ok(node) = CallNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = CompoundNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = PrefixNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = InfixNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = PrefixBinaryNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = BinaryNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = TernaryNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = PostfixNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = GroupNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = BoxNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = GroupMissingCloserNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = GroupMissingOpenerNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = SyntaxErrorNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = CodeNode::from_expr(expr) {
            // return Ok(Self::from(node));
            return Ok(Cst::Code(node));
        }

        // todo!("expr: {expr}")

        Err(format!("not a known Node variant: {expr}"))
    }
}

impl FromExpr for LeafNode {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let normal = try_normal(expr)?;

        let head = normal.head();

        if *head != Symbol::new("CodeParser`LeafNode")
            && *head != Symbol::new("CodeParser`ErrorNode")
        {
            return Err(format!("expected LeafNode[..] or ErrorNode[..]"));
        }

        let elements = normal.elements();

        if elements.len() != 3 {
            todo!()
        }

        let kind = TokenKind::from_expr(&elements[0])?;
        let input: String = match elements[1].try_as_str() {
            Some(string) => string.to_owned(),
            None => {
                return Err(format!(
                    "expected 2nd element of LeafNode to be string"
                ))
            },
        };
        let Metadata { source, .. } = Metadata::from_expr(&elements[2])?;

        let src = source;

        Ok(LeafNode { kind, input, src })
    }
}

impl FromExpr for CallNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_CallNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let head = if elements[0].has_normal_head(&Symbol::new("System`List")) {
            let List(head) = List::from_expr(&elements[0]).expect("PRE_COMMIT");

            CallHead::Concrete(NodeSeq(head))
        } else {
            // TODO(cleanup): Is this branch used?
            CallHead::aggregate(Cst::from_expr(&elements[0])?)
        };

        let body = if let Ok(group) = GroupNode::from_expr(&elements[1]) {
            CallBody::Group(group)
        } else if let Ok(group) =
            GroupMissingCloserNode::from_expr(&elements[1])
        {
            CallBody::GroupMissingCloser(group)
        } else {
            todo!("unexpected CallNode body: {}", elements[1])
        };

        let metadata = Metadata::from_expr(&elements[2])?;

        let node = CallNode { head, body };

        compare_declared_and_computed_src!("CallNode", node, metadata.source);

        Ok(node)
    }
}

impl FromExpr for PrefixNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_PrefixNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = PrefixOperator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;

        let Metadata { source, .. } = Metadata::from_expr(&elements[2])?;

        let node = PrefixNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("PrefixNode", node, source);

        Ok(node)
    }
}

impl FromExpr for InfixNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_InfixNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = InfixOperator::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children = NodeSeq::from_expr(&elements[1]).expect("_PRE_COMMIT");

        let Metadata { source, .. } =
            Metadata::from_expr(&elements[2]).expect("PRE_COMMIT");

        let node = InfixNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("InfixNode", node, source);

        Ok(node)
    }
}

impl FromExpr for PrefixBinaryNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements =
            try_normal_with_head(expr, sym::CodeParser_PrefixBinaryNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = PrefixBinaryOperator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = PrefixBinaryNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("PrefixBinaryNode", node, src);

        Ok(node)
    }
}

impl FromExpr for BinaryNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_BinaryNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = BinaryOperator::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children = NodeSeq::from_expr(&elements[1]).expect("PRE_COMMIT");
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = BinaryNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("BinaryNode", node, src);

        Ok(node)
    }
}

impl FromExpr for TernaryNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_TernaryNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = TernaryOperator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = TernaryNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("TernaryNode", node, src);

        Ok(node)
    }
}

impl FromExpr for PostfixNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_PostfixNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = PostfixOperator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = PostfixNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("PostfixNode", node, src);

        Ok(node)
    }
}

impl<O: FromExpr> FromExpr for GroupNode<TokenString, Source, O> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_GroupNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = O::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children = NodeSeq::from_expr(&elements[1]).expect("PRE_COMMIT");
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = GroupNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("GroupNode", node, src);

        Ok(node)
    }
}

impl FromExpr for BoxNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_BoxNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let kind = BoxKind::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children: CstSeq<TokenString, Source> =
            NodeSeq::from_expr(&elements[1]).expect("PRE_COMMIT");
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(BoxNode {
            kind,
            children,
            src,
        })
    }
}

impl FromExpr for BoxKind {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let symbol = expr.try_as_symbol().expect("PRE_COMMIT");

        let kind = match BoxKind::from_symbol(symbol.as_symbol_ref()) {
            Some(kind) => kind,
            None => panic!("unrecognized BoxKind name: '{symbol}'"),
        };

        Ok(kind)
    }
}

impl FromExpr for CodeNode<Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::CodeParser_CodeNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let first = elements[0].clone();
        let second = elements[1].clone();
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(CodeNode { first, second, src })
    }
}

impl<O: FromExpr> FromExpr for GroupMissingCloserNode<TokenString, Source, O> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements =
            try_normal_with_head(expr, sym::CodeParser_GroupMissingCloserNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = O::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = GroupMissingCloserNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("GroupMissingCloserNode", node, src);

        Ok(node)
    }
}

impl FromExpr for GroupMissingOpenerNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements =
            try_normal_with_head(expr, sym::CodeParser_GroupMissingOpenerNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = GroupOperator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = GroupMissingOpenerNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("GroupMissingOpenerNode", node, src);

        Ok(node)
    }
}

impl FromExpr for CompoundNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements =
            try_normal_with_head(expr, sym::CodeParser_CompoundNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = CompoundOperator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = CompoundNode(OperatorNode { op, children });

        compare_declared_and_computed_src!("CompoundNode", node, src);

        Ok(node)
    }
}

impl FromExpr for SyntaxErrorNode<TokenString, Source> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements =
            try_normal_with_head(expr, sym::CodeParser_SyntaxErrorNode)?;

        if elements.len() != 3 {
            todo!()
        }

        let err = match elements[0].try_as_symbol() {
            Some(sym) => sym,
            None => todo!(),
        };

        if err.context().as_str() != "SyntaxError`" {
            todo!()
        }

        let variant_name = err.symbol_name().as_str();

        let err = match SyntaxErrorKind::from_str(variant_name) {
            Some(kind) => kind,
            None => {
                panic!("unrecognized SyntaxErrorKind name: '{variant_name}'")
            },
        };

        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        let node = SyntaxErrorNode { err, children };

        // TODO: Make this a warning instead? We're effectively throwing away the
        //       source the caller specified, so if it differs from the computed
        //       source that may indicate a bug or something surprising to the
        //       caller.
        debug_assert!(node.get_source() == src);

        Ok(node)
    }
}

impl FromExpr for UnsafeCharacterEncoding {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::Missing)?;

        if elements.len() != 1 {
            todo!()
        }

        let err: &str = match elements[0].try_as_str() {
            Some(sym) => sym,
            None => todo!(),
        };

        let Some(kind) = UnsafeCharacterEncoding::from_missing_reason(err)
        else {
            return Err(format!(
                "not a recognized UnsafeCharacterEncoding name: '{err}'"
            ));
        };

        Ok(kind)
    }
}

impl FromExpr for Metadata {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let Association(rules) = Association::from_expr(expr)?;

        let mut source: Option<Expr> = None;
        let mut syntax_issues: Option<Expr> = None;
        let mut confidence_level: Option<Expr> = None;
        let mut code_actions: Option<Expr> = None;
        let mut additional_descriptions: Option<Expr> = None;
        let mut file_name: Option<Expr> = None;
        let mut embedded_tabs: Option<Expr> = None;
        let mut embedded_newlines: Option<Expr> = None;
        let mut simple_line_continuations: Option<Expr> = None;
        let mut complex_line_continuations: Option<Expr> = None;

        for Rule { lhs, rhs } in rules {
            if lhs == Symbol::new("CodeParser`Source") {
                source = Some(rhs);
            } else if lhs == Symbol::new("CodeParser`SyntaxIssues") {
                syntax_issues = Some(rhs);
            } else if lhs == Symbol::new("System`ConfidenceLevel") {
                confidence_level = Some(rhs);
            } else if lhs == Symbol::new("CodeParser`CodeActions") {
                code_actions = Some(rhs);
            } else if lhs == Expr::string("EmbeddedTabs") {
                embedded_tabs = Some(rhs);
            } else if lhs == Expr::string("EmbeddedNewlines") {
                embedded_newlines = Some(rhs);
            } else if lhs == Expr::string("ComplexLineContinuations") {
                complex_line_continuations = Some(rhs);
            } else if lhs == Expr::string("SimpleLineContinuations") {
                simple_line_continuations = Some(rhs);
            } else if lhs == Expr::string("InsertionText") {
                // PRE_COMMIT
            } else if lhs == Expr::string("ReplacementText") {
                // PRE_COMMIT
            } else if lhs == Expr::string("FileName") {
                file_name = Some(rhs);
            } else if lhs == Expr::string("AdditionalDescriptions") {
                additional_descriptions = Some(rhs);
            } else {
                todo!("unrecognized Source LHS: {lhs}")
            }
        }

        let source: Source = match source {
            Some(source) => Source::from_expr(&source)?,
            None => Source::unknown(),
        };

        let syntax_issues: Option<Vec<Issue>> = match syntax_issues {
            Some(expr) => {
                let List(issues) = List::<Issue>::from_expr(&expr)?;
                Some(issues)
            },
            None => None,
        };

        let confidence_level: Option<Number> = match confidence_level {
            Some(level) => Some(level.try_as_number().expect("PRE_COMMIT")),
            None => None,
        };

        let code_actions = match code_actions {
            Some(expr) => {
                let List(actions) = List::<CodeAction>::from_expr(&expr)?;
                Some(actions)
            },
            None => None,
        };

        let additional_descriptions = match additional_descriptions {
            Some(expr) => {
                let List(descs) = List::<String>::from_expr(&expr)?;
                Some(descs)
            },
            None => None,
        };

        Ok(Metadata {
            source,
            syntax_issues,
            confidence_level,
            code_actions,
            additional_descriptions,
            file_name,
            embedded_tabs,
            embedded_newlines,
            simple_line_continuations,
            complex_line_continuations,
            // embedded_tabs,
        })
    }
}

impl FromExpr for Source {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        // Check for After[{..}] and Before[{..}]
        if let Some([list]) = try_normal_with_head_and_len(expr, sym::After)? {
            let List(indexes) =
                List::<usize>::from_expr(list).map_err(|err| {
                    format!("invalid After[..] box position list: {err}")
                })?;

            return Ok(Source::Box(BoxPosition::After(indexes)));
        } else if let Some([list]) =
            try_normal_with_head_and_len(expr, sym::Before)?
        {
            let List(indexes) =
                List::<usize>::from_expr(list).map_err(|err| {
                    format!("invalid Before[..] box position list: {err}")
                })?;

            return Ok(Source::Box(BoxPosition::Before(indexes)));
        }

        let elements = try_normal_with_head(expr, sym::List)?;

        // NOTE: If you see this panic, use from_expr_with_container_kind(..)
        //       instead of TheType::from_expr() at the top level of whatever
        //       FromExpr conversion is triggering this panic.
        let global_container_kind = CONTAINER_KIND.get().expect("CONTAINER_KIND was None on code path that executed Source::from_expr()");

        let [source_start, source_end] = match global_container_kind {
            ContainerKind::Cell | ContainerKind::Box => {
                let List(indexes) =
                    List::<usize>::from_expr(expr).map_err(|err| {
                        format!("invalid box position list: {err}")
                    })?;

                // Note: `elements` can sometimes be {}, {1, 1, 1}, etc.
                //        These are the source positions of boxes.
                return Ok(Source::Box(BoxPosition::At(indexes)));
            },
            ContainerKind::String
            | ContainerKind::Byte
            | ContainerKind::File
            | ContainerKind::Hold => {
                let Ok([start_pos, end_pos]): Result<&[_; 2], _> =
                    elements.try_into()
                else {
                    return Err(format!(
                        "expected Source value of node inside container of kind {:?} to have length 2; got: {:?}",
                        global_container_kind,
                        elements
                    ));
                };

                [start_pos, end_pos]
            },
        };

        if let Ok(start_index) = get_source_pos(&source_start) {
            let end_index = get_source_pos(&source_end)?;

            return Ok(Source::Span(Span::from_character_span(
                start_index,
                // FIXME: We add one here because in Span::put() we
                //        subtract 1. Instead of doing this in the WL
                //        serialization/deserialization, rationalize the
                //        representation of this field in Rust/WL so that
                //        they're consistent by definition.
                end_index + 1,
            )));
        }

        let start = try_normal_with_head(&source_start, sym::List)?;
        let end = try_normal_with_head(&source_end, sym::List)?;

        if start.len() != 2 || end.len() != 2 {
            todo!()
        }

        fn get_source_pos(expr: &Expr) -> Result<u32, String> {
            let value: i64 = match expr.kind() {
                ExprKind::Integer(int) => *int,
                _ => {
                    return Err(format!(
                        "expected source Location u32, got: {expr}"
                    ))
                },
            };

            match u32::try_from(value) {
                Ok(value) => Ok(value),
                Err(_) => Err(format!(
                    "source location component overflows u32: {value}"
                )),
            }
        }

        let start_first = get_source_pos(&start[0])?;
        let start_second = get_source_pos(&start[1])?;

        let end_first = get_source_pos(&end[0])?;
        let end_second = get_source_pos(&end[1])?;

        let span = span_from_raw_values(
            start_first,
            start_second,
            end_first,
            end_second,
        )?;

        Ok(Source::Span(span))
    }
}

fn span_from_raw_values(
    start_first: u32,
    start_second: u32,
    end_first: u32,
    end_second: u32,
) -> Result<Span, String> {
    let span = match [start_first, start_second, end_first, end_second]
        .map(NonZeroU32::new)
    {
        [
            Some(start_line), Some(start_column),
            Some(end_line), Some(end_column)
        ] => {
            Span::line_column(
                LineColumn(start_line, start_column),
                LineColumn(end_line, end_column),
            )
        },
        [
            None, Some(start_char),
            None, Some(end_char)
        ] => {
            Span::from_character_span(start_char.get(), end_char.get())
        },
        // FIXME: Construct Span::Unknown here instead, to avoid the hard error?
        //        Ideally print a message as well, to distinguish this from a
        //        truly just plain absent Source.
        values => return Err(format!(
            "Invalid source specification. Not recognizable as either line:column span or character span. Values: {:?}",
            values
        ))
    };

    Ok(span)
}

impl FromExpr for Span {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        match Source::from_expr(expr)? {
            Source::Span(span) => Ok(span),
            Source::Unknown => todo!("Span from Source::Unknown"),
            Source::Box(_) => todo!("Span from Source::Box"),
        }
    }
}

impl FromExpr for InfixOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match InfixOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for PrefixOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match PrefixOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for PostfixOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match PostfixOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for BinaryOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match BinaryOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for TernaryOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match TernaryOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for PrefixBinaryOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match PrefixBinaryOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for GroupOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match GroupOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for CallOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match CallOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for CompoundOperator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match CompoundOperator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for TokenKind {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym: &wolfram_library_link::expr::Symbol =
            match expr.try_as_symbol() {
                Some(sym) => sym,
                None => panic!(),
            };

        let kind = match TokenKind::from_symbol(sym.as_symbol_ref()) {
            Some(kind) => kind,
            None => {
                return Err(format!("symbol is not a known TokenKind: {sym}"))
            },
        };

        Ok(kind)
    }
}

impl FromExpr for Issue {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        // FIXME: What about the other Issue `make_sym` values?
        let normal = try_normal(expr)?;

        let head = normal.head();
        let elements = normal.elements();

        let head = head.try_as_symbol().expect("PRE_COMMIT");

        const HEADS: &[SymbolRef] = &[
            sym::CodeParser_SyntaxIssue,
            sym::CodeParser_EncodingIssue,
            sym::CodeParser_FormatIssue,
        ];

        let make_sym = *HEADS
            .iter()
            .find(|choice| choice.as_str() == head.as_str())
            .expect("PRE_COMMIT");

        if elements.len() != 4 {
            todo!()
        }

        let tag = elements[0].try_as_str().expect("PRE_COMMIT");
        let msg = elements[1].try_as_str().expect("PRE_COMMIT").to_owned();
        let sev = elements[2].try_as_str().expect("PRE_COMMIT");
        let Metadata {
            source,
            syntax_issues,
            confidence_level,
            code_actions,
            additional_descriptions,
            file_name,
            embedded_tabs,
            embedded_newlines,
            simple_line_continuations,
            complex_line_continuations,
        } = Metadata::from_expr(&elements[3]).expect("PRE_COMMIT");

        if syntax_issues.is_some() {
            todo!("unexpected SyntaxIssues field in Issue metadata");
        }
        if file_name.is_some() {
            todo!("unexpected FileName field in Issue metadata");
        }
        if embedded_tabs.is_some() {
            todo!("unexpected EmbeddedTabs field in Issue metadata");
        }
        if embedded_newlines.is_some() {
            todo!("unexpected EmbeddedNewlines field in Issue metadata");
        }
        if simple_line_continuations.is_some() {
            todo!("unexpected SimpleLineContinuations field in Issue metadata");
        }
        if complex_line_continuations.is_some() {
            todo!(
                "unexpected ComplexLineContinuations field in Issue metadata"
            );
        }

        let tag = IssueTag::from_str(tag).expect("PRE_COMMIT");
        let sev = Severity::from_str(sev).expect("PRE_COMMIT");

        let val: f64 = match confidence_level {
            Some(Number::Integer(int)) => int as f64,
            Some(Number::Real(real)) => *real,
            None => todo!(),
        };

        Ok(Issue {
            make_sym,
            tag,
            msg,
            sev,
            src: source,
            val,
            actions: code_actions.unwrap_or_else(Vec::new),
            additional_descriptions: additional_descriptions
                .unwrap_or_else(Vec::new),
            // FIXME: These aren't always empty.
            additional_sources: vec![],
        })
    }
}

impl FromExpr for CodeAction {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        // FIXME: What about the other Issue `make_sym` values?
        let elements = try_normal_with_head(expr, sym::CodeParser_CodeAction)?;

        if elements.len() != 3 {
            todo!()
        }

        let label: String =
            elements[0].try_as_str().expect("PRE_COMMIT").to_owned();
        let kind: &Symbol = elements[1].try_as_symbol().expect("PRE_COMMIT");
        let assoc = Association::from_expr(&elements[2]).expect("PRE_COMMIT");

        if kind.context().as_str() != "CodeParser`" {
            todo!()
        }

        let kind = match kind.symbol_name().as_str() {
            "ReplaceText" => {
                let text: &Expr = assoc
                    .lookup(&Expr::string("ReplacementText"))
                    .expect("PRE_COMMIT");
                let text: String =
                    text.try_as_str().expect("PRE_COMMIT").to_owned();

                CodeActionKind::ReplaceText {
                    replacement_text: text,
                }
            },
            "InsertText" => {
                let text: &Expr = assoc
                    .lookup(&Expr::string("InsertionText"))
                    .expect("PRE_COMMIT");
                let text: String =
                    text.try_as_str().expect("PRE_COMMIT").to_owned();

                CodeActionKind::InsertText {
                    insertion_text: text,
                }
            },
            "DeleteText" => CodeActionKind::DeleteText,
            _ => todo!(),
        };

        let src = Metadata::from_expr(&elements[2])
            .expect("PRE_COMMIT")
            .source;

        let src = match src {
            Source::Span(src) => src,
            Source::Box(_) | Source::Unknown => {
                todo!("unexpected source for CodeAction: {src:?}")
            },
        };

        Ok(CodeAction { label, kind, src })
    }
}


impl FromExpr for QuirkSettings {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let Association(rules) = Association::from_expr(expr)?;

        let mut infix_binary_at: Option<Expr> = None;
        let mut flatten_times: Option<Expr> = None;
        let mut old_at_at_at: Option<Expr> = None;

        for Rule { lhs, rhs } in rules {
            if lhs == Expr::string("InfixBinaryAt") {
                infix_binary_at = Some(rhs);
            } else if lhs == Expr::string("FlattenTimes") {
                flatten_times = Some(rhs);
            } else if lhs == Expr::string("OldAtAtAt") {
                old_at_at_at = Some(rhs);
            } else {
                todo!("unrecognized QuirkSettings LHS: {lhs}")
            }
        }

        let default = QuirkSettings::default();

        let infix_binary_at = infix_binary_at
            .as_ref()
            .and_then(Expr::try_as_bool)
            .unwrap_or(default.infix_binary_at);

        let flatten_times = flatten_times
            .as_ref()
            .and_then(Expr::try_as_bool)
            .unwrap_or(default.flatten_times);

        let old_at_at_at = old_at_at_at
            .as_ref()
            .and_then(Expr::try_as_bool)
            .unwrap_or(default.old_at_at_at);

        Ok(QuirkSettings {
            infix_binary_at,
            flatten_times,
            old_at_at_at,
        })
    }
}

//==========================================================
// Built-in Wolfram Language forms
//==========================================================

struct Association(pub Vec<Rule>);

impl FromExpr for Association {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::Association)?;

        let rules: Vec<Rule> = elements
            .into_iter()
            .map(Rule::from_expr)
            .collect::<Result<Vec<Rule>, String>>()?;

        Ok(Association(rules))
    }
}

impl Association {
    fn lookup(&self, needle: &Expr) -> Option<&Expr> {
        let Association(rules) = self;

        rules
            .iter()
            .find(|Rule { lhs, rhs: _ }| lhs == needle)
            .map(|Rule { lhs: _, rhs }| rhs)
    }
}

pub(crate) struct List<T>(pub Vec<T>);

impl<T: FromExpr> FromExpr for List<T> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(&expr, sym::List)?;

        let elements: Vec<T> = elements
            .into_iter()
            .map(T::from_expr)
            .collect::<Result<Vec<_>, String>>()?;

        Ok(List(elements))
    }
}

impl FromExpr for List<String> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(&expr, sym::List)?;

        let elements: Vec<String> = elements
            .into_iter()
            .map(|elem| match elem.try_as_str() {
                Some(elem) => Ok(elem.to_owned()),
                None => Err(format!("expected List element to be String")),
            })
            .collect::<Result<Vec<_>, String>>()?;

        Ok(List(elements))
    }
}

impl FromExpr for List<usize> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::List)?;

        let mut indexes = Vec::new();

        for elem in elements {
            let index: i64 = match elem.kind() {
                ExprKind::Integer(int) => *int,
                _ => {
                    return Err(format!(
                        "expected non-negative Integer, got {elem}"
                    ))
                },
            };

            let index = match usize::try_from(index) {
                Ok(index) => index,
                Err(err) => return Err(format!(
                    "expected non-negative Integer, got negative or too large to fix in usize: {err}: {index}"
                ))
            };

            indexes.push(index);
        }

        Ok(List(indexes))
    }
}

struct Rule {
    lhs: Expr,
    rhs: Expr,
}

impl FromExpr for Rule {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, sym::Rule)?;

        if elements.len() != 2 {
            todo!()
        }

        Ok(Rule {
            lhs: elements[0].clone(),
            rhs: elements[1].clone(),
        })
    }
}

//==========================================================
// FromExpr Utilities
//==========================================================

fn try_normal(expr: &Expr) -> Result<&Normal, String> {
    expr.try_as_normal()
        .ok_or_else(|| format!("expected Normal expression, got: {expr}"))
}

fn try_normal_with_head<'e>(
    expr: &'e Expr,
    expected_head: SymbolRef,
) -> Result<&'e [Expr], String> {
    let normal = try_normal(expr)?;

    if *normal.head() == expected_head.to_symbol() {
        Ok(normal.elements())
    } else {
        Err(format!(
            "expected Normal expression with head '{}', got head '{}'",
            expected_head.as_str(),
            normal.head()
        ))
    }
}

fn try_normal_with_head_2<'e>(
    expr: &'e Expr,
    expected_head: SymbolRef,
) -> Option<&'e [Expr]> {
    let Some(normal) = expr.try_as_normal() else {
        return None;
    };

    if *normal.head() == expected_head.to_symbol() {
        Some(normal.elements())
    } else {
        None
    }
}

fn try_normal_with_head_and_len<'e, const LEN: usize>(
    expr: &'e Expr,
    expected_head: SymbolRef,
) -> Result<Option<&'e [Expr; LEN]>, String> {
    let Some(elements) = try_normal_with_head_2(expr, expected_head) else {
        return Ok(None);
    };

    let array: &[_; LEN] = elements.try_into().map_err(|_| {
        format!(
            "expected expression with head '{}' and length {LEN}, \
             actual expression had length {}: ",
            expected_head.as_str(),
            elements.len()
        )
    })?;

    Ok(Some(array))
}
