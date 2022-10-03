use ordered_float::NotNan;
use wolfram_expr::{symbol::SymbolRef, ExprKind, Number};
use wolfram_library_link::expr::{Expr, Normal, Symbol};

use crate::{
    node::{
        BinaryNode, BoxKind, BoxNode, CallNode, CodeNode, CompoundNode, GroupMissingCloserNode,
        GroupNode, InfixNode, LeafNode, Node, NodeSeq, Operator, OperatorNode, PostfixNode,
        PrefixBinaryNode, PrefixNode, SyntaxErrorKind, SyntaxErrorNode, TernaryNode,
        UnterminatedGroupNode,
    },
    source::{CodeAction, CodeActionKind, GeneralSource, Issue, IssueTag, Severity},
    symbol_registration::*,
    token::{OwnedTokenInput, Token, TokenKind},
    token_enum_registration::SymbolToToken,
    Container, ContainerBody, ContainerKind, Metadata, Source, SourceLocation,
    UnsafeCharacterEncoding,
};

pub(crate) trait FromExpr: Sized {
    fn from_expr(expr: &Expr) -> Result<Self, String>;
}

//==========================================================
// FromExpr impls
//==========================================================

impl FromExpr for Container<GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(
            expr,
            SymbolRef::try_new("CodeParser`ContainerNode").unwrap(),
        )?;

        if elements.len() != 3 {
            todo!()
        }

        let kind = match ContainerKind::from_expr(&elements[0]) {
            Ok(kind) => kind,
            Err(err) => return Err(format!("invalid Container: {expr}: {err}")),
        };
        let body = ContainerBody::from_expr(&elements[1]).expect("PRE_COMMIT");
        let metadata = Metadata::from_expr(&elements[2])?;

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
            "System`Box" => ContainerKind::Box,
            "System`Hold" => ContainerKind::Hold,
            "System`Byte" => ContainerKind::Byte,
            other => return Err(format!("invalid ContainerKind: {other}")),
        };

        Ok(kind)
    }
}

impl FromExpr for ContainerBody<GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        if let Ok(elements) = try_normal_with_head(expr, SYMBOL_LIST) {
            if elements.len() == 1 {
                if let Ok(node) = UnsafeCharacterEncoding::from_expr(&elements[0]) {
                    return Ok(ContainerBody::Missing(node));
                }
            }
        }

        let nodes = NodeSeq::from_expr(expr)?;

        Ok(ContainerBody::Nodes(nodes))
    }
}

impl FromExpr for NodeSeq<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_LIST)?;

        let nodes = elements
            .into_iter()
            .map(Node::from_expr)
            .collect::<Result<Vec<_>, String>>()?;

        Ok(NodeSeq(nodes))
    }
}

impl FromExpr for Node<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        if let Ok(LeafNode { kind, input, src }) = LeafNode::from_expr(expr) {
            let token = Token {
                tok: kind,
                src,
                input: OwnedTokenInput {
                    buf: input.into_bytes(),
                },
            };
            return Ok(Node::Token(token));
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

        if let Ok(node) = UnterminatedGroupNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = SyntaxErrorNode::from_expr(expr) {
            return Ok(Self::from(node));
        }

        if let Ok(node) = CodeNode::from_expr(expr) {
            // return Ok(Self::from(node));
            return Ok(Node::Code(node));
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
            None => return Err(format!("expected 2nd element of LeafNode to be string")),
        };
        let Metadata { source, .. } = Metadata::from_expr(&elements[2])?;

        let src = source;

        Ok(LeafNode { kind, input, src })
    }
}

impl FromExpr for CallNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_CALLNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let (head, is_concrete) = if elements[0].has_normal_head(&Symbol::new("System`List")) {
            let List(head) = List::from_expr(&elements[0]).expect("PRE_COMMIT");

            (NodeSeq(head), true)
        } else {
            // PRE_COMMIT: Is this variant used?
            (NodeSeq(vec![Node::from_expr(&elements[0])?]), false)
        };
        let body = if let Ok(group) = GroupNode::from_expr(&elements[1]) {
            Node::Group(group)
        } else if let Ok(group) = UnterminatedGroupNode::from_expr(&elements[1]) {
            Node::UnterminatedGroup(group)
        } else if let Ok(group) = GroupMissingCloserNode::from_expr(&elements[1]) {
            Node::GroupMissingCloser(group)
        } else {
            todo!("unexpected CallNode body: {}", elements[1])
        };
        let metadata = Metadata::from_expr(&elements[2])?;
        let src = metadata.source;

        Ok(CallNode {
            head,
            body: Box::new(body),
            src,
            is_concrete,
        })
    }
}

impl FromExpr for PrefixNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_PREFIXNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;

        let Metadata { source, .. } = Metadata::from_expr(&elements[2])?;

        let src = source;

        Ok(PrefixNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for InfixNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_INFIXNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children = NodeSeq::from_expr(&elements[1]).expect("_PRE_COMMIT");

        let Metadata { source, .. } = Metadata::from_expr(&elements[2]).expect("PRE_COMMIT");

        let src = source;

        Ok(InfixNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for PrefixBinaryNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_PREFIXBINARYNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(PrefixBinaryNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for BinaryNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_BINARYNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children = NodeSeq::from_expr(&elements[1]).expect("PRE_COMMIT");
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(BinaryNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for TernaryNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_TERNARYNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(TernaryNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for PostfixNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_POSTFIXNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(PostfixNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for GroupNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_GROUPNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children = NodeSeq::from_expr(&elements[1]).expect("PRE_COMMIT");
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(GroupNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for BoxNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_BOXNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let kind = BoxKind::from_expr(&elements[0]).expect("PRE_COMMIT");
        let children: NodeSeq<OwnedTokenInput, GeneralSource> =
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

        let context = symbol.context().as_str();
        let symbol_name = symbol.symbol_name().as_str();

        if context != "System`" {
            todo!()
        }

        if !symbol_name.ends_with("Box") {
            todo!()
        }

        let kind = match BoxKind::from_str(symbol_name) {
            Some(kind) => kind,
            None => panic!("unrecognized BoxKind name: '{symbol_name}'"),
        };

        Ok(kind)
    }
}

impl FromExpr for CodeNode<GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_CODENODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let first = elements[0].clone();
        let second = elements[1].clone();
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(CodeNode { first, second, src })
    }
}

impl FromExpr for UnterminatedGroupNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(UnterminatedGroupNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for GroupMissingCloserNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(GroupMissingCloserNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for CompoundNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_COMPOUNDNODE)?;

        if elements.len() != 3 {
            todo!()
        }

        let op = Operator::from_expr(&elements[0])?;
        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(CompoundNode(OperatorNode { op, children, src }))
    }
}

impl FromExpr for SyntaxErrorNode<OwnedTokenInput, GeneralSource> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_SYNTAXERRORNODE)?;

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
            None => panic!("unrecognized SyntaxErrorKind name: '{variant_name}'"),
        };

        let children = NodeSeq::from_expr(&elements[1])?;
        let src = Metadata::from_expr(&elements[2])?.source;

        Ok(SyntaxErrorNode { err, children, src })
    }
}

impl FromExpr for UnsafeCharacterEncoding {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_MISSING)?;

        if elements.len() != 1 {
            todo!()
        }

        let err: &str = match elements[0].try_as_str() {
            Some(sym) => sym,
            None => todo!(),
        };

        if !err.starts_with("UnsafeCharacterEncoding_") {
            todo!()
        }

        let variant_name: &str = err.trim_start_matches("UnsafeCharacterEncoding_");

        let kind = match UnsafeCharacterEncoding::from_str(variant_name) {
            Some(kind) => kind,
            None => {
                panic!("unrecognized UnsafeCharacterEncoding name: '{variant_name}'")
            },
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
            } else {
                todo!("unrecognized Source LHS: {lhs}")
            }
        }

        let source: GeneralSource = match source {
            Some(source) => GeneralSource::from_expr(&source)?,
            None => GeneralSource::unknown(),
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

        Ok(Metadata {
            source,
            syntax_issues,
            confidence_level,
            code_actions,
            file_name,
            embedded_tabs,
            embedded_newlines,
            simple_line_continuations,
            complex_line_continuations,
            // embedded_tabs,
        })
    }
}

impl FromExpr for GeneralSource {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_LIST)?;

        if elements.len() != 2 {
            let mut indexes = Vec::new();

            for elem in elements {
                let index: i64 = match elem.kind() {
                    ExprKind::Integer(int) => *int,
                    _ => {
                        return Err(format!(
                            "invalid box position element: {elem}. Expected Integer."
                        ))
                    },
                };
                let index = match usize::try_from(index) {
                    Ok(index) => index,
                    Err(err) => return Err(format!(
                        "invalid box position index: negative or too large to fix in usize: {err}: {index}"
                    ))
                };

                indexes.push(index);
            }

            // Note: `elements` can sometimes be {}, {1, 1, 1}, etc.
            //        These are the source positions of boxes.
            return Ok(GeneralSource::BoxPosition(indexes));
        }

        if let Ok(start_index) = get_source_pos(&elements[0]) {
            let end_index = get_source_pos(&elements[1])?;

            return Ok(GeneralSource::String(Source {
                start: SourceLocation {
                    first: 0,
                    second: start_index,
                },
                end: SourceLocation {
                    first: 0,
                    // FIXME: We add one here because in Source::put() we
                    //        subtract 1. Instead of doing this in the WL
                    //        serialization/deserialization, rationalize the
                    //        representation of this field in Rust/WL so that
                    //        they're consistent by definition.
                    second: end_index + 1,
                },
            }));
        }

        let start = try_normal_with_head(&elements[0], SYMBOL_LIST)?;
        let end = try_normal_with_head(&elements[1], SYMBOL_LIST)?;

        if start.len() != 2 || end.len() != 2 {
            todo!()
        }

        fn get_source_pos(expr: &Expr) -> Result<u32, String> {
            let value: i64 = match expr.kind() {
                ExprKind::Integer(int) => *int,
                _ => return Err(format!("expected SourceLocation u32, got: {expr}")),
            };

            match u32::try_from(value) {
                Ok(value) => Ok(value),
                Err(_) => Err(format!("source location component overflows u32: {value}")),
            }
        }

        let start_first = get_source_pos(&start[0])?;
        let start_second = get_source_pos(&start[1])?;

        let end_first = get_source_pos(&end[0])?;
        let end_second = get_source_pos(&end[1])?;

        Ok(GeneralSource::String(Source {
            start: SourceLocation {
                first: start_first,
                second: start_second,
            },
            end: SourceLocation {
                first: end_first,
                second: end_second,
            },
        }))
    }
}

impl FromExpr for Source {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        match GeneralSource::from_expr(expr)? {
            GeneralSource::String(source) => Ok(source),
            GeneralSource::BoxPosition(_) => todo!("Source from GeneralSource::BoxPosition"),
        }
    }
}

impl FromExpr for Operator {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        match Operator::try_from_symbol(sym.as_symbol_ref()) {
            Some(op) => Ok(op),
            None => Err(format!("unable to match symbol '{sym}' to Operator")),
        }
    }
}

impl FromExpr for TokenKind {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let sym: &wolfram_expr::Symbol = match expr.try_as_symbol() {
            Some(sym) => sym,
            None => panic!(),
        };

        let kind = match SymbolToToken(sym.as_symbol_ref()) {
            Some(kind) => kind,
            None => return Err(format!("symbol is not a known TokenKind: {sym}")),
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
            SYMBOL_CODEPARSER_SYNTAXISSUE,
            SYMBOL_CODEPARSER_ENCODINGISSUE,
            SYMBOL_CODEPARSER_FORMATISSUE,
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
            todo!("unexpected ComplexLineContinuations field in Issue metadata");
        }

        let tag = IssueTag::from_str(tag).expect("PRE_COMMIT");
        let sev = Severity::from_str(sev).expect("PRE_COMMIT");

        let val: NotNan<f64> = match confidence_level {
            Some(Number::Integer(int)) => NotNan::new(int as f64).unwrap(),
            Some(Number::Real(real)) => NotNan::new(*real).unwrap(),
            None => todo!(),
        };

        Ok(Issue {
            make_sym,
            tag,
            msg,
            sev,
            src: source,
            val,
            // FIXME: These aren't always empty.
            actions: code_actions.unwrap_or_else(Vec::new),
            additional_descriptions: vec![],
            additional_sources: vec![],
        })
    }
}

impl FromExpr for CodeAction {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        // FIXME: What about the other Issue `make_sym` values?
        let elements = try_normal_with_head(expr, SYMBOL_CODEPARSER_CODEACTION)?;

        if elements.len() != 3 {
            todo!()
        }

        let label: String = elements[0].try_as_str().expect("PRE_COMMIT").to_owned();
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
                let text: String = text.try_as_str().expect("PRE_COMMIT").to_owned();

                CodeActionKind::ReplaceText {
                    replacement_text: text,
                }
            },
            "InsertText" => {
                let text: &Expr = assoc
                    .lookup(&Expr::string("InsertionText"))
                    .expect("PRE_COMMIT");
                let text: String = text.try_as_str().expect("PRE_COMMIT").to_owned();

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
            GeneralSource::String(src) => src,
            GeneralSource::BoxPosition(_) => {
                todo!("unexpected source for CodeAction: {src:?}")
            },
        };

        Ok(CodeAction { label, kind, src })
    }
}

//==========================================================
// Built-in Wolfram Language forms
//==========================================================

struct Association(pub Vec<Rule>);

impl FromExpr for Association {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_ASSOCIATION)?;

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

pub(crate) struct List<T: FromExpr>(pub Vec<T>);

impl<T: FromExpr> FromExpr for List<T> {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(&expr, SYMBOL_LIST)?;

        let elements: Vec<T> = elements
            .into_iter()
            .map(T::from_expr)
            .collect::<Result<Vec<_>, String>>()?;

        Ok(List(elements))
    }
}

struct Rule {
    lhs: Expr,
    rhs: Expr,
}

impl FromExpr for Rule {
    fn from_expr(expr: &Expr) -> Result<Self, String> {
        let elements = try_normal_with_head(expr, SYMBOL_RULE)?;

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
