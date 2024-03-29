use std::collections::HashSet;

use wolfram_library_link::{expr::Expr, wstp};

use wolfram_parser::{
    ast::{AbstractSyntaxError, Ast, AstMetadata},
    cst::{
        BinaryNode, BoxKind, BoxNode, CallBody, CallHead, CallNode, CodeNode,
        CompoundNode, Cst, GroupMissingCloserNode, GroupMissingOpenerNode,
        GroupNode, InfixNode, OperatorNode, PostfixNode, PrefixBinaryNode,
        PrefixNode, SyntaxErrorNode, TernaryNode,
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
    source::{
        BoxPosition, CharacterSpan, LineColumn, Location, Source, Span,
        SpanKind,
    },
    symbol::Symbol,
    symbols as sym,
    tokenize::{Token, TokenInput, TokenKind, TokenSource},
    Container, ContainerBody, ContainerKind, ContainerMissingReason, Metadata,
    NodeSeq, ParseResult, UnsafeCharacterEncoding,
};

use crate::from_expr::List;

pub trait WstpPut {
    fn put(&self, link: &mut wstp::Link);
}

//======================================
// Container
//======================================

impl<N: WstpPut> WstpPut for Container<N> {
    fn put(&self, link: &mut wstp::Link) {
        let Container {
            kind,
            body,
            metadata,
        } = self;

        link.put_function("CodeParser`ContainerNode", 3).unwrap();

        kind.put(link);
        body.put(link);
        metadata.put(link);
    }
}

impl WstpPut for ContainerKind {
    fn put(&self, link: &mut wstp::Link) {
        let symbol = match self {
            ContainerKind::String => Symbol::try_new("System`String").unwrap(),
            ContainerKind::File => Symbol::try_new("System`File").unwrap(),
            ContainerKind::Byte => Symbol::try_new("System`Byte").unwrap(),
            ContainerKind::Box => Symbol::try_new("System`Box").unwrap(),
            ContainerKind::Cell => Symbol::try_new("System`Cell").unwrap(),
            ContainerKind::Hold => Symbol::try_new("System`Hold").unwrap(),
        };

        Symbol_put(symbol, link);
    }
}

impl<S: WstpPut> WstpPut for ContainerBody<S> {
    fn put(&self, link: &mut wstp::Link) {
        match self {
            ContainerBody::Nodes(nodes) => nodes.put(link),
            ContainerBody::Missing(reason) => {
                link.put_function(sym::List.as_str(), 1).unwrap();
                link.put_function(sym::Missing.as_str(), 1).unwrap();

                let missing_reason: String = match reason {
                    ContainerMissingReason::EmptyInput => {
                        "EmptyInput".to_owned()
                    },
                    ContainerMissingReason::UnsafeCharacterEncoding(err) => {
                        let variant_name = err.as_str();

                        format!("UnsafeCharacterEncoding_{variant_name}")
                    },
                };

                link.put_str(&missing_reason).unwrap();
            },
        }
    }
}

impl WstpPut for Metadata {
    fn put(&self, link: &mut wstp::Link) {
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
        } = self;

        let mut len = 0;

        if include_source(source) {
            len += 1;
        }

        if syntax_issues.is_some() {
            len += 1;
        }

        if confidence_level.is_some() {
            len += 1;
        }

        if code_actions.is_some() {
            len += 1;
        }

        if additional_descriptions.is_some() {
            len += 1;
        }

        if file_name.is_some() {
            len += 1;
        }

        if embedded_tabs.is_some() {
            len += 1;
        }

        if embedded_newlines.is_some() {
            len += 1;
        }

        if simple_line_continuations.is_some() {
            len += 1;
        }

        if complex_line_continuations.is_some() {
            len += 1;
        }

        link.put_function(sym::Association.as_str(), len).unwrap();

        if let Some(issues) = syntax_issues {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol("CodeParser`SyntaxIssues").unwrap();

            // TODO: This clone() is unnecessarily inefficient.
            let issues = List(issues.clone());
            issues.put(link);
        }

        if let Some(embedded_tabs) = embedded_tabs {
            link.put_function("System`Rule", 2).unwrap();
            link.put_str("EmbeddedTabs").unwrap();
            link.put_expr(embedded_tabs).unwrap();
        }

        if let Some(embedded_newlines) = embedded_newlines {
            link.put_function("System`Rule", 2).unwrap();
            link.put_str("EmbeddedNewlines").unwrap();
            link.put_expr(embedded_newlines).unwrap();
        }

        if let Some(simple_line_continuations) = simple_line_continuations {
            link.put_function("System`Rule", 2).unwrap();
            link.put_str("SimpleLineContinuations").unwrap();
            link.put_expr(simple_line_continuations).unwrap();
        }

        if let Some(complex_line_continuations) = complex_line_continuations {
            link.put_function("System`Rule", 2).unwrap();
            link.put_str("ComplexLineContinuations").unwrap();
            link.put_expr(complex_line_continuations).unwrap();
        }

        if include_source(source) {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol(sym::CodeParser_Source.as_str()).unwrap();
            put_source_rhs(link, source);
        }

        if let Some(level) = confidence_level {
            link.put_expr(&Expr::number(level.clone())).unwrap();
        }

        if let Some(actions) = code_actions {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol(sym::CodeParser_CodeActions.as_str())
                .unwrap();

            // TODO: This clone() is unnecessarily inefficient.
            let actions = List(actions.clone());
            actions.put(link);
        }

        if let Some(additional_descriptions) = additional_descriptions {
            link.put_function("System`Rule", 2).unwrap();
            link.put_str("AdditionalDescriptions").unwrap();

            // TODO: This clone() is unnecessarily inefficient.
            let descs = List(additional_descriptions.clone());
            descs.put(link);
        }

        if let Some(file_name) = file_name {
            link.put_function("System`Rule", 2).unwrap();
            link.put_str("FileName").unwrap();
            link.put_expr(file_name).unwrap();
        }
    }
}

impl WstpPut for AstMetadata {
    fn put(&self, link: &mut wstp::Link) {
        let AstMetadata { source, issues } = self;

        let mut len = 0;

        if include_source(source) {
            len += 1;
        }

        len += !issues.is_empty() as usize;

        link.put_function(sym::Association.as_str(), len).unwrap();

        if include_source(source) {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol(sym::CodeParser_Source.as_str()).unwrap();
            put_source_rhs(link, source);
        }

        if !issues.is_empty() {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol("CodeParser`AbstractSyntaxIssues").unwrap();

            // TODO: This clone() is unnecessarily inefficient.
            let issues = List(issues.clone());
            issues.put(link);
        }
    }
}

//======================================
// Ast types
//======================================

impl WstpPut for Ast {
    fn put(&self, link: &mut wstp::Link) {
        match self {
            Ast::Leaf { kind, input, data } => {
                link.put_function(sym::CodeParser_LeafNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                let input: &[u8] = &input.as_bytes();
                let input = std::str::from_utf8(input)
                    .expect("token source span is not valid UTF-8");
                link.put_str(input).unwrap();

                data.put(link);
            },
            Ast::Error { kind, input, data } => {
                link.put_function(sym::CodeParser_ErrorNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                let input: &[u8] = &input.as_bytes();
                let input = std::str::from_utf8(input)
                    .expect("error token source span is not valid UTF-8");
                link.put_str(input).unwrap();

                data.put(link);
            },
            Ast::Call { head, args, data } => {
                link.put_function(sym::CodeParser_CallNode.as_str(), 3)
                    .unwrap();

                head.put(link);

                link.put_function(sym::List.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            Ast::CallMissingCloser { head, args, data } => {
                link.put_function(
                    sym::CodeParser_CallMissingCloserNode.as_str(),
                    3,
                )
                .unwrap();

                head.put(link);

                link.put_function(sym::List.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            Ast::SyntaxError {
                kind,
                children,
                data,
            } => {
                link.put_function(sym::CodeParser_SyntaxErrorNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                link.put_function(sym::List.as_str(), children.len())
                    .unwrap();

                for arg in children {
                    arg.put(link);
                }

                data.put(link);
            },
            Ast::AbstractSyntaxError { kind, args, data } => {
                link.put_function(
                    sym::CodeParser_AbstractSyntaxErrorNode.as_str(),
                    3,
                )
                .unwrap();

                kind.put(link);

                link.put_function(sym::List.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            Ast::Box { kind, args, data } => {
                link.put_function(sym::CodeParser_BoxNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                link.put_function(sym::List.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            Ast::Group {
                kind,
                children,
                data,
            } => {
                link.put_function(sym::CodeParser_GroupNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                {
                    let (opener, body, closer) = &**children;

                    link.put_function(sym::List.as_str(), 3).unwrap();
                    opener.put(link);
                    body.put(link);
                    closer.put(link);
                }

                data.put(link);
            },
            Ast::GroupMissingCloser {
                kind,
                children,
                data,
            } => {
                link.put_function(
                    sym::CodeParser_GroupMissingCloserNode.as_str(),
                    3,
                )
                .unwrap();

                kind.put(link);

                link.put_function(sym::List.as_str(), children.len())
                    .unwrap();

                for child in children {
                    child.put(link);
                }

                data.put(link);
            },
            Ast::GroupMissingOpener {
                kind,
                children,
                data,
            } => {
                link.put_function(
                    sym::CodeParser_GroupMissingOpenerNode.as_str(),
                    3,
                )
                .unwrap();

                kind.put(link);

                link.put_function(sym::List.as_str(), children.len())
                    .unwrap();

                for child in children {
                    child.put(link);
                }

                data.put(link);
            },
            Ast::Code {
                first,
                second,
                data,
            } => {
                link.put_function(sym::CodeParser_CodeNode.as_str(), 3)
                    .unwrap();

                link.put_expr(first).unwrap();
                link.put_expr(second).unwrap();
                data.put(link);
            },
            Ast::TagBox_GroupParen {
                group,
                tag,
                data: data1,
            } => {
                // BoxNode[
                //     TagBox,
                //     {
                //         GroupNode[GroupParen, {o, abstract[b], c}, data2],
                //         tag
                //     },
                //     data1
                // ]
                link.put_function(sym::CodeParser_BoxNode.as_str(), 3)
                    .unwrap();

                BoxKind::TagBox.put(link);

                link.put_function(sym::List.as_str(), 2).unwrap();

                // GroupNode[..]
                {
                    link.put_function(sym::CodeParser_GroupNode.as_str(), 3)
                        .unwrap();

                    GroupOperator::CodeParser_GroupParen.put(link);

                    let (o, b, c, data2) = &**group;
                    link.put_function(sym::List.as_str(), 3).unwrap();
                    o.put(link);
                    b.put(link);
                    c.put(link);

                    data2.put(link);
                }

                tag.put(link);

                data1.put(link);
            },
            Ast::PrefixNode_PrefixLinearSyntaxBang(children, data) => {
                link.put_function(sym::CodeParser_PrefixNode.as_str(), 3)
                    .unwrap();

                PrefixOperator::CodeParser_PrefixLinearSyntaxBang.put(link);

                link.put_function(sym::List.as_str(), children.len())
                    .unwrap();

                for child in children.iter() {
                    child.put(link);
                }

                data.put(link);
            },
        }
    }
}

impl WstpPut for AbstractSyntaxError {
    fn put(&self, link: &mut wstp::Link) {
        let symbol = format!("AbstractSyntaxError`{}", self.as_str());

        link.put_symbol(&symbol).unwrap();
    }
}

//======================================
// Token
//======================================

impl WstpPut for TokenKind {
    fn put(&self, link: &mut wstp::Link) {
        let sym = self.to_symbol();

        Symbol_put(sym, link);
    }
}

impl<I: TokenInput, S: TokenSource + WstpPut> WstpPut for Token<I, S> {
    fn put(&self, callLink: &mut wstp::Link) {
        let Token { tok, src, input } = self;

        if tok.isError() {
            callLink
                .put_function(sym::CodeParser_ErrorNode.as_str(), 3)
                .unwrap();
        } else {
            //
            // These are Symbols, Strings, Integers, Reals, Rationals.
            //

            callLink
                .put_function(sym::CodeParser_LeafNode.as_str(), 3)
                .unwrap();
        }

        tok.put(callLink);

        // bufLen().put(callLink);
        // let source: &[u8] = &session.tokenizer.input[span.offset..span.offset + span.len];
        let source: &[u8] = &input.as_bytes();

        let source = std::str::from_utf8(source)
            .expect("token source span is not valid UTF-8");
        callLink.put_str(source).unwrap();

        src.put(callLink);
    }
}

//======================================
// Node types
//======================================

impl<I: TokenInput, S: TokenSource + WstpPut> WstpPut for Cst<I, S> {
    fn put(&self, link: &mut wstp::Link) {
        match self {
            Cst::Token(token) => token.put(link),
            Cst::Call(node) => node.put(link),
            Cst::SyntaxError(node) => node.put(link),
            Cst::Infix(InfixNode(op)) => {
                put_op(link, op, sym::CodeParser_InfixNode)
            },
            Cst::Prefix(PrefixNode(op)) => {
                put_op(link, op, sym::CodeParser_PrefixNode)
            },
            Cst::Postfix(PostfixNode(op)) => {
                put_op(link, op, sym::CodeParser_PostfixNode)
            },
            Cst::Binary(BinaryNode(op)) => {
                put_op(link, op, sym::CodeParser_BinaryNode)
            },
            Cst::Ternary(TernaryNode(op)) => {
                put_op(link, op, sym::CodeParser_TernaryNode)
            },
            Cst::PrefixBinary(PrefixBinaryNode(op)) => {
                put_op(link, op, sym::CodeParser_PrefixBinaryNode)
            },
            Cst::Compound(CompoundNode(op)) => {
                put_op(link, op, sym::CodeParser_CompoundNode)
            },
            Cst::Group(GroupNode(op)) => {
                put_op(link, op, sym::CodeParser_GroupNode)
            },
            Cst::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                put_op(link, op, sym::CodeParser_GroupMissingCloserNode)
            },
            Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                put_op(link, op, sym::CodeParser_GroupMissingOpenerNode)
            },
            Cst::Box(box_node) => box_node.put(link),
            Cst::Code(node) => node.put(link),
        }
    }
}

impl<N: WstpPut> WstpPut for NodeSeq<N> {
    fn put(&self, callLink: &mut wstp::Link) {
        let NodeSeq(vec) = self;

        callLink
            .put_function(sym::List.as_str(), vec.len())
            .unwrap();

        for C in vec {
            if crate::feature::CHECK_ABORT && crate::abortQ() {
                Symbol_put(sym::_Aborted, callLink);
                continue;
            }

            C.put(callLink)
        }
    }
}

impl<I: TokenInput, S: TokenSource + WstpPut> WstpPut for BoxNode<I, S> {
    fn put(&self, link: &mut wstp::Link) {
        let BoxNode {
            kind,
            children,
            src,
        } = self;

        link.put_function(sym::CodeParser_BoxNode.as_str(), 3)
            .unwrap();
        kind.put(link);
        children.put(link);
        src.put(link);
    }
}

impl WstpPut for BoxKind {
    fn put(&self, link: &mut wstp::Link) {
        let symbol_name = self.as_str();
        debug_assert!(symbol_name.ends_with("Box"));

        let name = format!("System`{}", symbol_name);

        link.put_symbol(&name).unwrap();
    }
}

impl<S: WstpPut> WstpPut for CodeNode<S> {
    fn put(&self, link: &mut wstp::Link) {
        let CodeNode { first, second, src } = self;

        link.put_function(sym::CodeParser_CodeNode.as_str(), 3)
            .unwrap();
        link.put_expr(&first).unwrap();
        link.put_expr(&second).unwrap();
        src.put(link);
    }
}

fn put_op<I: TokenInput, S: TokenSource + WstpPut, O: WstpPut>(
    link: &mut wstp::Link,
    node: &OperatorNode<I, S, O>,
    op_head: Symbol,
) {
    let src = node.get_source();

    let OperatorNode { op, children } = node;

    link.put_function(op_head.as_str(), 3).unwrap();

    op.put(link);

    children.put(link);

    src.put(link);
}

impl WstpPut for InfixOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for PrefixOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for PostfixOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for BinaryOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for TernaryOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for PrefixBinaryOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for GroupOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for CallOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl WstpPut for CompoundOperator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl<I: TokenInput, S: TokenSource + WstpPut> WstpPut for CallNode<I, S> {
    fn put(&self, callLink: &mut wstp::Link) {
        let CallNode { head, body } = self;

        let src = self.get_source();

        callLink
            .put_function(sym::CodeParser_CallNode.as_str(), 3)
            .unwrap();

        // A CST can contiain CallNode[{__}, ..], but an aggregated
        // tree must not, because abstract[..] only checks for
        // CallNode[node_, ..]
        match head {
            // {...}
            CallHead::Concrete(head) => head.put(callLink),
            // node[...]
            CallHead::Aggregate(head) => head.put(callLink),
        }

        body.put(callLink);

        src.put(callLink);
    }
}

impl<I: TokenInput, S: TokenSource + WstpPut> WstpPut for CallBody<I, S> {
    fn put(&self, link: &mut wstp::Link) {
        match self {
            CallBody::Group(GroupNode(op)) => {
                put_op(link, op, sym::CodeParser_GroupNode)
            },
            CallBody::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                put_op(link, op, sym::CodeParser_GroupMissingCloserNode)
            },
        }
    }
}

impl<I: TokenInput, S: TokenSource + WstpPut> WstpPut
    for SyntaxErrorNode<I, S>
{
    fn put(&self, callLink: &mut wstp::Link) {
        let SyntaxErrorNode { err, children } = self;

        callLink
            .put_function(sym::CodeParser_SyntaxErrorNode.as_str(), 3)
            .unwrap();

        err.put(callLink);

        children.put(callLink);

        self.get_source().put(callLink);
    }
}

impl WstpPut for SyntaxErrorKind {
    fn put(&self, link: &mut wstp::Link) {
        let err: Symbol = self.to_symbol();

        Symbol_put(err, link);
    }
}

impl WstpPut for UnsafeCharacterEncoding {
    fn put(&self, link: &mut wstp::Link) {
        link.put_function(sym::Missing.as_str(), 1).unwrap();
        link.put_str(&self.missing_reason()).unwrap();
    }
}

//======================================
// Source types
//======================================

impl WstpPut for Issue {
    fn put(&self, callLink: &mut wstp::Link) {
        let Issue {
            make_sym,
            tag,
            msg,
            sev,
            src,
            val,
            actions,
            additional_descriptions,
            additional_sources,
        } = self;

        callLink.put_function(make_sym.as_str(), 4).unwrap();

        tag.put(callLink);

        callLink.put_str(msg).unwrap();

        sev.put(callLink);

        {
            let len = 2
                + (!actions.is_empty() as usize)
                + (!additional_descriptions.is_empty() as usize)
                + (!additional_sources.is_empty() as usize);

            callLink
                .put_function(sym::Association.as_str(), len)
                .unwrap();

            src.put(callLink);

            {
                callLink.put_function(sym::Rule.as_str(), 2).unwrap();

                Symbol_put(sym::ConfidenceLevel, callLink);

                callLink.put_f64(*val).unwrap();
            }

            if !actions.is_empty() {
                callLink.put_function(sym::Rule.as_str(), 2).unwrap();

                Symbol_put(sym::CodeParser_CodeActions, callLink);

                callLink
                    .put_function(sym::List.as_str(), actions.len())
                    .unwrap();

                for A in actions {
                    A.put(callLink);
                }
            }

            if !additional_descriptions.is_empty() {
                callLink.put_function(sym::Rule.as_str(), 2).unwrap();

                callLink.put_str("AdditionalDescriptions").unwrap();

                callLink
                    .put_function(
                        sym::List.as_str(),
                        additional_descriptions.len(),
                    )
                    .unwrap();

                for D in additional_descriptions {
                    callLink.put_str(D).unwrap();
                }
            }

            if !additional_sources.is_empty() {
                callLink.put_function(sym::Rule.as_str(), 2).unwrap();

                callLink.put_str("AdditionalSources").unwrap();

                callLink
                    .put_function(sym::List.as_str(), additional_sources.len())
                    .unwrap();

                for source in additional_sources {
                    put_source_rhs(callLink, source);
                }
            }
        }
    }
}

impl WstpPut for CodeAction {
    fn put(&self, callLink: &mut wstp::Link) {
        let CodeAction {
            label: Label,
            src: Src,
            kind,
        } = self;

        match kind {
            CodeActionKind::ReplaceText { replacement_text } => {
                callLink
                    .put_function(sym::CodeParser_CodeAction.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(sym::CodeParser_ReplaceText, callLink);

                {
                    callLink
                        .put_function(sym::Association.as_str(), 2)
                        .unwrap();

                    Src.put(callLink);

                    callLink.put_function(sym::Rule.as_str(), 2).unwrap();

                    callLink.put_str("ReplacementText").unwrap();

                    callLink.put_str(replacement_text).unwrap();
                }
            },
            CodeActionKind::InsertText { insertion_text } => {
                callLink
                    .put_function(sym::CodeParser_CodeAction.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(sym::CodeParser_InsertText, callLink);

                {
                    callLink
                        .put_function(sym::Association.as_str(), 2)
                        .unwrap();

                    Src.put(callLink);

                    callLink.put_function(sym::Rule.as_str(), 2).unwrap();

                    callLink.put_str("InsertionText").unwrap();

                    callLink.put_str(insertion_text).unwrap();
                }
            },
            CodeActionKind::DeleteText => {
                callLink
                    .put_function(sym::CodeParser_CodeAction.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(sym::CodeParser_DeleteText, callLink);

                {
                    callLink
                        .put_function(sym::Association.as_str(), 1)
                        .unwrap();

                    Src.put(callLink);
                }
            },
        }
    }
}

impl WstpPut for Location {
    fn put(&self, callLink: &mut wstp::Link) {
        let (first, second) = match self {
            Location::LineColumn(LineColumn(line, column)) => {
                (line.get(), column.get())
            },
            Location::CharacterIndex(index) => (0, *index),
        };

        callLink.put_function(sym::List.as_str(), 2).unwrap();

        callLink.put_i64(first.into()).unwrap();

        callLink.put_i64(second.into()).unwrap();
    }
}

/// Returns `true` if a `Source -> <source>` field should be included in
/// the node metadata `<| ... |>` being constructed by the caller.
fn include_source(source: &Source) -> bool {
    match source {
        Source::Unknown => false,
        Source::Box(BoxPosition::Spanning { .. })
            if crate::compatibility_mode() =>
        {
            // NOTE: Avoid outputing box positions like {1, 2, 3 ;; 4} that
            //       end in a Span if compatibility mode is active (the default).
            //       This is avoided Span box positions are a new feature, and
            //       not all uses of CodeParser Source data will be expecting
            //       them. For compatibility, better to treat the Source value
            //       as missing entirely.
            false
        },
        _ => true,
    }
}

fn put_source_rhs(link: &mut wstp::Link, source: &Source) {
    match source {
        Source::Span(span) => put_span_rhs(link, *span),
        Source::Box(box_pos) => put_box_position(link, box_pos),
        // `{}`
        // TODO: What representation should `<| Source -> <unknown> |>`
        //       have?
        //  * Source -> {}
        //  * Source -> None
        //  * Source -> Missing["Unknown"]
        //  * Panic here? (The caller should construct <||>, NOT
        //    <| Source -> <unknown> |>)
        Source::Unknown => {
            // link.put_function(sym::List.as_str(), 0).unwrap();
            panic!("unable to serialize Source::Unknown")
        },
    }
}

fn put_span_rhs(link: &mut wstp::Link, source: Span) {
    match source.kind() {
        SpanKind::LineColumnSpan { .. } => {
            let (start, end) = source.start_end();

            link.put_function(sym::List.as_str(), 2).unwrap();

            start.put(link);
            end.put(link);
        },
        SpanKind::CharacterSpan(CharacterSpan(start, end)) => {
            link.put_function(sym::List.as_str(), 2).unwrap();

            link.put_i64(start.into()).unwrap();

            link.put_i64((end - 1).into()).unwrap();
        },
    }
}

fn put_box_position(link: &mut wstp::Link, box_pos: &BoxPosition) {
    match box_pos {
        BoxPosition::At(indexes) => {
            link.put_function(sym::List.as_str(), indexes.len())
                .unwrap();

            for elem in indexes {
                let elem = i64::try_from(*elem)
                    .expect("box position usize index overflows i64");
                link.put_i64(elem).unwrap();
            }
        },
        BoxPosition::Spanning {
            index,
            span: (span_start, span_end),
        } => {
            let span_start =
                i64::try_from(*span_start).expect("Span start overflows i64");
            let span_end =
                i64::try_from(*span_end).expect("Span end overflows i64");

            link.put_function(sym::List.as_str(), index.len() + 1)
                .unwrap();

            for elem in index {
                let elem = i64::try_from(*elem)
                    .expect("box position usize index overflows i64");
                link.put_i64(elem).unwrap();
            }

            link.put_function(sym::Span.as_str(), 2).unwrap();

            link.put_i64(span_start).unwrap();
            link.put_i64(span_end).unwrap();
        },
        BoxPosition::Before(indexes) => {
            link.put_function(sym::Before.as_str(), 1).unwrap();

            link.put_function(sym::List.as_str(), indexes.len())
                .unwrap();
            for index in indexes {
                let index = i64::try_from(*index)
                    .expect("box position usize index overflows i64");
                link.put_i64(index).unwrap();
            }
        },
        BoxPosition::After(indexes) => {
            link.put_function(sym::After.as_str(), 1).unwrap();

            link.put_function(sym::List.as_str(), indexes.len())
                .unwrap();
            for index in indexes {
                let index = i64::try_from(*index)
                    .expect("box position usize index overflows i64");
                link.put_i64(index).unwrap();
            }
        },
    }
}

impl WstpPut for Span {
    fn put(&self, link: &mut wstp::Link) {
        link.put_function(sym::Association.as_str(), 1).unwrap();

        // Put: CodeParser`Source -> source
        link.put_function(sym::Rule.as_str(), 2).unwrap();
        Symbol_put(sym::CodeParser_Source, link);
        put_span_rhs(link, *self)
    }
}

impl WstpPut for Source {
    fn put(&self, link: &mut wstp::Link) {
        if !include_source(self) {
            // Put: <||>
            link.put_function(sym::Association.as_str(), 0).unwrap();
            return;
        }

        // Put: <| CodeParser`Source -> source |>
        link.put_function(sym::Association.as_str(), 1).unwrap();
        link.put_function(sym::Rule.as_str(), 2).unwrap();
        Symbol_put(sym::CodeParser_Source, link);

        put_source_rhs(link, self)
    }
}

fn put_source_locations(link: &mut wstp::Link, source_locs: HashSet<Location>) {
    link.put_function(sym::List.as_str(), source_locs.len())
        .unwrap();

    for loc in source_locs {
        loc.put(link);
    }
}

//======================================
// Result types
//======================================

impl<'i, N: WstpPut> WstpPut for ParseResult<N> {
    fn put(&self, link: &mut wstp::Link) {
        let ParseResult {
            syntax: outer_exprs,
            unsafe_character_encoding,
            fatal_issues,
            non_fatal_issues,
            tracked,
        } = self;

        link.put_function(sym::List.as_str(), 6).unwrap();

        // 1.
        // Collected expressions.
        match unsafe_character_encoding {
            None => outer_exprs.put(link),
            Some(flag) => {
                link.put_function(sym::List.as_str(), 1).unwrap();
                flag.put(link);
            },
        };

        //
        // Now handle the out-of-band expressions, i.e., issues and metadata
        //

        // 2.
        {
            //
            // if there are fatal issues, then only send fatal issues
            //
            let issues = if !fatal_issues.is_empty() {
                fatal_issues
            } else {
                non_fatal_issues
            };

            link.put_function(sym::List.as_str(), issues.len()).unwrap();

            for issue in issues {
                issue.put(link);
            }
        }

        // 3, 4, 5, 6
        for source_locs in tracked.to_nodes() {
            put_source_locations(link, source_locs);
        }
    }
}

//======================================
// Other
//======================================

// Note: This function can't be a method on Symbol because Symbol (currently) is
//       a type alias to a type from wolfram_expr, and its not legal in Rust
//       to implement methods on types that aren't part of the current crate.
pub(crate) fn Symbol_put(self_: Symbol, callLink: &mut wstp::Link) {
    callLink.put_symbol(self_.as_str()).unwrap();
}

impl WstpPut for Severity {
    fn put(&self, link: &mut wstp::Link) {
        let string: &'static str = self.as_str();

        link.put_str(string).unwrap();
    }
}

impl WstpPut for IssueTag {
    fn put(&self, link: &mut wstp::Link) {
        let string: &'static str = self.as_str();

        link.put_str(string).unwrap();
    }
}

//==========================================================
// Built-in Wolfram Language forms
//==========================================================

// TODO(cleanup): Add a Put trait and make this generic on List<T: Put>.
impl List<Issue> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let List(elements) = self;

        link.put_function("System`List", elements.len()).unwrap();

        for elem in elements {
            elem.put(link);
        }
    }
}

impl List<CodeAction> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let List(elements) = self;

        link.put_function("System`List", elements.len()).unwrap();

        for elem in elements {
            elem.put(link);
        }
    }
}

impl List<String> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let List(elements) = self;

        link.put_function("System`List", elements.len()).unwrap();

        for elem in elements {
            link.put_str(elem).unwrap();
        }
    }
}
