use std::collections::HashSet;

use wolfram_library_link::{expr::Expr, wstp};

use crate::{
    ast::{AbstractSyntaxError, AstMetadata, AstNode},
    cst::CstNode,
    from_expr::List,
    my_string::MyString,
    my_string_registration::*,
    node::{
        BinaryNode, BoxKind, BoxNode, CallNode, CodeNode, CompoundNode, GroupMissingCloserNode,
        GroupMissingOpenerNode, GroupNode, InfixNode, Node, NodeSeq, Operator, OperatorNode,
        PostfixNode, PrefixBinaryNode, PrefixNode, SyntaxErrorKind, SyntaxErrorNode, TernaryNode,
    },
    source::{
        BufferAndLength, CharacterRange, CodeAction, CodeActionKind, GeneralSource, Issue,
        IssueTag, Severity, Source, SourceLocation, StringSourceKind,
    },
    symbol::Symbol,
    symbol_registration::{self as sym, *},
    token::{BorrowedTokenInput, Token, TokenInput, TokenKind},
    token_enum_registration::TokenToSymbol,
    Container, ContainerBody, ContainerKind, Metadata, ParseResult, Tokens,
    UnsafeCharacterEncoding,
};

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

impl ContainerKind {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let symbol = match self {
            ContainerKind::String => Symbol::try_new("System`String").unwrap(),
            ContainerKind::File => Symbol::try_new("System`File").unwrap(),
            ContainerKind::Box => Symbol::try_new("System`Box").unwrap(),
            ContainerKind::Hold => Symbol::try_new("System`Hold").unwrap(),
            ContainerKind::Byte => Symbol::try_new("System`Byte").unwrap(),
        };

        Symbol_put(symbol, link);
    }
}

impl<S: WstpPut> WstpPut for ContainerBody<S> {
    fn put(&self, link: &mut wstp::Link) {
        match self {
            ContainerBody::Nodes(nodes) => nodes.put(link),
            ContainerBody::Missing(flag) => {
                link.put_function(SYMBOL_LIST.as_str(), 1).unwrap();
                flag.put(link);
            },
        }
    }
}

impl Metadata {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
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

        if !source.is_unknown() {
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

        link.put_function(SYMBOL_ASSOCIATION.as_str(), len).unwrap();

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

        if !source.is_unknown() {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol(SYMBOL_CODEPARSER_SOURCE.as_str()).unwrap();
            match source {
                GeneralSource::String(source) => put_source_rhs(link, *source),
                GeneralSource::BoxPosition(other) => put_box_position(link, other),
                GeneralSource::After(expr) => link.put_expr(expr).unwrap(),
            }
        }

        if let Some(level) = confidence_level {
            link.put_expr(&Expr::number(level.clone())).unwrap();
        }

        if let Some(actions) = code_actions {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol(SYMBOL_CODEPARSER_CODEACTIONS.as_str())
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

        if !source.is_unknown() {
            len += 1;
        }

        len += !issues.is_empty() as usize;

        link.put_function(SYMBOL_ASSOCIATION.as_str(), len).unwrap();

        if !source.is_unknown() {
            link.put_function("System`Rule", 2).unwrap();
            link.put_symbol(SYMBOL_CODEPARSER_SOURCE.as_str()).unwrap();
            match source {
                GeneralSource::String(source) => put_source_rhs(link, *source),
                GeneralSource::BoxPosition(other) => put_box_position(link, other),
                GeneralSource::After(expr) => link.put_expr(expr).unwrap(),
            }
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
// AstNode types
//======================================

impl WstpPut for AstNode {
    fn put(&self, link: &mut wstp::Link) {
        match self {
            AstNode::Leaf { kind, input, data } => {
                link.put_function(SYMBOL_CODEPARSER_LEAFNODE.as_str(), 3)
                    .unwrap();

                kind.put(link);

                let input: &[u8] = &input.as_bytes();
                let input =
                    std::str::from_utf8(input).expect("token source span is not valid UTF-8");
                link.put_str(input).unwrap();

                data.put(link);
            },
            AstNode::Error { kind, input, data } => {
                link.put_function(SYMBOL_CODEPARSER_ERRORNODE.as_str(), 3)
                    .unwrap();

                kind.put(link);

                let input: &[u8] = &input.as_bytes();
                let input =
                    std::str::from_utf8(input).expect("error token source span is not valid UTF-8");
                link.put_str(input).unwrap();

                data.put(link);
            },
            AstNode::Call { head, args, data } => {
                link.put_function(SYMBOL_CODEPARSER_CALLNODE.as_str(), 3)
                    .unwrap();

                head.put(link);

                link.put_function(SYMBOL_LIST.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            AstNode::CallMissingCloser { head, args, data } => {
                link.put_function(sym::CodeParser_CallMissingCloserNode.as_str(), 3)
                    .unwrap();

                head.put(link);

                link.put_function(SYMBOL_LIST.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            AstNode::UnterminatedCall { head, args, data } => {
                link.put_function(sym::CodeParser_UnterminatedCallNode.as_str(), 3)
                    .unwrap();

                head.put(link);

                link.put_function(SYMBOL_LIST.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            AstNode::SyntaxError {
                kind,
                children,
                data,
            } => {
                link.put_function(sym::CodeParser_SyntaxErrorNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                link.put_function(SYMBOL_LIST.as_str(), children.len())
                    .unwrap();

                for arg in children {
                    arg.put(link);
                }

                data.put(link);
            },
            AstNode::AbstractSyntaxError { kind, args, data } => {
                link.put_function(SYMBOL_CODEPARSER_ABSTRACTSYNTAXERRORNODE.as_str(), 3)
                    .unwrap();

                kind.put(link);

                link.put_function(SYMBOL_LIST.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            AstNode::Box { kind, args, data } => {
                link.put_function(SYMBOL_CODEPARSER_BOXNODE.as_str(), 3)
                    .unwrap();

                kind.put(link);

                link.put_function(SYMBOL_LIST.as_str(), args.len()).unwrap();

                for arg in args {
                    arg.put(link);
                }

                data.put(link);
            },
            AstNode::Group {
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
            AstNode::GroupMissingCloser {
                kind,
                children,
                data,
            } => {
                link.put_function(sym::CodeParser_GroupMissingCloserNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                link.put_function(SYMBOL_LIST.as_str(), children.len())
                    .unwrap();

                for child in children {
                    child.put(link);
                }

                data.put(link);
            },
            AstNode::GroupMissingOpener {
                kind,
                children,
                data,
            } => {
                link.put_function(sym::CodeParser_GroupMissingOpenerNode.as_str(), 3)
                    .unwrap();

                kind.put(link);

                link.put_function(SYMBOL_LIST.as_str(), children.len())
                    .unwrap();

                for child in children {
                    child.put(link);
                }

                data.put(link);
            },
            AstNode::Code {
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
            AstNode::TagBox_GroupParen {
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
                link.put_function(SYMBOL_CODEPARSER_BOXNODE.as_str(), 3)
                    .unwrap();

                BoxKind::TagBox.put(link);

                link.put_function(SYMBOL_LIST.as_str(), 2).unwrap();

                // GroupNode[..]
                {
                    link.put_function(SYMBOL_CODEPARSER_GROUPNODE.as_str(), 3)
                        .unwrap();

                    Operator::CodeParser_GroupParen.put(link);

                    let (o, b, c, data2) = &**group;
                    link.put_function(SYMBOL_LIST.as_str(), 3).unwrap();
                    o.put(link);
                    b.put(link);
                    c.put(link);

                    data2.put(link);
                }

                tag.put(link);

                data1.put(link);
            },
            AstNode::PrefixNode_PrefixLinearSyntaxBang(children, data) => {
                link.put_function(SYMBOL_CODEPARSER_PREFIXNODE.as_str(), 3)
                    .unwrap();

                Operator::CodeParser_PrefixLinearSyntaxBang.put(link);

                link.put_function(SYMBOL_LIST.as_str(), children.len())
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
        let sym = TokenToSymbol(*self);

        Symbol_put(sym, link);
    }
}

impl<I: TokenInput, S: WstpPut> Token<I, S> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let Token { tok, src, input } = self;

        if tok.isError() {
            callLink
                .put_function(SYMBOL_CODEPARSER_ERRORNODE.as_str(), 3)
                .unwrap();
        } else {
            //
            // These are Symbols, Strings, Integers, Reals, Rationals.
            //

            callLink
                .put_function(SYMBOL_CODEPARSER_LEAFNODE.as_str(), 3)
                .unwrap();
        }

        tok.put(callLink);

        // bufLen().put(callLink);
        // let source: &[u8] = &session.tokenizer.input[span.offset..span.offset + span.len];
        let source: &[u8] = &input.as_bytes();

        let source = std::str::from_utf8(source).expect("token source span is not valid UTF-8");
        callLink.put_str(source).unwrap();

        src.put(callLink);
    }
}

//======================================
// Node types
//======================================

impl<I: TokenInput, S: WstpPut> WstpPut for Node<I, S> {
    fn put(&self, link: &mut wstp::Link) {
        match self {
            Node::Token(token) => token.put(link),
            Node::Call(node) => node.put(link),
            Node::SyntaxError(node) => node.put(link),
            Node::Infix(InfixNode(op)) => op.put(link, SYMBOL_CODEPARSER_INFIXNODE),
            Node::Prefix(PrefixNode(op)) => op.put(link, SYMBOL_CODEPARSER_PREFIXNODE),
            Node::Postfix(PostfixNode(op)) => op.put(link, SYMBOL_CODEPARSER_POSTFIXNODE),
            Node::Binary(BinaryNode(op)) => op.put(link, SYMBOL_CODEPARSER_BINARYNODE),
            Node::Ternary(TernaryNode(op)) => op.put(link, SYMBOL_CODEPARSER_TERNARYNODE),
            Node::PrefixBinary(PrefixBinaryNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_PREFIXBINARYNODE)
            },
            Node::Compound(CompoundNode(op)) => op.put(link, SYMBOL_CODEPARSER_COMPOUNDNODE),
            Node::Group(GroupNode(op)) => op.put(link, SYMBOL_CODEPARSER_GROUPNODE),
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE)
            },
            Node::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_GROUPMISSINGOPENERNODE)
            },
            Node::Box(box_node) => box_node.put(link),
            Node::Code(node) => node.put(link),
        }
    }
}

impl<N: WstpPut> NodeSeq<N> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let NodeSeq(vec) = self;

        callLink
            .put_function(SYMBOL_LIST.as_str(), vec.len())
            .unwrap();

        for C in vec {
            if crate::feature::CHECK_ABORT && crate::abortQ() {
                Symbol_put(SYMBOL__ABORTED, callLink);
                continue;
            }

            C.put(callLink)
        }
    }
}

impl<'i> Tokens<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let Tokens(tokens) = self;

        link.put_function(SYMBOL_LIST.as_str(), tokens.len())
            .unwrap();

        for token in tokens {
            if crate::feature::CHECK_ABORT && crate::abortQ() {
                Symbol_put(SYMBOL__ABORTED, link);
                continue;
            }

            token.put(link);
        }
    }
}

impl<I: TokenInput, S: WstpPut> BoxNode<I, S> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let BoxNode {
            kind,
            children,
            src,
        } = self;

        link.put_function(SYMBOL_CODEPARSER_BOXNODE.as_str(), 3)
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

impl<S: WstpPut> CodeNode<S> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let CodeNode { first, second, src } = self;

        link.put_function(SYMBOL_CODEPARSER_CODENODE.as_str(), 3)
            .unwrap();
        link.put_expr(&first).unwrap();
        link.put_expr(&second).unwrap();
        src.put(link);
    }
}

impl<I: TokenInput, S: WstpPut> OperatorNode<I, S> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link, op_head: Symbol) {
        let OperatorNode { op, children, src } = self;

        callLink.put_function(op_head.as_str(), 3).unwrap();

        op.put(callLink);

        children.put(callLink);

        src.put(callLink);
    }
}

impl WstpPut for Operator {
    fn put(&self, link: &mut wstp::Link) {
        Symbol_put(self.to_symbol(), link)
    }
}

impl<I: TokenInput, S: WstpPut> CallNode<I, S> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CallNode {
            head,
            body,
            src,
            is_concrete,
        } = self;

        callLink
            .put_function(SYMBOL_CODEPARSER_CALLNODE.as_str(), 3)
            .unwrap();

        if *is_concrete {
            head.put(callLink);
        } else {
            // PRE_COMMIT: A CST can contiain CallNode[{__}, ..], but an aggregated
            //             tree must not, because abstract[..] only checks for
            //             CallNode[node_, ..]
            let NodeSeq(head) = head;
            if head.len() != 1 {
                todo!();
            }

            let head = &head[0];

            head.put(callLink);
        }

        body.put(callLink);

        src.put(callLink);
    }
}

impl<I: TokenInput, S: WstpPut> SyntaxErrorNode<I, S> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let SyntaxErrorNode { err, children, src } = self;


        callLink
            .put_function(SYMBOL_CODEPARSER_SYNTAXERRORNODE.as_str(), 3)
            .unwrap();

        err.put(callLink);

        children.put(callLink);

        src.put(callLink);
    }
}

impl WstpPut for SyntaxErrorKind {
    fn put(&self, link: &mut wstp::Link) {
        let err: Symbol = self.to_symbol();

        Symbol_put(err, link);
    }
}

impl UnsafeCharacterEncoding {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        link.put_function(SYMBOL_MISSING.as_str(), 1).unwrap();

        let variant_name: &'static str = self.as_str();

        let name = format!("UnsafeCharacterEncoding_{variant_name}");

        link.put_str(&name).unwrap();
    }
}

//======================================
// Source types
//======================================

impl<'i> BufferAndLength<'i> {
    #[allow(dead_code)]
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        callLink.put_str(self.as_str()).unwrap();
    }
}

impl Issue {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
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
                .put_function(SYMBOL_ASSOCIATION.as_str(), len)
                .unwrap();

            src.put(callLink);

            {
                callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                Symbol_put(SYMBOL_CONFIDENCELEVEL, callLink);

                callLink.put_f64(**val).unwrap();
            }

            if !actions.is_empty() {
                callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_CODEACTIONS, callLink);

                callLink
                    .put_function(SYMBOL_LIST.as_str(), actions.len())
                    .unwrap();

                for A in actions {
                    A.put(callLink);
                }
            }

            if !additional_descriptions.is_empty() {
                callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                STRING_ADDITIONALDESCRIPTIONS.put(callLink);

                callLink
                    .put_function(SYMBOL_LIST.as_str(), additional_descriptions.len())
                    .unwrap();

                for D in additional_descriptions {
                    callLink.put_str(D).unwrap();
                }
            }

            if !additional_sources.is_empty() {
                callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                callLink.put_str("AdditionalSources").unwrap();

                callLink
                    .put_function(SYMBOL_LIST.as_str(), additional_sources.len())
                    .unwrap();

                for source in additional_sources {
                    match source {
                        GeneralSource::String(source) => put_source_rhs(callLink, *source),
                        GeneralSource::BoxPosition(other) => put_box_position(callLink, other),
                        GeneralSource::After(expr) => callLink.put_expr(expr).unwrap(),
                    }
                }
            }
        }
    }
}

impl CodeAction {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CodeAction {
            label: Label,
            src: Src,
            kind,
        } = self;

        match kind {
            CodeActionKind::ReplaceText { replacement_text } => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_REPLACETEXT, callLink);

                {
                    callLink
                        .put_function(SYMBOL_ASSOCIATION.as_str(), 2)
                        .unwrap();

                    Src.put(callLink);

                    callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                    STRING_REPLACEMENTTEXT.put(callLink);

                    callLink.put_str(replacement_text).unwrap();
                }
            },
            CodeActionKind::InsertText { insertion_text } => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_INSERTTEXT, callLink);

                {
                    callLink
                        .put_function(SYMBOL_ASSOCIATION.as_str(), 2)
                        .unwrap();

                    Src.put(callLink);

                    callLink.put_function(SYMBOL_RULE.as_str(), 2).unwrap();

                    STRING_INSERTIONTEXT.put(callLink);

                    callLink.put_str(insertion_text).unwrap();
                }
            },
            CodeActionKind::DeleteText => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.as_str(), 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                Symbol_put(SYMBOL_CODEPARSER_DELETETEXT, callLink);

                {
                    callLink
                        .put_function(SYMBOL_ASSOCIATION.as_str(), 1)
                        .unwrap();

                    Src.put(callLink);
                }
            },
        }
    }
}

impl SourceLocation {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let SourceLocation { first, second } = *self;

        callLink.put_function(SYMBOL_LIST.as_str(), 2).unwrap();

        callLink.put_i64(first.into()).unwrap();

        callLink.put_i64(second.into()).unwrap();
    }
}

fn put_source_rhs(link: &mut wstp::Link, source: Source) {
    match source.kind() {
        StringSourceKind::LineColumnRange { .. } => {
            let Source { start, end } = source;

            link.put_function(SYMBOL_LIST.as_str(), 2).unwrap();

            start.put(link);
            end.put(link);
        },
        StringSourceKind::CharacterRange(CharacterRange(start, end)) => {
            link.put_function(SYMBOL_LIST.as_str(), 2).unwrap();

            link.put_i64(start.into()).unwrap();

            link.put_i64((end - 1).into()).unwrap();
        },
        // `{}`
        // TODO: What representation should `<| Source -> <unknown> |>`
        //       have?
        //  * Source -> {}
        //  * Source -> None
        //  * Source -> Missing["Unknown"]
        //  * Panic here? (The caller should construct <||>, NOT
        //    <| Source -> <unknown> |>)
        StringSourceKind::Unknown => {
            // link.put_function(SYMBOL_LIST.as_str(), 0).unwrap();
            panic!("unable to serialize StringSourceKind::Unknown")
        },
    }
}

fn put_box_position(link: &mut wstp::Link, indexes: &Vec<usize>) {
    link.put_function(SYMBOL_LIST.as_str(), indexes.len())
        .unwrap();

    for elem in indexes {
        let elem = i64::try_from(*elem).expect("box position usize index overflows i64");
        link.put_i64(elem).unwrap();
    }
}

impl WstpPut for Source {
    fn put(&self, link: &mut wstp::Link) {
        link.put_function(SYMBOL_ASSOCIATION.as_str(), 1).unwrap();

        // Put: CodeParser`Source -> source
        link.put_function(SYMBOL_RULE.as_str(), 2).unwrap();
        Symbol_put(SYMBOL_CODEPARSER_SOURCE, link);
        put_source_rhs(link, *self)
    }
}

impl WstpPut for GeneralSource {
    fn put(&self, link: &mut wstp::Link) {
        if self.is_unknown() {
            // Put: <||>
            link.put_function(SYMBOL_ASSOCIATION.as_str(), 0).unwrap();
            return;
        }

        // Put: <| CodeParser`Source -> source |>
        link.put_function(SYMBOL_ASSOCIATION.as_str(), 1).unwrap();
        link.put_function(SYMBOL_RULE.as_str(), 2).unwrap();
        Symbol_put(SYMBOL_CODEPARSER_SOURCE, link);

        match self {
            GeneralSource::String(source) => put_source_rhs(link, *source),
            GeneralSource::BoxPosition(other) => put_box_position(link, other),
            GeneralSource::After(expr) => link.put_expr(expr).unwrap(),
        }
    }
}

fn put_source_locations(link: &mut wstp::Link, source_locs: HashSet<SourceLocation>) {
    link.put_function(SYMBOL_LIST.as_str(), source_locs.len())
        .unwrap();

    for loc in source_locs {
        loc.put(link);
    }
}

//======================================
// Result types
//======================================

impl<'i> ParseResult<CstNode<BorrowedTokenInput<'i>>> {
    pub(crate) fn put(self, link: &mut wstp::Link) {
        let ParseResult {
            nodes: outer_exprs,
            unsafe_character_encoding,
            fatal_issues,
            non_fatal_issues,
            tracked,
        } = self;

        link.put_function(SYMBOL_LIST.as_str(), 6).unwrap();

        // 1.
        // Collected expressions.
        match unsafe_character_encoding {
            None => outer_exprs.put(link),
            Some(flag) => {
                link.put_function(SYMBOL_LIST.as_str(), 1).unwrap();
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

            link.put_function(SYMBOL_LIST.as_str(), issues.len())
                .unwrap();

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

impl MyString {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let MyString(val) = self;

        link.put_str(val).unwrap()
    }
}

// Note: This function can't be a method on Symbol because Symbol (currently) is
//       a type alias to a type from wolfram_expr, and its not legal in Rust
//       to implement methods on types that aren't part of the current crate.
pub(crate) fn Symbol_put(self_: Symbol, callLink: &mut wstp::Link) {
    callLink.put_symbol(self_.as_str()).unwrap();
}

impl Severity {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let string: &'static str = self.as_str();

        link.put_str(string).unwrap();
    }
}

impl IssueTag {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
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
