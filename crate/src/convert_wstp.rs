use std::collections::HashSet;

use wolfram_library_link::{expr::Expr, wstp};

use crate::{
    from_expr::List,
    my_string::MyString,
    my_string_registration::*,
    node::{
        BinaryNode, BoxNode, CallNode, CodeNode, CompoundNode, GroupMissingCloserNode, GroupNode,
        InfixNode, Node, NodeSeq, OperatorNode, PostfixNode, PrefixBinaryNode, PrefixNode,
        SyntaxErrorNode, TernaryNode, UnterminatedGroupNode,
    },
    source::{
        BufferAndLength, CharacterRange, CodeAction, CodeActionKind, GeneralSource, Issue,
        IssueTag, Severity, Source, SourceLocation, StringSourceKind,
    },
    symbol::Symbol,
    symbol_registration::*,
    token::{BorrowedTokenInput, Token, TokenInput},
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

// TODO(cleanup): Combine with impl Container<Source>
impl Container<GeneralSource> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
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

impl Container<Source> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
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

        if let Some(file_name) = file_name {
            link.put_function("System`Rule", 2).unwrap();
            link.put_str("FileName").unwrap();
            link.put_expr(file_name).unwrap();
        }
    }
}

//======================================
// Token
//======================================

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

        let sym = TokenToSymbol(*tok);

        Symbol_put(sym, callLink);

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

impl<I: TokenInput, S: WstpPut> Node<I, S> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
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
            Node::UnterminatedGroup(UnterminatedGroupNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNODE)
            },
            Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_GROUPMISSINGCLOSERNODE)
            },
            Node::Box(box_node) => box_node.put(link),
            Node::Code(node) => node.put(link),
        }
    }
}

impl<I: TokenInput, S: WstpPut> NodeSeq<I, S> {
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

        let name = format!("System`{}Box", kind.as_str());

        link.put_function(SYMBOL_CODEPARSER_BOXNODE.as_str(), 3)
            .unwrap();
        link.put_symbol(&name).unwrap();
        children.put(link);
        src.put(link);
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

        Symbol_put(op.to_symbol(), callLink);

        children.put(callLink);

        src.put(callLink);
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

        let err: Symbol = err.to_symbol();

        callLink
            .put_function(SYMBOL_CODEPARSER_SYNTAXERRORNODE.as_str(), 3)
            .unwrap();

        Symbol_put(err, callLink);

        children.put(callLink);

        src.put(callLink);
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
        } = self;

        callLink.put_function(make_sym.as_str(), 4).unwrap();

        tag.put(callLink);

        callLink.put_str(msg).unwrap();

        sev.put(callLink);

        {
            callLink
                .put_function(
                    SYMBOL_ASSOCIATION.as_str(),
                    2 + (if actions.is_empty() { 0 } else { 1 })
                        + (if additional_descriptions.is_empty() {
                            0
                        } else {
                            1
                        }),
                )
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

impl<'i> ParseResult<BorrowedTokenInput<'i>> {
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
