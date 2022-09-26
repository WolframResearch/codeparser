use wolfram_library_link::wstp;

use crate::{
    my_string::MyString,
    my_string_registration::*,
    node::{
        unsafeCharacterEncodingReason, BinaryNode, CallNode, CollectedExpressionsNode,
        CollectedIssuesNode, CollectedSourceLocationsNode, CompoundNode, GroupMissingCloserNode,
        GroupNode, InfixNode, MissingBecauseUnsafeCharacterEncodingNode, Node, NodeContainer,
        NodeSeq, OperatorNode, PostfixNode, PrefixBinaryNode, PrefixNode, SafeStringNode,
        SyntaxErrorNode, TernaryNode, UnterminatedGroupNeedsReparseNode,
    },
    source::{
        BufferAndLength, CodeAction, CodeActionKind, Issue, Source, SourceConvention,
        SourceLocation,
    },
    symbol::Symbol,
    symbol_registration::*,
    token::{BorrowedTokenInput, Token},
    token_enum_registration::TokenToSymbol,
};

//======================================
// Token
//======================================

impl<'i> Token<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let Token { tok, src, input } = self;

        if tok.isError() {
            if tok.isUnterminated() {
                callLink
                    .put_function(
                        SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.name,
                        3,
                    )
                    .unwrap();
            } else {
                callLink
                    .put_function(SYMBOL_CODEPARSER_ERRORNODE.name, 3)
                    .unwrap();
            }
        } else {
            //
            // These are Symbols, Strings, Integers, Reals, Rationals.
            //

            callLink
                .put_function(SYMBOL_CODEPARSER_LEAFNODE.name, 3)
                .unwrap();
        }

        let sym = TokenToSymbol(*tok);

        sym.put(callLink);

        // bufLen().put(callLink);
        // let source: &[u8] = &session.tokenizer.input[span.offset..span.offset + span.len];
        let source: &[u8] = &input.buf.as_bytes();

        let source = std::str::from_utf8(source).expect("token source span is not valid UTF-8");
        callLink.put_str(source).unwrap();

        callLink.put_function(SYMBOL_ASSOCIATION.name, 1).unwrap();

        src.put(callLink);
    }
}

//======================================
// Node types
//======================================

impl<'i> Node<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        match self {
            Node::Token(token) => token.put(link),
            Node::Call(node) => node.put(link),
            Node::SyntaxError(node) => node.put(link),
            Node::CollectedExpressions(node) => node.put(link),
            Node::CollectedSourceLocations(node) => node.put(link),
            Node::CollectedIssues(node) => node.put(link),
            Node::MissingBecauseUnsafeCharacterEncoding(node) => node.put(link),
            Node::SafeString(node) => node.put(link),
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
            Node::UnterminatedGroupNeedsReparse(UnterminatedGroupNeedsReparseNode(op)) => {
                op.put(link, SYMBOL_CODEPARSER_UNTERMINATEDGROUPNEEDSREPARSENODE)
            },
        }
    }
}

impl<'i> NodeSeq<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let NodeSeq(vec) = self;

        callLink.put_function(SYMBOL_LIST.name, vec.len()).unwrap();

        for C in vec {
            if crate::feature::CHECK_ABORT && crate::abortQ() {
                SYMBOL__ABORTED.put(callLink);
                continue;
            }

            C.put(callLink)
        }
    }
}

impl<'i> OperatorNode<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link, op_head: Symbol) {
        let OperatorNode { op, children, src } = self;

        callLink.put_function(op_head.name, 3).unwrap();

        op.to_symbol().put(callLink);

        children.put(callLink);

        callLink.put_function(SYMBOL_ASSOCIATION.name, 1).unwrap();

        src.put(callLink);
    }
}

impl<'i> CallNode<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CallNode { head, body, src } = self;
        callLink
            .put_function(SYMBOL_CODEPARSER_CALLNODE.name, 3)
            .unwrap();

        head.put(callLink);

        body.put(callLink);

        callLink.put_function(SYMBOL_ASSOCIATION.name, 1).unwrap();

        src.put(callLink);
    }
}

impl<'i> SyntaxErrorNode<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let SyntaxErrorNode { err, children, src } = self;

        callLink
            .put_function(SYMBOL_CODEPARSER_SYNTAXERRORNODE.name, 3)
            .unwrap();

        err.put(callLink);

        children.put(callLink);

        callLink.put_function(SYMBOL_ASSOCIATION.name, 1).unwrap();

        src.put(callLink);
    }
}

impl<'i> CollectedExpressionsNode<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CollectedExpressionsNode { exprs } = self;

        exprs.put(callLink);
    }
}

impl CollectedIssuesNode {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CollectedIssuesNode(issues) = self;

        callLink
            .put_function(SYMBOL_LIST.name, issues.len())
            .unwrap();

        for issue in issues {
            issue.put(callLink);
        }
    }
}

impl CollectedSourceLocationsNode {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let CollectedSourceLocationsNode { source_locs } = self;

        callLink
            .put_function(SYMBOL_LIST.name, source_locs.len())
            .unwrap();

        for loc in source_locs {
            loc.put(callLink);
        }
    }
}

impl MissingBecauseUnsafeCharacterEncodingNode {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let MissingBecauseUnsafeCharacterEncodingNode { flag } = *self;

        callLink.put_function(SYMBOL_MISSING.name, 1).unwrap();

        let reason = unsafeCharacterEncodingReason(flag);

        reason.put(callLink);
    }
}

impl SafeStringNode {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let SafeStringNode { bufAndLen } = self;

        // bufAndLen.put(callLink);

        callLink.put_str(bufAndLen).unwrap();
    }
}

impl<'i> NodeContainer<BorrowedTokenInput<'i>> {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let NodeContainer { nodes } = self;

        nodes.put(callLink);
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

        callLink.put_function(make_sym.name, 4).unwrap();

        tag.put(callLink);

        callLink.put_str(msg).unwrap();

        sev.put(callLink);

        {
            callLink
                .put_function(
                    SYMBOL_ASSOCIATION.name,
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
                callLink.put_function(SYMBOL_RULE.name, 2).unwrap();

                SYMBOL_CONFIDENCELEVEL.put(callLink);

                callLink.put_f64(**val).unwrap();
            }

            if !actions.is_empty() {
                callLink.put_function(SYMBOL_RULE.name, 2).unwrap();

                SYMBOL_CODEPARSER_CODEACTIONS.put(callLink);

                callLink
                    .put_function(SYMBOL_LIST.name, actions.len())
                    .unwrap();

                for A in actions {
                    A.put(callLink);
                }
            }

            if !additional_descriptions.is_empty() {
                callLink.put_function(SYMBOL_RULE.name, 2).unwrap();

                STRING_ADDITIONALDESCRIPTIONS.put(callLink);

                callLink
                    .put_function(SYMBOL_LIST.name, additional_descriptions.len())
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
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.name, 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                SYMBOL_CODEPARSER_REPLACETEXT.put(callLink);

                {
                    callLink.put_function(SYMBOL_ASSOCIATION.name, 2).unwrap();

                    Src.put(callLink);

                    callLink.put_function(SYMBOL_RULE.name, 2).unwrap();

                    STRING_REPLACEMENTTEXT.put(callLink);

                    callLink.put_str(replacement_text).unwrap();
                }
            },
            CodeActionKind::InsertText { insertion_text } => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.name, 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                SYMBOL_CODEPARSER_INSERTTEXT.put(callLink);

                {
                    callLink.put_function(SYMBOL_ASSOCIATION.name, 2).unwrap();

                    Src.put(callLink);

                    callLink.put_function(SYMBOL_RULE.name, 2).unwrap();

                    STRING_INSERTIONTEXT.put(callLink);

                    callLink.put_str(insertion_text).unwrap();
                }
            },
            CodeActionKind::DeleteText => {
                callLink
                    .put_function(SYMBOL_CODEPARSER_CODEACTION.name, 3)
                    .unwrap();

                callLink.put_str(Label).unwrap();

                SYMBOL_CODEPARSER_DELETETEXT.put(callLink);

                {
                    callLink.put_function(SYMBOL_ASSOCIATION.name, 1).unwrap();

                    Src.put(callLink);
                }
            },
        }
    }
}

impl SourceLocation {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let SourceLocation { first, second } = *self;

        callLink.put_function(SYMBOL_LIST.name, 2).unwrap();

        callLink.put_i64(first.into()).unwrap();

        callLink.put_i64(second.into()).unwrap();
    }
}

impl Source {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let Source { start, end } = self;

        callLink.put_function(SYMBOL_RULE.name, 2).unwrap();

        SYMBOL_CODEPARSER_SOURCE.put(callLink);

        debug_assert_eq!(start.convention(), end.convention());

        match start.convention() {
            SourceConvention::LineColumn => {
                callLink.put_function(SYMBOL_LIST.name, 2).unwrap();

                start.put(callLink);
                end.put(callLink);
            },
            SourceConvention::CharacterIndex => {
                callLink.put_function(SYMBOL_LIST.name, 2).unwrap();

                callLink.put_i64(start.second.into()).unwrap();

                callLink.put_i64((end.second - 1).into()).unwrap();

                return;
            },
        }
    }
}

//======================================
// Other
//======================================

impl MyString {
    pub(crate) fn put(&self, link: &mut wstp::Link) {
        let MyString { val, id: _ } = self;

        link.put_str(val).unwrap()
    }
}

impl Symbol {
    pub(crate) fn put(&self, callLink: &mut wstp::Link) {
        let Symbol { name, id: _ } = self;

        callLink.put_symbol(name).unwrap();
    }
}
