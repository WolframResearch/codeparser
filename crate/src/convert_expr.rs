//======================================
// Token
//======================================

impl Token {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let Token { tok } = self;

        let head: expr;

        if tok.isError() {
            if tok.isUnterminated() {
                head = SYMBOL_CODEPARSER_UNTERMINATEDTOKENERRORNEEDSREPARSENODE.toExpr(session);
            } else {
                head = SYMBOL_CODEPARSER_ERRORNODE.toExpr(session);
            }
        } else {
            //
            // These are Symbols, Strings, Integers, Reals, Rationals.
            //

            head = SYMBOL_CODEPARSER_LEAFNODE.toExpr(session);
        }

        let e = Expr_BuildExprA(head, 3);

        let Sym = TokenToSymbol(Tok);

        let SymExpr = Sym.toExpr(session);
        Expr_InsertA(e, 0 + 1, SymExpr);

        let TokBufLenExpr = bufLen().toExpr(session);
        Expr_InsertA(e, 1 + 1, TokBufLenExpr);

        {
            let head = SYMBOL_ASSOCIATION.toExpr(session);

            let DataExpr = Expr_BuildExprA(head, 1);

            let SrcExpr = Src.toExpr(session);
            Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

            Expr_InsertA(e, 2 + 1, DataExpr);
        }

        return e;
    }
}

//======================================
// Node types
//======================================

struct ToExprVisitor {
    session: ParserSessionPtr,
    // ToExprVisitor(ParserSessionPtr session) : session(session) {};

    // expr operator()(const NodePtr& N) { return N->toExpr(session); }

    // expr operator()(const Token& L) { return L.toExpr(session); }
}

impl NodeSeq {
    fn toExpr(session: &ParserSession) -> expr {
        let head = SYMBOL_LIST.toExpr(session);

        let e = Expr_BuildExprA(head, size_to_int(vec.len()));

        for i in 0..vec.len() {
            if crate::feature::CHECK_ABORT && crate::abortQ() {
                Expr_InsertA(e, i + 1, SYMBOL__ABORTED.toExpr(session));
                continue;
            }

            auto & C = vec[i];

            let CExpr = std::visit(ToExprVisitor { session }, C);

            Expr_InsertA(e, i + 1, CExpr);
        }

        return e;
    }
}

impl OperatorNode {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let head = self.MakeSym.toExpr(session);

        let e = Expr_BuildExprA(head, 3);

        let OpExpr = Op.toExpr(session);
        Expr_InsertA(e, 0 + 1, OpExpr);

        let ChildrenExpr = Children.toExpr(session);
        Expr_InsertA(e, 1 + 1, ChildrenExpr);

        {
            let head = SYMBOL_ASSOCIATION.toExpr(session);

            let DataExpr = Expr_BuildExprA(head, 1);

            let SrcExpr = Src.toExpr(session);
            Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

            Expr_InsertA(e, 2 + 1, DataExpr);
        }

        return e;
    }
}

impl CallNode {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let head = SYMBOL_CODEPARSER_CALLNODE.toExpr(session);

        let e = Expr_BuildExprA(head, 3);

        let HeadExpr = Head.toExpr(session);
        Expr_InsertA(e, 0 + 1, HeadExpr);

        let BodyExpr = std::visit(ToExprVisitor { session }, Body);
        Expr_InsertA(e, 1 + 1, BodyExpr);

        {
            let head = SYMBOL_ASSOCIATION.toExpr(session);

            let DataExpr = Expr_BuildExprA(head, 1);

            let SrcExpr = Src.toExpr(session);
            Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

            Expr_InsertA(e, 2 + 1, DataExpr);
        }

        return e;
    }
}

impl SyntaxErrorNode {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let head = SYMBOL_CODEPARSER_SYNTAXERRORNODE.toExpr(session);

        let e = Expr_BuildExprA(head, 3);

        let SymExpr = Err.toExpr(session);
        Expr_InsertA(e, 0 + 1, SymExpr);

        let ChildrenExpr = Children.toExpr(session);
        Expr_InsertA(e, 1 + 1, ChildrenExpr);

        {
            let head = SYMBOL_ASSOCIATION.toExpr(session);

            let DataExpr = Expr_BuildExprA(head, 1);

            let SrcExpr = Src.toExpr(session);
            Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

            Expr_InsertA(e, 2 + 1, DataExpr);
        }

        return e;
    }
}

impl CollectedExpressionsNode {
    fn toExpr(&self, session: ParserSession) -> expr {
        let head = SYMBOL_LIST.toExpr(session);

        let e = Expr_BuildExprA(head, size_to_int(Exprs.len()));

        for i in 0..Exprs.len() {
            auto & NN = Exprs[i];

            let NExpr = std::visit(ToExprVisitor { session }, NN);

            Expr_InsertA(e, i + 1, NExpr);
        }

        return e;
    }
}

impl CollectedIssuesNode {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let CollectedIssuesNode { issues } = self;

        let head = SYMBOL_LIST.toExpr(session);

        let e = Expr_BuildExprA(head, size_to_int(issues.len()));

        let i = 0;
        for I in issues {
            let IExpr = I.toExpr(session);

            Expr_InsertA(e, i + 1, IExpr);

            i += 1;
        }

        return e;
    }
}

impl CollectedSourceLocationsNode {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let CollectedSourceLocationsNode { source_locs } = self;

        let head = SYMBOL_LIST.toExpr(session);

        let e = Expr_BuildExprA(head, size_to_int(source_locs.len()));

        let i = 0;
        for L in SourceLocs {
            let LExpr = L.toExpr(session);

            Expr_InsertA(e, i + 1, LExpr);

            i += 1;
        }

        return e;
    }
}

impl MissingBecauseUnsafeCharacterEncodingNode {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let head = SYMBOL_MISSING.toExpr(session);

        let e = Expr_BuildExprA(head, 1);

        let reason = unsafeCharacterEncodingReason(flag);

        let StrExpr = reason.toExpr(session);
        Expr_InsertA(e, 0 + 1, StrExpr);

        return e;
    }
}

impl SafeStringNode {
    fn toExpr(&self, session: &ParserSession) -> expr {
        return bufAndLen.toExpr(session);
    }
}

impl NodeContainer {
    fn toExpr(&self, session: &ParserSession) -> expr {
        return Nodes.toExpr(session);
    }
}

//======================================
// Source types
//======================================

impl<'i> BufferAndLength<'i> {
    fn toExpr(&self, session: &ParserSession) -> expr {
        return Expr_UTF8BytesToStringExpr(Buf, self.length() as c_int);
    }
}

impl Issue {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let head = MakeSym.toExpr(session);

        let e = Expr_BuildExprA(head, 4);

        let TagExpr = Tag.toExpr(session);
        Expr_InsertA(e, 0 + 1, TagExpr);

        let MsgExpr = Expr_UTF8BytesToStringExpr((Msg.c_str()), (Msg.len()));
        Expr_InsertA(e, 1 + 1, MsgExpr);

        let SevExpr = Sev.toExpr(session);
        Expr_InsertA(e, 2 + 1, SevExpr);

        {
            let head = SYMBOL_ASSOCIATION.toExpr(session);

            let DataExpr = Expr_BuildExprA(
                head,
                2 + (if Actions.is_empty() { 0 } else { 1 })
                    + (if AdditionalDescriptions.is_empty() {
                        0
                    } else {
                        1
                    }),
            );

            let SrcExpr = Src.toExpr(session);
            Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

            {
                let head = SYMBOL_RULE.toExpr(session);

                let ConfidenceLevelRuleExpr = Expr_BuildExprA(head, 2);

                let ConfidenceLevelSymExpr = SYMBOL_CONFIDENCELEVEL.toExpr(session);
                Expr_InsertA(ConfidenceLevelRuleExpr, 0 + 1, ConfidenceLevelSymExpr);

                let ValExpr = Expr_FromReal64(Val);
                Expr_InsertA(ConfidenceLevelRuleExpr, 1 + 1, ValExpr);

                Expr_InsertA(DataExpr, 1 + 1, ConfidenceLevelRuleExpr);
            }

            let mut i: c_int = 2;

            if !Actions.is_empty() {
                let head = SYMBOL_RULE.toExpr(session);

                let CodeActionsRuleExpr = Expr_BuildExprA(head, 2);

                let CodeActionsSymExpr = SYMBOL_CODEPARSER_CODEACTIONS.toExpr(session);
                Expr_InsertA(CodeActionsRuleExpr, 0 + 1, CodeActionsSymExpr);

                {
                    let head = SYMBOL_LIST.toExpr(session);

                    let CodeActionsListExpr = Expr_BuildExprA(head, (Actions.len()));

                    let mut j: c_int = 0;
                    for A in Actions {
                        let AExpr = A.toExpr(session);

                        Expr_InsertA(CodeActionsListExpr, j + 1, AExpr);

                        j += 1;
                    }

                    Expr_InsertA(CodeActionsRuleExpr, 1 + 1, CodeActionsListExpr);
                }

                Expr_InsertA(DataExpr, i + 1, CodeActionsRuleExpr);
                i += 1;
            }

            if (!AdditionalDescriptions.is_empty()) {
                let head = SYMBOL_RULE.toExpr(session);

                let AdditionalDescriptionsRuleExpr = Expr_BuildExprA(head, 2);

                let AdditionalDescriptionsStrExpr = STRING_ADDITIONALDESCRIPTIONS.toExpr(session);
                Expr_InsertA(
                    AdditionalDescriptionsRuleExpr,
                    0 + 1,
                    AdditionalDescriptionsStrExpr,
                );

                {
                    let head = SYMBOL_LIST.toExpr(session);

                    let AdditionalDescriptionsListExpr =
                        Expr_BuildExprA(head, (AdditionalDescriptions.len()));

                    let j: c_int = 0;
                    for D in AdditionalDescriptions {
                        let DExpr = Expr_UTF8BytesToStringExpr((D.c_str()), (D.len()));

                        Expr_InsertA(AdditionalDescriptionsListExpr, j + 1, DExpr);

                        j += 1;
                    }

                    Expr_InsertA(
                        AdditionalDescriptionsRuleExpr,
                        1 + 1,
                        AdditionalDescriptionsListExpr,
                    );
                }

                Expr_InsertA(DataExpr, i + 1, AdditionalDescriptionsRuleExpr);
                i += 1;
            }

            Expr_InsertA(e, 3 + 1, DataExpr);
        }

        return e;
    }
}

impl CodeAction {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let CodeAction {
            label: Label,
            src: Src,
            kind,
        } = self;

        match kind {
            CodeActionKind::ReplaceText { replacement_text } => {
                let head = SYMBOL_CODEPARSER_CODEACTION.toExpr(session);

                let e = Expr_BuildExprA(head, 3);

                let LabelExpr = Expr_UTF8BytesToStringExpr((Label.c_str()), (Label.len()));
                Expr_InsertA(e, 0 + 1, LabelExpr);

                let CommandExpr = SYMBOL_CODEPARSER_REPLACETEXT.toExpr(session);
                Expr_InsertA(e, 1 + 1, CommandExpr);

                {
                    let head = SYMBOL_ASSOCIATION.toExpr(session);

                    let DataExpr = Expr_BuildExprA(head, 2);

                    let SrcExpr = Src.toExpr(session);
                    Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

                    {
                        let head = SYMBOL_RULE.toExpr(session);

                        let ReplacementTextRuleExpr = Expr_BuildExprA(head, 2);

                        let ReplacementTextStrExpr = STRING_REPLACEMENTTEXT.toExpr(session);
                        Expr_InsertA(ReplacementTextRuleExpr, 0 + 1, ReplacementTextStrExpr);

                        let ReplacementTextExpr = Expr_UTF8BytesToStringExpr(
                            (ReplacementText.c_str()),
                            (ReplacementText.len()),
                        );
                        Expr_InsertA(ReplacementTextRuleExpr, 1 + 1, ReplacementTextExpr);

                        Expr_InsertA(DataExpr, 1 + 1, ReplacementTextRuleExpr);
                    }

                    Expr_InsertA(e, 2 + 1, DataExpr);
                }

                return e;
            },
            CodeActionKind::InsertText { insertion_text } => {
                let head = SYMBOL_CODEPARSER_CODEACTION.toExpr(session);

                let e = Expr_BuildExprA(head, 3);

                let LabelExpr = Expr_UTF8BytesToStringExpr((Label.c_str()), (Label.len()));
                Expr_InsertA(e, 0 + 1, LabelExpr);

                let CommandExpr = SYMBOL_CODEPARSER_INSERTTEXT.toExpr(session);
                Expr_InsertA(e, 1 + 1, CommandExpr);

                {
                    let head = SYMBOL_ASSOCIATION.toExpr(session);

                    let DataExpr = Expr_BuildExprA(head, 2);

                    let SrcExpr = Src.toExpr(session);
                    Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

                    {
                        let head = SYMBOL_RULE.toExpr(session);

                        let InsertionTextRuleExpr = Expr_BuildExprA(head, 2);

                        let InsertionTextStrExpr = STRING_INSERTIONTEXT.toExpr(session);
                        Expr_InsertA(InsertionTextRuleExpr, 0 + 1, InsertionTextStrExpr);

                        let InsertionTextExpr = Expr_UTF8BytesToStringExpr(
                            (InsertionText.c_str()),
                            (InsertionText.len()),
                        );
                        Expr_InsertA(InsertionTextRuleExpr, 1 + 1, InsertionTextExpr);

                        Expr_InsertA(DataExpr, 1 + 1, InsertionTextRuleExpr);
                    }

                    Expr_InsertA(e, 2 + 1, DataExpr);
                }

                return e;
            },
            CodeActionKind::DeleteText => {
                let head = SYMBOL_CODEPARSER_CODEACTION.toExpr(session);

                let e = Expr_BuildExprA(head, 3);

                let LabelExpr = Expr_UTF8BytesToStringExpr((Label.c_str()), (Label.len()));
                Expr_InsertA(e, 0 + 1, LabelExpr);

                let CommandExpr = SYMBOL_CODEPARSER_DELETETEXT.toExpr(session);
                Expr_InsertA(e, 1 + 1, CommandExpr);

                {
                    let head = SYMBOL_ASSOCIATION.toExpr(session);

                    let DataExpr = Expr_BuildExprA(head, 1);

                    let SrcExpr = Src.toExpr(session);
                    Expr_InsertA(DataExpr, 0 + 1, SrcExpr);

                    Expr_InsertA(e, 2 + 1, DataExpr);
                }

                return e;
            },
        }
    }
}

impl SourceLocation {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let head = SYMBOL_LIST.toExpr(session);

        let e = Expr_BuildExprA(head, 2);

        let FirstExpr = Expr_FromInteger64(first);
        Expr_InsertA(e, 0 + 1, FirstExpr);

        let SecondExpr = Expr_FromInteger64(second);
        Expr_InsertA(e, 1 + 1, SecondExpr);

        return e;
    }
}

impl Source {
    fn toExpr(&self, session: &ParserSession) -> expr {
        let head = SYMBOL_RULE.toExpr(session);

        let e = Expr_BuildExprA(head, 2);

        let SourceSymExpr = SYMBOL_CODEPARSER_SOURCE.toExpr(session);
        Expr_InsertA(e, 0 + 1, SourceSymExpr);

        match session.srcConvention {
            SourceConvention::LineColumn => {
                let head = SYMBOL_LIST.toExpr(session);

                let SrcExpr = Expr_BuildExprA(head, 2);

                {
                    let head = SYMBOL_LIST.toExpr(session);

                    let StartExpr = Expr_BuildExprA(head, 2);

                    let StartFirstExpr = Expr_FromInteger64(Start.first);
                    Expr_InsertA(StartExpr, 0 + 1, StartFirstExpr);

                    let StartSecondExpr = Expr_FromInteger64(Start.second);
                    Expr_InsertA(StartExpr, 1 + 1, StartSecondExpr);

                    Expr_InsertA(SrcExpr, 0 + 1, StartExpr);
                }

                {
                    let head = SYMBOL_LIST.toExpr(session);

                    let EndExpr = Expr_BuildExprA(head, 2);

                    let EndFirstExpr = Expr_FromInteger64(End.first);
                    Expr_InsertA(EndExpr, 0 + 1, EndFirstExpr);

                    let EndSecondExpr = Expr_FromInteger64(End.second);
                    Expr_InsertA(EndExpr, 1 + 1, EndSecondExpr);

                    Expr_InsertA(SrcExpr, 1 + 1, EndExpr);
                }

                Expr_InsertA(e, 1 + 1, SrcExpr);

                return e;
            },
            SourceConvention::CharacterIndex => {
                let head = SYMBOL_LIST.toExpr(session);

                let SrcExpr = Expr_BuildExprA(head, 2);

                let StartSecondExpr = Expr_FromInteger64(Start.second);
                Expr_InsertA(SrcExpr, 0 + 1, StartSecondExpr);

                let EndSecondExpr = Expr_FromInteger64(End.second - 1);
                Expr_InsertA(SrcExpr, 1 + 1, EndSecondExpr);

                Expr_InsertA(e, 1 + 1, SrcExpr);

                return e;
            },
        }

        assert!(false);

        return nullptr;
    }
}

//======================================
// Other
//======================================

impl Symbol {
    fn toExpr(&self, session: &ParserSession) -> expr {
        return Expr_MEncodedStringToSymbolExpr(Name);
    }
}
