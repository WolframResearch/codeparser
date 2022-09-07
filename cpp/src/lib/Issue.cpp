
#include "Issue.h"

#include "MyStringRegistration.h"
#include "SymbolRegistration.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB


Issue::Issue(Symbol MakeSym, MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : MakeSym(MakeSym), Tag(Tag), Msg(Msg), Sev(Sev), Src(Src), Val(Val), Actions(Actions), AdditionalDescriptions(AdditionalDescriptions) {}

void Issue::print(std::ostream& s) const {
    
    MakeSym.print(s);
    s << "[";
    
    Tag.print(s);
    s << ", ";
    
    s << Msg;
    s << ", ";
    
    Sev.print(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << Val;
    s << ", ";
    
    for (auto& A : Actions) {
        A->print(s);
        s << ", ";
    }
    
    for (auto& D : AdditionalDescriptions) {
        s << D.c_str();
        s << ", ";
    }
    
    s << "]";
}

bool Issue::check() const {
    return Sev != STRING_FATAL;
}

bool operator==(Issue a, Issue b) {
    return a.Src == b.Src && a.Tag == b.Tag;
}


SyntaxIssue::SyntaxIssue(MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : Issue(SYMBOL_CODEPARSER_SYNTAXISSUE, Tag, Msg, Sev, Src, Val, Actions, AdditionalDescriptions) {}

FormatIssue::FormatIssue(MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : Issue(SYMBOL_CODEPARSER_FORMATISSUE, Tag, Msg, Sev, Src, Val, Actions, AdditionalDescriptions) {}

EncodingIssue::EncodingIssue(MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : Issue(SYMBOL_CODEPARSER_ENCODINGISSUE, Tag, Msg, Sev, Src, Val, Actions, AdditionalDescriptions) {}


CodeAction::CodeAction(std::string Label, Source Src) : Label(Label), Src(Src) {}


void ReplaceTextCodeAction::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CODEACTION.print(s);
    s << "[";
    
    s << Label;
    s << ", ";
    
    SYMBOL_CODEPARSER_REPLACETEXT.print(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << ReplacementText;
    
    s << "]";
}

void InsertTextCodeAction::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CODEACTION.print(s);
    s << "[";
    
    s << Label;
    s << ", ";
    
    SYMBOL_CODEPARSER_INSERTTEXT.print(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << InsertionText;
    
    s << "]";
}

void DeleteTextCodeAction::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CODEACTION.print(s);
    s << "[";
    
    s << Label;
    s << ", ";
    
    SYMBOL_CODEPARSER_DELETETEXT.print(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}


#if USE_MATHLINK
void Issue::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, MakeSym.Name, 4)) {
        assert(false);
    }
    
    Tag.put(session, callLink);
    
    if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(Msg.c_str()), static_cast<int>(Msg.size()))) {
        assert(false);
    }
    
    Sev.put(session, callLink);
    
    {
        if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 2 + (Actions.empty() ? 0 : 1) + (AdditionalDescriptions.empty() ? 0 : 1))) {
            assert(false);
        }
        
        Src.put(session, callLink);
        
        {
            if (!MLPutFunction(callLink, SYMBOL_RULE.Name, 2)) {
                assert(false);
            }
            
            SYMBOL_CONFIDENCELEVEL.put(session, callLink);
            
            if (!MLPutReal(callLink, Val)) {
                assert(false);
            }
        }
        
        if (!Actions.empty()) {
            
            if (!MLPutFunction(callLink, SYMBOL_RULE.Name, 2)) {
                assert(false);
            }
            
            SYMBOL_CODEPARSER_CODEACTIONS.put(session, callLink);
            
            if (!MLPutFunction(callLink, SYMBOL_LIST.Name, static_cast<int>(Actions.size()))) {
                assert(false);
            }
            
            for (auto& A : Actions) {
                A->put(session, callLink);
            }
        }
        
        if (!AdditionalDescriptions.empty()) {
            
            if (!MLPutFunction(callLink, SYMBOL_RULE.Name, 2)) {
                assert(false);
            }
            
            STRING_ADDITIONALDESCRIPTIONS.put(session, callLink);
            
            if (!MLPutFunction(callLink, SYMBOL_LIST.Name, static_cast<int>(AdditionalDescriptions.size()))) {
                assert(false);
            }
            
            for (auto& D : AdditionalDescriptions) {
                if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(D.c_str()), static_cast<int>(D.size()))) {
                    assert(false);
                }
            }
        }
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void ReplaceTextCodeAction::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_CODEACTION.Name, 3)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_REPLACETEXT.put(session, callLink);
    
    {
        if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 2)) {
            assert(false);
        }
        
        Src.put(session, callLink);
        
        if (!MLPutFunction(callLink, SYMBOL_RULE.Name, 2)) {
            assert(false);
        }
        
        STRING_REPLACEMENTTEXT.put(session, callLink);
        
        if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(ReplacementText.c_str()), static_cast<int>(ReplacementText.size()))) {
            assert(false);
        }
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void InsertTextCodeAction::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_CODEACTION.Name, 3)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_INSERTTEXT.put(session, callLink);
    
    {
        if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 2)) {
            assert(false);
        }
        
        Src.put(session, callLink);
        
        if (!MLPutFunction(callLink, SYMBOL_RULE.Name, 2)) {
            assert(false);
        }
        
        STRING_INSERTIONTEXT.put(session, callLink);
        
        if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(InsertionText.c_str()), static_cast<int>(InsertionText.size()))) {
            assert(false);
        }
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void DeleteTextCodeAction::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_CODEPARSER_CODEACTION.Name, 3)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(callLink, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_DELETETEXT.put(session, callLink);
    
    {
        if (!MLPutFunction(callLink, SYMBOL_ASSOCIATION.Name, 1)) {
            assert(false);
        }
        
        Src.put(session, callLink);
    }
}
#endif // USE_MATHLINK


#if USE_EXPR_LIB
expr Issue::toExpr(ParserSessionPtr session) const {
    
    auto head = MakeSym.toExpr(session);
    
    auto e = Expr_BuildExprA(head, 4);
    
    auto TagExpr = Tag.toExpr(session);
    Expr_InsertA(e, 0 + 1, TagExpr);
    
    auto MsgExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Msg.c_str()), static_cast<int>(Msg.size()));
    Expr_InsertA(e, 1 + 1, MsgExpr);
    
    auto SevExpr = Sev.toExpr(session);
    Expr_InsertA(e, 2 + 1, SevExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 2 + (Actions.empty() ? 0 : 1) + (AdditionalDescriptions.empty() ? 0 : 1));
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        {
            auto head = SYMBOL_RULE.toExpr(session);
                    
            auto ConfidenceLevelRuleExpr = Expr_BuildExprA(head, 2);
            
            auto ConfidenceLevelSymExpr = SYMBOL_CONFIDENCELEVEL.toExpr(session);
            Expr_InsertA(ConfidenceLevelRuleExpr, 0 + 1, ConfidenceLevelSymExpr);
            
            auto ValExpr = Expr_FromReal64(Val);
            Expr_InsertA(ConfidenceLevelRuleExpr, 1 + 1, ValExpr);
            
            Expr_InsertA(DataExpr, 1 + 1, ConfidenceLevelRuleExpr);
        }
        
        int i = 2;
        
        if (!Actions.empty()) {
            
            auto head = SYMBOL_RULE.toExpr(session);
                    
            auto CodeActionsRuleExpr = Expr_BuildExprA(head, 2);
            
            auto CodeActionsSymExpr = SYMBOL_CODEPARSER_CODEACTIONS.toExpr(session);
            Expr_InsertA(CodeActionsRuleExpr, 0 + 1, CodeActionsSymExpr);
            
            {
                auto head = SYMBOL_LIST.toExpr(session);
                        
                auto CodeActionsListExpr = Expr_BuildExprA(head, static_cast<int>(Actions.size()));
                
                int j = 0;
                for (auto& A : Actions) {
                    
                    auto AExpr = A->toExpr(session);
                    
                    Expr_InsertA(CodeActionsListExpr, j + 1, AExpr);
                    
                    j++;
                }
                
                Expr_InsertA(CodeActionsRuleExpr, 1 + 1, CodeActionsListExpr);
            }
            
            Expr_InsertA(DataExpr, i + 1, CodeActionsRuleExpr);
            i++;
        }
        
        if (!AdditionalDescriptions.empty()) {
            
            auto head = SYMBOL_RULE.toExpr(session);
                    
            auto AdditionalDescriptionsRuleExpr = Expr_BuildExprA(head, 2);
            
            auto AdditionalDescriptionsStrExpr = STRING_ADDITIONALDESCRIPTIONS.toExpr(session);
            Expr_InsertA(AdditionalDescriptionsRuleExpr, 0 + 1, AdditionalDescriptionsStrExpr);
            
            {
                auto head = SYMBOL_LIST.toExpr(session);
                        
                auto AdditionalDescriptionsListExpr = Expr_BuildExprA(head, static_cast<int>(AdditionalDescriptions.size()));
                
                int j = 0;
                for (auto& D : AdditionalDescriptions) {
                    
                    auto DExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(D.c_str()), static_cast<int>(D.size()));
                    
                    Expr_InsertA(AdditionalDescriptionsListExpr, j + 1, DExpr);
                    
                    j++;
                }
                
                Expr_InsertA(AdditionalDescriptionsRuleExpr, 1 + 1, AdditionalDescriptionsListExpr);
            }
            
            Expr_InsertA(DataExpr, i + 1, AdditionalDescriptionsRuleExpr);
            i++;
        }
        
        Expr_InsertA(e, 3 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr ReplaceTextCodeAction::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_CODEPARSER_CODEACTION.toExpr(session);
            
    auto e = Expr_BuildExprA(head, 3);
    
    auto LabelExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()));
    Expr_InsertA(e, 0 + 1, LabelExpr);
    
    auto CommandExpr = SYMBOL_CODEPARSER_REPLACETEXT.toExpr(session);
    Expr_InsertA(e, 1 + 1, CommandExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 2);
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        {
            auto head = SYMBOL_RULE.toExpr(session);
                    
            auto ReplacementTextRuleExpr = Expr_BuildExprA(head, 2);
            
            auto ReplacementTextStrExpr = STRING_REPLACEMENTTEXT.toExpr(session);
            Expr_InsertA(ReplacementTextRuleExpr, 0 + 1, ReplacementTextStrExpr);
            
            auto ReplacementTextExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(ReplacementText.c_str()), static_cast<int>(ReplacementText.size()));
            Expr_InsertA(ReplacementTextRuleExpr, 1 + 1, ReplacementTextExpr);
            
            Expr_InsertA(DataExpr, 1 + 1, ReplacementTextRuleExpr);
        }
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr InsertTextCodeAction::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_CODEPARSER_CODEACTION.toExpr(session);
            
    auto e = Expr_BuildExprA(head, 3);
    
    auto LabelExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()));
    Expr_InsertA(e, 0 + 1, LabelExpr);
    
    auto CommandExpr = SYMBOL_CODEPARSER_INSERTTEXT.toExpr(session);
    Expr_InsertA(e, 1 + 1, CommandExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 2);
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        {
            auto head = SYMBOL_RULE.toExpr(session);
                    
            auto InsertionTextRuleExpr = Expr_BuildExprA(head, 2);
            
            auto InsertionTextStrExpr = STRING_INSERTIONTEXT.toExpr(session);
            Expr_InsertA(InsertionTextRuleExpr, 0 + 1, InsertionTextStrExpr);
            
            auto InsertionTextExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(InsertionText.c_str()), static_cast<int>(InsertionText.size()));
            Expr_InsertA(InsertionTextRuleExpr, 1 + 1, InsertionTextExpr);
            
            Expr_InsertA(DataExpr, 1 + 1, InsertionTextRuleExpr);
        }
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr DeleteTextCodeAction::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_CODEPARSER_CODEACTION.toExpr(session);
            
    auto e = Expr_BuildExprA(head, 3);
    
    auto LabelExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()));
    Expr_InsertA(e, 0 + 1, LabelExpr);
    
    auto CommandExpr = SYMBOL_CODEPARSER_DELETETEXT.toExpr(session);
    Expr_InsertA(e, 1 + 1, CommandExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr(session);
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr(session);
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB
