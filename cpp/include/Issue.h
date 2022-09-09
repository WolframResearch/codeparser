
#pragma once

#include "Symbol.h"
#include "MyString.h"
#include "Source.h"

#include <string>
#include <vector>

class Issue;
class CodeAction;

using IssuePtr = Issue *;
using CodeActionPtr = CodeAction *;
using IssuePtrVector = std::vector<IssuePtr>;
using CodeActionPtrVector = std::vector<CodeActionPtr>;
using AdditionalDescriptionVector = std::vector<std::string>;


//
//
//
class Issue {
public:
    
    const Symbol MakeSym;
    const MyString Tag;
    const std::string Msg;
    const MyString Sev;
    const Source Src;
    const double Val;
    const CodeActionPtrVector Actions;
    const AdditionalDescriptionVector AdditionalDescriptions;
    
    Issue(Symbol MakeSym, MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    bool syntaxQ() const;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const;
#endif // USE_EXPR_LIB
};

bool operator==(Issue a, Issue b);


//
//
//
class CodeAction {
protected:
    
    const std::string Label;
    const Source Src;
    
public:
    
    CodeAction(std::string Label, Source Src);
    
#if USE_MATHLINK
    virtual void put(ParserSessionPtr session, MLINK callLink) const = 0;
#endif // USE_MATHLINK
    
    virtual void print(std::ostream& s) const = 0;
    
#if USE_EXPR_LIB
    virtual expr toExpr(ParserSessionPtr session) const = 0;
#endif // USE_EXPR_LIB
    
    virtual ~CodeAction() {}
};


//
//
//
class ReplaceTextCodeAction : public CodeAction {
private:
    
    const std::string ReplacementText;
    
public:
    
    ReplaceTextCodeAction(std::string Label, Source Src, std::string ReplacementText) : CodeAction(Label, Src), ReplacementText(ReplacementText) {}
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class InsertTextCodeAction : public CodeAction {
private:
    
    const std::string InsertionText;
    
public:
    
    InsertTextCodeAction(std::string Label, Source Src, std::string InsertionText) : CodeAction(Label, Src), InsertionText(InsertionText) {}
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class DeleteTextCodeAction : public CodeAction {
public:
    
    DeleteTextCodeAction(std::string Label, Source Src) : CodeAction(Label, Src) {}
    
#if USE_MATHLINK
    void put(ParserSessionPtr session, MLINK callLink) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr(ParserSessionPtr session) const override;
#endif // USE_EXPR_LIB
};

//
//
//
class SyntaxIssue : public Issue {
public:
    
    SyntaxIssue(MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
};

//
//
//
class FormatIssue : public Issue {
public:
    
    FormatIssue(MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
};

//
//
//
class EncodingIssue : public Issue {
public:
    
    EncodingIssue(MyString Tag, std::string Msg, MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
};
