
#pragma once

#include "SourceManager.h"

#include "mathlink.h"

#include <string>

#define SYNTAXISSUETAG_SYNTAXERROR std::string("SyntaxError")
#define SYNTAXISSUETAG_STRANGECHARACTER std::string("StrangeCharacter")
#define SYNTAXISSUETAG_SYNTAXAMBIGUITY std::string("SyntaxAmbiguity")
#define SYNTAXISSUETAG_NOTCONTIGUOUS std::string("NotContiguous")
#define SYNTAXISSUETAG_MAXEXPRESSIONDEPTH std::string("MaxExpressionDepth")
#define SYNTAXISSUETAG_MAXEXPRESSIONBREADTH std::string("MaxExpressionBreadth")

//
// Used to be just SEVERITY_ERROR, etc.,
// but this was observed:
// c:\users\brenton\dropbox\wolfram\ast\ast\cpp\include\SyntaxIssue.h(19): warning C4005: 'SEVERITY_ERROR': macro redefinition
// C:\Program Files (x86)\Windows Kits\10\include\10.0.17763.0\shared\winerror.h(28563): note: see previous definition of 'SEVERITY_ERROR'
//
#define SYNTAXISSUESEVERITY_REMARK std::string("Remark")
#define SYNTAXISSUESEVERITY_WARNING std::string("Warning")
#define SYNTAXISSUESEVERITY_ERROR std::string("Error")
#define SYNTAXISSUESEVERITY_FATAL std::string("Fatal")

struct SyntaxIssue {
    std::string Tag;
    std::string Msg;
    std::string Severity;
    SourceSpan Span;

    SyntaxIssue(std::string Tag, std::string Msg, std::string Severity, SourceSpan Span) : Tag(Tag), Msg(Msg), Severity(Severity), Span(Span) {}

    void put(MLINK mlp);
};

struct Comment {
	std::string Msg;
	SourceSpan Span;

	Comment(std::string Msg, SourceSpan Span) : Msg(Msg), Span(Span) {}

	void put(MLINK mlp);
};
