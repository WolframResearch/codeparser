
#pragma once

#include "SourceManager.h"

#include <string>
#include <sstream>

#define TAG_SYNTAXERROR "SyntaxError"
#define TAG_STRANGECHARACTER "StrangeCharacter"
#define TAG_SYNTAXAMBIGUITY "SyntaxAmbiguity"
#define TAG_NOTCONTIGUOUS "NotContiguous"
#define TAG_MAXEXPRESSIONDEPTH "MaxExpressionDepth"
#define TAG_MAXEXPRESSIONBREADTH "MaxExpressionBreadth"

#define SEVERITY_REMARK "Remark"
#define SEVERITY_WARNING "Warning"
#define SEVERITY_ERROR "Error"
#define SEVERITY_FATAL "Fatal"

struct SyntaxIssue {
    std::string Tag;
    std::string Msg;
    std::string Severity;
    SourceSpan Span;

    SyntaxIssue(std::string Tag, std::string Msg, std::string Severity, SourceSpan Span) : Tag(Tag), Msg(Msg), Severity(Severity), Span(Span) {}

    std::string string();
};
