
#pragma once

#include "SourceManager.h"
#include "Symbol.h"

#include <string>
#include <sstream>

#define SEVERITY_REMARK "Remark"
#define SEVERITY_WARNING "Warning"
#define SEVERITY_ERROR "Error"
#define SEVERITY_FATAL "Fatal"

struct SyntaxIssue {
    std::string Msg;
    std::string Severity;
    SourceSpan Span;

    SyntaxIssue(std::string Msg, std::string Severity, SourceSpan Span) : Msg(Msg), Severity(Severity), Span(Span) {}

    std::string string() {
        std::ostringstream ss;
        ss << SYMBOL_SYNTAXISSUE.name();
        ss << "[";
        ss << stringEscape(Msg);
        ss << ", ";
        ss << stringEscape(Severity);
        ss << ",<|";
        ss << SYMBOL_SOURCE.name();
        ss << "->{";
        ss << Span.start.string();
        ss << ", ";
        ss << Span.end.string();
        ss << "}";
        ss << "|>";
        ss << "]";
    	return ss.str();
    }

};
