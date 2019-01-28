
#include "SyntaxIssue.h"

#include "Symbol.h"
#include "Utils.h"

std::string SyntaxIssue::string() {
    std::ostringstream ss;
    ss << SYMBOL_SYNTAXISSUE.name();
    ss << "[";
    ss << stringEscape(Tag);
    ss << ", ";
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
