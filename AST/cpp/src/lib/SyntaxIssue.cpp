
#include "SyntaxIssue.h"

#include "Utils.h"

void SyntaxIssue::put(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_SYNTAXISSUE.name(), 4);

    auto escapedTag = stringEscape(Tag);
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(escapedTag.c_str()), escapedTag.size());

    auto escapedMsg = stringEscape(Msg);
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(escapedMsg.c_str()), escapedMsg.size());

    auto escapedSeverity = stringEscape(Severity);
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(escapedSeverity.c_str()), escapedSeverity.size());

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1);

    Span.putSourceRule(mlp);

}

void Comment::put(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_COMMENT.name(), 2);

    auto escapedMsg = stringEscape(Msg);
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(escapedMsg.c_str()), escapedMsg.size());

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1);

    Span.putSourceRule(mlp);

}
