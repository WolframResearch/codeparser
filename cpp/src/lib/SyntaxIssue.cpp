
#include "Source.h"

#include "Utils.h"

void SyntaxIssue::put(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_SYNTAXISSUE.name(), 4);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tag.c_str()), static_cast<int>(Tag.size()));

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Msg.c_str()), static_cast<int>(Msg.size()));

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Severity.c_str()), static_cast<int>(Severity.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1);

    Span.putSourceRule(mlp);

}

void Comment::put(MLINK mlp) {

    MLPutFunction(mlp, SYMBOL_COMMENT.name(), 2);
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Msg.c_str()), static_cast<int>(Msg.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1);

    Span.putSourceRule(mlp);

}
