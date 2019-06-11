
#include "Source.h"

#include "Utils.h"

#include <iostream>

void SyntaxIssue::put(MLINK mlp) const {

    MLPutFunction(mlp, SYMBOL_SYNTAXISSUE->name(), 4);

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tag.c_str()), static_cast<int>(Tag.size()));

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Msg.c_str()), static_cast<int>(Msg.size()));

    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Severity.c_str()), static_cast<int>(Severity.size()));

    MLPutFunction(mlp, SYMBOL_ASSOCIATION->name(), 1);

    Span.putSourceRule(mlp);
}

void Metadata::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_METADATA->name(), 2);
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Key.c_str()), static_cast<int>(Key.size()));
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Val.c_str()), static_cast<int>(Val.size()));
}

std::string SyntaxErrorToString(SyntaxError Err) {
    switch (Err) {
        case SYNTAXERROR_UNKNOWN: return "SyntaxError`Unknown";
        case SYNTAXERROR_EXPECTEDTILDE: return "SyntaxError`ExpectedTilde";
        case SYNTAXERROR_EXPECTEDSET: return "SyntaxError`ExpectedSet";
        case SYNTAXERROR_EXPECTEDSYMBOL: return "SyntaxError`ExpectedSymbol";
        case SYNTAXERROR_EXPECTEDOPERAND: return "SyntaxError`ExpectedOperand";
        case SYNTAXERROR_EXPECTEDPOSSIBLEEXPRESSION: return "SyntaxError`ExpectedPossibleExpression";
        case SYNTAXERROR_NONASSOCIATIVE: return "SyntaxError`NonAssociative";
        case SYNTAXERROR_TOKEN_EXPECTEDEQUAL: return "SyntaxError`ExpectedEqual";
        case SYNTAXERROR_TOKEN_UNHANDLEDCHARACTER: return "SyntaxError`UnhandledCharacter";
        case SYNTAXERROR_TOKEN_EXPECTEDDIGITORALPHA: return "SyntaxError`ExpectedDigitOrAlpha";
        case SYNTAXERROR_TOKEN_EXPECTEDLETTERLIKE: return "SyntaxError`ExpectedLetterlike";
        case SYNTAXERROR_TOKEN_UNTERMINATEDCOMMENT: return "SyntaxError`UnterminatedComment";
        case SYNTAXERROR_TOKEN_UNTERMINATEDSTRING: return "SyntaxError`UnterminatedString";
        case SYNTAXERROR_TOKEN_INVALIDBASE: return "SyntaxError`InvalidBase";
        case SYNTAXERROR_TOKEN_EXPECTEDACCURACY: return "SyntaxError`ExpectedAccuracy";
        case SYNTAXERROR_TOKEN_EXPECTEDEXPONENT: return "SyntaxError`ExpectedExponent";
        case SYNTAXERROR_TOKEN_EMPTYSTRING: return "SyntaxError`EmptyString";
        default:
            assert(false);
            return "";
    }
}

SyntaxError TokenErrorToSyntaxError(TokenEnum Tok) {
    switch (Tok) {
        case TOKEN_ERROR_EXPECTEDEQUAL: return SYNTAXERROR_TOKEN_EXPECTEDEQUAL;
        case TOKEN_ERROR_UNHANDLEDCHARACTER: return SYNTAXERROR_TOKEN_UNHANDLEDCHARACTER;
        case TOKEN_ERROR_EXPECTEDDIGITORALPHA: return SYNTAXERROR_TOKEN_EXPECTEDDIGITORALPHA;
        case TOKEN_ERROR_EXPECTEDLETTERLIKE: return SYNTAXERROR_TOKEN_EXPECTEDLETTERLIKE;
        case TOKEN_ERROR_UNTERMINATEDCOMMENT: return SYNTAXERROR_TOKEN_UNTERMINATEDCOMMENT;
        case TOKEN_ERROR_UNTERMINATEDSTRING: return SYNTAXERROR_TOKEN_UNTERMINATEDSTRING;
        case TOKEN_ERROR_INVALIDBASE: return SYNTAXERROR_TOKEN_INVALIDBASE;
        case TOKEN_ERROR_EXPECTEDACCURACY: return SYNTAXERROR_TOKEN_EXPECTEDACCURACY;
        case TOKEN_ERROR_EXPECTEDEXPONENT: return SYNTAXERROR_TOKEN_EXPECTEDEXPONENT;
        case TOKEN_ERROR_EMPTYSTRING: return SYNTAXERROR_TOKEN_EMPTYSTRING;
        default:
            assert(false);
            return SYNTAXERROR_UNKNOWN;
    }
}
