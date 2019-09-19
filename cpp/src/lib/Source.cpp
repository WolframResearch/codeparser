
//
// Characters:
// Manage the start and end of WL characters like \[Alpha] with setWLCharacterStart() and setWLCharacterEnd()
//
// Tokens:
// Manage the start and end of tokens like @@@ with setTokenStart() and setTokenEnd()
// Linear syntax like \! and \( is a single token
//

#include "Symbol.h"
#include "ByteEncoder.h"
#include "Utils.h"
#include "Source.h"
#include "CodePoint.h"

#include <sstream>
#include <cctype> // for isalnum, isxdigit, isupper, isdigit, isalpha, ispunct, iscntrl with GCC and MSVC

bool containsOnlyASCII(std::string s);


//
// SyntaxIssue
//

void SyntaxIssue::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKESYNTAXISSUE->name(), SYNTAXISSUE_LENGTH);
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tag.c_str()), static_cast<int>(Tag.size()));
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Msg.c_str()), static_cast<int>(Msg.size()));
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Severity.c_str()), static_cast<int>(Severity.size()));
    
    Span.putLineCols(mlp);
}


//
// Metadata
//

void Metadata::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_METADATA->name(), 2);
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Key.c_str()), static_cast<int>(Key.size()));
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Val.c_str()), static_cast<int>(Val.size()));
}


//
// SyntaxError
//

std::string SyntaxErrorToString(SyntaxError Err) {
    switch (Err) {
        case SYNTAXERROR_UNKNOWN: return "SyntaxError`Unknown";
        case SYNTAXERROR_EXPECTEDTILDE: return "SyntaxError`ExpectedTilde";
        case SYNTAXERROR_EXPECTEDSET: return "SyntaxError`ExpectedSet";
        case SYNTAXERROR_COLONERROR: return "SyntaxError`ColonError";
        case SYNTAXERROR_EXPECTEDOPERAND: return "SyntaxError`ExpectedOperand";
        case SYNTAXERROR_EXPECTEDPOSSIBLEEXPRESSION: return "SyntaxError`ExpectedPossibleExpression";
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
        case SYNTAXERROR_TOKEN_UNHANDLEDDOT: return "SyntaxError`UnhandledDot";
        default:
            assert(false);
            return "";
    }
}

SyntaxError TokenErrorToSyntaxError(TokenEnum Tok) {
    switch (Tok) {
        case TOKEN_ERROR_EXPECTEDEQUAL: return SYNTAXERROR_TOKEN_EXPECTEDEQUAL;
        case TOKEN_ERROR_UNHANDLEDCHARACTER: return SYNTAXERROR_TOKEN_UNHANDLEDCHARACTER;
        case TOKEN_ERROR_EXPECTEDLETTERLIKE: return SYNTAXERROR_TOKEN_EXPECTEDLETTERLIKE;
        case TOKEN_ERROR_UNTERMINATEDCOMMENT: return SYNTAXERROR_TOKEN_UNTERMINATEDCOMMENT;
        case TOKEN_ERROR_UNTERMINATEDSTRING: return SYNTAXERROR_TOKEN_UNTERMINATEDSTRING;
        case TOKEN_ERROR_INVALIDBASE: return SYNTAXERROR_TOKEN_INVALIDBASE;
        case TOKEN_ERROR_EXPECTEDACCURACY: return SYNTAXERROR_TOKEN_EXPECTEDACCURACY;
        case TOKEN_ERROR_EXPECTEDEXPONENT: return SYNTAXERROR_TOKEN_EXPECTEDEXPONENT;
        case TOKEN_ERROR_EMPTYSTRING: return SYNTAXERROR_TOKEN_EMPTYSTRING;
        case TOKEN_ERROR_UNHANDLEDDOT: return SYNTAXERROR_TOKEN_UNHANDLEDDOT;
        default:
            assert(false);
            return SYNTAXERROR_UNKNOWN;
    }
}


//
// SourceLocation
//

SourceLocation::SourceLocation() : Line(0), Col(0) {}

SourceLocation::SourceLocation(size_t Line, size_t Col) : Line(Line), Col(Col) {}

bool isContiguous(SourceLocation a, SourceLocation b) {
    return a.Line == b.Line && a.Col + 1 == b.Col;
}

bool operator<=(SourceLocation a, SourceLocation b) {
    
    if (a.Line < b.Line) {
        return true;
    }
    
    if (a.Line > b.Line) {
        return false;
    }
    
    assert(a.Line == b.Line);
    
    if (a.Col <= b.Col) {
        return true;
    }
    
    return false;
}


//
// Source
//

bool isContiguous(Source a, Source b) {
    return isContiguous(a.lines.end, b.lines.start);
}

Source::Source() : lines{SourceLocation(), SourceLocation()} {}

Source::Source(SourceLocation loc) : lines{loc, loc} {}

Source::Source(SourceLocation start, SourceLocation end) : lines{start, end} {
    
    //
    // This is a really good way to uncover problems
    //
    assert( start <= end );
}

size_t Source::size() const {
    assert(lines.start.Line == lines.end.Line);
    return lines.end.Col - lines.start.Col + 1;
}

void Source::putLineCols(MLINK mlp) const {
    
    assert(lines.start <= lines.end);
    
    MLPutInteger(mlp, static_cast<int>(lines.start.Line));
    MLPutInteger(mlp, static_cast<int>(lines.start.Col));
    MLPutInteger(mlp, static_cast<int>(lines.end.Line));
    MLPutInteger(mlp, static_cast<int>(lines.end.Col));
}


//
// SourceCharacter
//

bool SourceCharacter::isDigitOrAlpha() const {
    if (!(0x00 <= valBits && valBits <= 0x7f)) {
        return false;
    }
    return std::isalnum(valBits);
}

bool SourceCharacter::isHex() const {
    if (!(0x00 <= valBits && valBits <= 0x7f)) {
        return false;
    }
    return std::isxdigit(valBits);
}

bool SourceCharacter::isOctal() const {
    if (!(0x00 <= valBits && valBits <= 0x7f)) {
        return false;
    }
    return '0' <= valBits && valBits <= '7';
}

bool SourceCharacter::isUpper() const {
    if (!(0x00 <= valBits && valBits <= 0x7f)) {
        return false;
    }
    return std::isupper(valBits);
}

bool SourceCharacter::isEndOfFile() const {
    return valBits == EOF;
}

std::string SourceCharacter::string() const {
    
    std::ostringstream String;
    
    String << *this;
    
    return String.str();
}

std::ostream& operator<<(std::ostream& stream, const SourceCharacter c) {
    
    if (c.isEndOfFile()) {
        return stream;
    }
    
    auto val = c.to_point();
    
    assert(val != CODEPOINT_ERROR_INTERNAL);
    
//    if (val == CODEPOINT_NAKED_BACKSLASH) {
//        stream << '\\';
//        return stream;
//    }
    
    ByteEncoder::encodeBytes(stream, val);
    
    return stream;
}


//
// Token
//

Token::Token(TokenEnum Tok, std::string Str, Source Span) : Tok(Tok), Str(Str), Span(Span) {
    
    switch (Tok) {
            //
            // These are the tokens that do not quite have correct spans.
            // start and end are set to the same character, so size is 1
            // But they take up 0 characters
            //
        case TOKEN_ENDOFFILE:
        case TOKEN_UNKNOWN:
        case TOKEN_FAKE_IMPLICITTIMES:
        case TOKEN_ERROR_EMPTYSTRING:
        case TOKEN_ERROR_ABORTED:
        case TOKEN_FAKE_IMPLICITNULL:
        case TOKEN_FAKE_IMPLICITONE:
        case TOKEN_FAKE_IMPLICITALL:
            assert(Span.lines.start.Line == Span.lines.end.Line);
            assert(Span.lines.start.Col == Span.lines.end.Col);
            break;
        default:
            //
            // This is all just to do an assert.
            // But it's a good assert because it catches problems.
            //
            // Only bother checking if the token is all on one line
            // Spanning multiple lines is too complicated to care about
            //
            if (Span.lines.start.Line == Span.lines.end.Line) {
                if (Span.size() != Str.size()) {
                    //
                    // If the sizes do not match, then check if there are multi-byte characters
                    // If there are multi-bytes characters, then it is too complicated to compare sizes
                    //
                    // Note that this also catches changes in character representation, e.g.,
                    // If a character was in source with \XXX notation but was stringified with \:XXXX notation
                    //
                    assert(!containsOnlyASCII(Str));
                }
            }
            break;
    }
    
}


bool containsOnlyASCII(std::string s) {
    for (auto c : s) {
        //
        // Take care to cast to int before comparing
        //
        if ((static_cast<int>(c) & 0xff) >= 0x80) {
            return false;
        }
    }
    return true;
}

