
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
#include <utility> // for swap

//
// SyntaxIssue
//

void SyntaxIssue::put(MLINK mlp) const {
    
    MLPutFunction(mlp, SYMBOL_AST_LIBRARY_MAKESYNTAXISSUE->name(), 3 + Src.count());
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Tag.c_str()), static_cast<int>(Tag.size()));
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Msg.c_str()), static_cast<int>(Msg.size()));
    
    MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(Sev.c_str()), static_cast<int>(Sev.size()));
    
    Src.put(mlp);
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
// LineCol
//

LineCol::LineCol() : Line(1), Col(0) {}

LineCol::LineCol(size_t Line, size_t Col) : Line(Line), Col(Col) {}

bool isContiguous(LineCol a, LineCol b) {
    return a.Line == b.Line && a.Col + 1 == b.Col;
}

bool operator==(LineCol a, LineCol b) {
    return (a.Line == b.Line) && (a.Col == b.Col);
}

bool operator<=(LineCol a, LineCol b) {
    
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



Offset::Offset(size_t offset) : val(offset) {}

Offset operator-(Offset a, Offset b) {
    return Offset(a.val - b.val);
}

bool operator==(Offset a, Offset b) {
    return a.val == b.val;
}

bool operator<=(Offset a, Offset b) {
    return a.val <= b.val;
}




SourceLocation::SourceLocation() : style(SOURCESTYLE_UNKNOWN) {}

SourceLocation::SourceLocation(SourceStyle style) : style(style) {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            break;
        case SOURCESTYLE_LINECOL:
            lineCol = LineCol();
            break;
        case SOURCESTYLE_OFFSETLEN:
            offset = 0;
            break;
        default:
            assert(false);
    }
}

SourceLocation::SourceLocation(LineCol lineCol) : style(SOURCESTYLE_LINECOL), lineCol(lineCol) {}

SourceLocation::SourceLocation(Offset offset) : style(SOURCESTYLE_OFFSETLEN), offset(offset) {}

SourceLocation SourceLocation::operator+(int d) {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            return SourceLocation();
        case SOURCESTYLE_LINECOL:
            return SourceLocation(LineCol(lineCol.Line, lineCol.Col + d));
        default:
            assert(false);
            return *this;
    }
}

SourceLocation SourceLocation::operator-(int d) {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            return SourceLocation();
        case SOURCESTYLE_LINECOL:
            return SourceLocation(LineCol(lineCol.Line, lineCol.Col - d));
        default:
            assert(false);
            return *this;
    }
}

void SourceLocation::operator++(int ignored) {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            break;
        case SOURCESTYLE_LINECOL:
            lineCol.Col++;
            break;
        case SOURCESTYLE_OFFSETLEN:
            offset++;
            break;
        default:
            assert(false);
            break;
    }
}

SourceLocation SourceLocation::nextLine() {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            return SourceLocation();
        case SOURCESTYLE_LINECOL:
            return SourceLocation(LineCol(lineCol.Line+1, 0));
        case SOURCESTYLE_OFFSETLEN:
            return SourceLocation(offset + 1);
        default:
            assert(false);
            break;
    }
}

bool operator<=(SourceLocation a, SourceLocation b) {
    switch (a.style) {
        case SOURCESTYLE_UNKNOWN:
            assert(b.style == SOURCESTYLE_UNKNOWN);
            return true;
        case SOURCESTYLE_LINECOL:
            assert(b.style == SOURCESTYLE_LINECOL);
            return a.lineCol <= b.lineCol;
        case SOURCESTYLE_OFFSETLEN:
            assert(b.style == SOURCESTYLE_OFFSETLEN);
            return a.offset <= b.offset;
        default:
            assert(false);
            return false;
    }
}








//
// Source_LineCol_struct
//

Source_LineCol_struct::Source_LineCol_struct() : start(), end() {}
Source_LineCol_struct::Source_LineCol_struct(LineCol start, LineCol end) : start(start), end(end) {}

bool operator==(Source_LineCol_struct a, Source_LineCol_struct b) {
    return a.start == b.start && a.end == b.end;
}



//
// Source
//

Source::Source(SourceLocation loc) {
    switch (loc.style) {
        case SOURCESTYLE_UNKNOWN:
            style = SOURCESTYLE_UNKNOWN;
            break;
        case SOURCESTYLE_LINECOL:
            style = SOURCESTYLE_LINECOL;
            lineCol.start = loc.lineCol;
            lineCol.end = loc.lineCol;
            break;
        case SOURCESTYLE_OFFSETLEN:
            style = SOURCESTYLE_OFFSETLEN;
            offsetLen.offset = loc.offset;
            offsetLen.len = 1;
            break;
        default:
            assert(false);
            break;
    }
}

Source::Source() : style(SOURCESTYLE_UNKNOWN) {}

Source::Source(SourceStyle style) : style(style) {}

Source::Source(SourceLocation start, SourceLocation end) {
    switch (start.style) {
        case SOURCESTYLE_UNKNOWN:
            assert(end.style == SOURCESTYLE_UNKNOWN);
            style = SOURCESTYLE_UNKNOWN;
            break;
        case SOURCESTYLE_LINECOL:
            assert(end.style == SOURCESTYLE_LINECOL);
            //
            // This is a really good way to uncover problems
            //
            assert(start <= end);
            style = SOURCESTYLE_LINECOL;
            lineCol.start = start.lineCol;
            lineCol.end = end.lineCol;
            break;
        case SOURCESTYLE_OFFSETLEN:
            assert(end.style == SOURCESTYLE_OFFSETLEN);
            style = SOURCESTYLE_OFFSETLEN;
            offsetLen.offset = start.offset;
            offsetLen.len = (end.offset - start.offset + 1).val;
            break;
        default:
            assert(false);
            break;
    }
}

Source::Source(Source start, Source end) {
    switch (start.style) {
        case SOURCESTYLE_UNKNOWN:
            assert(end.style == SOURCESTYLE_UNKNOWN);
            style = SOURCESTYLE_UNKNOWN;
            break;
        case SOURCESTYLE_LINECOL:
            assert(end.style == SOURCESTYLE_LINECOL);
            style = SOURCESTYLE_LINECOL;
            lineCol.start = start.lineCol.start;
            lineCol.end = end.lineCol.end;
            break;
        case SOURCESTYLE_OFFSETLEN:
            assert(end.style == SOURCESTYLE_OFFSETLEN);
            assert(start.offsetLen.offset + start.offsetLen.len <= end.offsetLen.offset);
            style = SOURCESTYLE_OFFSETLEN;
            offsetLen.offset = start.offsetLen.offset;
            offsetLen.len = ((end.offsetLen.offset - start.offsetLen.offset) + end.offsetLen.len).val;
            break;
        default:
            assert(false);
            break;
    }
}

Source::~Source() {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            break;
        case SOURCESTYLE_LINECOL:
            lineCol.~Source_LineCol_struct();
            break;
        case SOURCESTYLE_OFFSETLEN:
            offsetLen.~Source_OffsetLen_struct();
            break;
        default:
            assert(false);
    }
}

Source::Source(const Source& o) {
    style = o.style;
    switch (o.style) {
        case SOURCESTYLE_UNKNOWN:
            break;
        case SOURCESTYLE_LINECOL:
            lineCol = o.lineCol;
            break;
        case SOURCESTYLE_OFFSETLEN:
            offsetLen = o.offsetLen;
            break;
        default:
            assert(false);
            break;
    }
}

Source& Source::operator=(Source o) {
    style = o.style;
    switch (o.style) {
        case SOURCESTYLE_UNKNOWN:
            break;
        case SOURCESTYLE_LINECOL:
            std::swap(lineCol, o.lineCol);
            break;
        case SOURCESTYLE_OFFSETLEN:
            std::swap(offsetLen, o.offsetLen);
            break;
        default:
            assert(false);
            break;
    }
    return *this;
}

bool isContiguous(Source a, Source b) {
    switch (a.style) {
        case SOURCESTYLE_LINECOL:
            assert(b.style == SOURCESTYLE_LINECOL);
            return isContiguous(a.lineCol.end, b.lineCol.start);
        default:
            assert(false);
            return false;
    }
}

size_t Source::size() const {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            return 0;
        case SOURCESTYLE_LINECOL:
            assert(lineCol.start.Line == lineCol.end.Line);
            return lineCol.end.Col - lineCol.start.Col + 1;
        case SOURCESTYLE_OFFSETLEN:
            return offsetLen.len;
        default:
            assert(false);
            return 0;
    }
}

size_t Source::count() const {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            return 0;
        case SOURCESTYLE_LINECOL:
            return 4;
        case SOURCESTYLE_OFFSETLEN:
            return 2;
        default:
            assert(false);
            return 0;
    }
}

SourceLocation Source::start() const {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            return SourceLocation();
        case SOURCESTYLE_LINECOL:
            return lineCol.start;
        case SOURCESTYLE_OFFSETLEN:
            return offsetLen.offset;
        default:
            assert(false);
            return SourceLocation();
    }
}

SourceLocation Source::end() const {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            return SourceLocation();
        case SOURCESTYLE_LINECOL:
            return lineCol.end;
        case SOURCESTYLE_OFFSETLEN:
            return offsetLen.offset + offsetLen.len;
        default:
            assert(false);
            return SourceLocation();
    }
}

void Source::put(MLINK mlp) const {
    switch (style) {
        case SOURCESTYLE_UNKNOWN:
            break;
        case SOURCESTYLE_LINECOL:
            assert(lineCol.start <= lineCol.end);
            MLPutInteger(mlp, static_cast<int>(lineCol.start.Line));
            MLPutInteger(mlp, static_cast<int>(lineCol.start.Col));
            MLPutInteger(mlp, static_cast<int>(lineCol.end.Line));
            MLPutInteger(mlp, static_cast<int>(lineCol.end.Col));
            break;
        case SOURCESTYLE_OFFSETLEN:
            MLPutInteger(mlp, static_cast<int>(offsetLen.offset.val));
            MLPutInteger(mlp, static_cast<int>(offsetLen.len));
            break;
        default:
            assert(false);
            break;
    }
}



//
// SourceCharacter
//

bool SourceCharacter::isAlphaOrDigit() const {
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

bool SourceCharacter::isDigit() const {
    if (!(0x00 <= valBits && valBits <= 0x7f)) {
        return false;
    }
    return std::isdigit(valBits);
}

bool SourceCharacter::isEndOfFile() const {
    return valBits == EOF;
}

SourceCharacter::SourceCharacter_iterator::SourceCharacter_iterator(int32_t val) : val(val), idx(0) {
    size = ByteEncoder::size(val);
    ByteEncoder::encodeBytes(arr, val);
}

std::ostream& operator<<(std::ostream& stream, const SourceCharacter c) {
    
    if (c.isEndOfFile()) {
        //
        // Do not print anything for EOF
        //
        return stream;
    }
    
    auto val = c.to_point();
    
    assert(val != CODEPOINT_ERROR_INTERNAL);
    
    ByteEncoder::encodeBytes(stream, val);
    
    return stream;
}
