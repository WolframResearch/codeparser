
#include "Source.h"

#include "ByteEncoder.h" // for ByteEncoder
#include "Utils.h" // for isMBNewline, etc.
#include "LongNamesRegistration.h" // for CodePointToLongNameMap
#include "LongNames.h"

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#include <cctype> // for isalnum, isxdigit, isupper, isdigit, isalpha, ispunct, iscntrl with GCC and MSVC
#include <sstream> // for ostringstream
#include <algorithm> // for lower_bound


BufferAndLength::BufferAndLength() : Buf(), Len() {}

BufferAndLength::BufferAndLength(Buffer Buf) : Buf(Buf), Len(0) {}

BufferAndLength::BufferAndLength(CBufferAndLength bufAndLen) : Buf(bufAndLen.Buf), Len(bufAndLen.Len) {}

BufferAndLength::BufferAndLength(Buffer Buf, size_t Len) : Buf(Buf), Len(Len) {
    assert(Len < 1ULL << 48);
}

size_t BufferAndLength::length() const {
    return Len;
}

Buffer BufferAndLength::end() const {
    return Buf + Len;
}

bool BufferAndLength::containsOnlyASCII() const {
    
    for (auto p = Buf; p < end(); p++) {
        
        auto c = *p;
        
        if (c > 0x7f) {
            return false;
        }
    }
    
    return true;
}

bool BufferAndLength::containsTab() const {
    
    for (auto p = Buf; p < end(); p++) {
        
        auto c = *p;
        
        if (c == 0x09) {
            return true;
        }
    }
    
    return false;
}

void BufferAndLength::print(std::ostream& s) const {
    s.write(reinterpret_cast<const char *>(Buf), length());
}

#if USE_MATHLINK
void BufferAndLength::put(ParserSessionPtr session, MLINK callLink) const {
    if (!MLPutUTF8String(callLink, Buf, static_cast<int>(length()))) {
        assert(false);
    }
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr BufferAndLength::toExpr(ParserSessionPtr session) const {
    return Expr_UTF8BytesToStringExpr(Buf, static_cast<int>(length()));
}
#endif // USE_EXPR_LIB





//
// SourceLocation
//

SourceLocation::SourceLocation() : first(0), second(0) {}

SourceLocation::SourceLocation(uint32_t first, uint32_t second) : first(first), second(second) {}

bool operator==(SourceLocation a, SourceLocation b) {
    return a.first == b.first && a.second == b.second;
}

bool operator!=(SourceLocation a, SourceLocation b) {
    return a.first != b.first || a.second != b.second;
}

bool operator<(SourceLocation a, SourceLocation b) {
    
    if (a.first < b.first) {
        return true;
    }
    
    if (a.first != b.first) {
        return false;
    }
    
    if (a.second < b.second) {
        return true;
    }

    return false;
}

bool operator<=(SourceLocation a, SourceLocation b) {
    
    if (a.first < b.first) {
        return true;
    }
    
    if (a.first != b.first) {
        return false;
    }
    
    if (a.second <= b.second) {
        return true;
    }

    return false;
}

SourceLocation SourceLocation::next() const {
#if COMPUTE_SOURCE
    return SourceLocation(first, second + 1);
#else
    return SourceLocation();
#endif // COMPUTE_SOURCE
}

SourceLocation SourceLocation::previous() const {
#if COMPUTE_SOURCE
    return SourceLocation(first, second - 1);
#else
    return SourceLocation();
#endif // COMPUTE_SOURCE
}

void SourceLocation::print(std::ostream& s) const {
    
    s << first;
    
    s << second;
}

#if BUILD_TESTS
//
// For googletest
//
void PrintTo(const SourceLocation& Loc, std::ostream *s) {
    Loc.print(*s);
}
#endif // BUILD_TESTS



//
// Source
//

Source::Source() : Start(), End() {}

Source::Source(SourceLocation only) : Start(only), End(only) {}

Source::Source(SourceLocation start, SourceLocation end) : Start(start), End(end) {
    assert(start <= end);
}

Source::Source(Source start, Source end) : Start(start.Start), End(end.End) {
    assert(Start <= End);
}

bool operator==(Source a, Source b) {
    return a.Start == b.Start && a.End == b.End;
}

bool operator!=(Source a, Source b) {
    return a.Start != b.Start || a.End != b.End;
}

bool operator<(Source a, Source b) {
    return a.Start < b.Start;
}

void Source::print(std::ostream& s) const {
    
    Start.print(s);
    
    End.print(s);
}

size_t Source::size() const {
    
    assert(Start.first == End.first);
    
    return End.second - Start.second;
}

#if BUILD_TESTS
//
// For googletest
//
void PrintTo(const Source& Src, std::ostream *s) {
    Src.print(*s);
}
#endif // BUILD_TESTS



//
// SourceCharacter
//

bool SourceCharacter::isAlphaOrDigit() const {
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::isalnum(val);
}

bool SourceCharacter::isHex() const {
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::isxdigit(val);
}

bool SourceCharacter::isOctal() const {
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return '0' <= val && val <= '7';
}

bool SourceCharacter::isUpper() const {
    
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    
    return std::isupper(val);
}

bool SourceCharacter::isEndOfFile() const {
    return val == EOF;
}

bool SourceCharacter::isNewline() const {
    
    switch (val) {
        case '\n': case '\r': {
            return true;
        }
    }
    
    return false;
}

bool SourceCharacter::isWhitespace() const {
    
    switch (val) {
        case ' ': case '\t': case '\v': case '\f': {
            return true;
        }
    }
    
    return false;
}

bool SourceCharacter::isMBNewline() const {
    return LongNames::isMBNewline(val);
}

bool SourceCharacter::isMBWhitespace() const {
    return LongNames::isMBWhitespace(val);
}

bool SourceCharacter::isMBUnsafeUTF8Sequence() const {
    
    switch (val) {
        case CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE:
        case CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE:
        case CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE: {
            return true;
        }
    }
    
    return false;
}

std::string SourceCharacter::graphicalString() const {
    
    std::ostringstream String;
    
    String << set_graphical << *this << clear_graphical;
    
    return String.str();
}

std::string SourceCharacter::safeAndGraphicalString() const {

    std::ostringstream String;

    String << "\"" << *this << "\" (" << set_graphical << *this << clear_graphical << ")";
    
    return String.str();
}

bool operator==(SourceCharacter a, SourceCharacter b) {
    return a.val == b.val;
}

std::ostream& operator<<(std::ostream& stream, const SourceCharacter c) {
    
    auto graphicalFlag = stream.iword(get_graphical_i()) == 1;
    
    if (!graphicalFlag) {

        auto val = c.to_point();

        assert(val != CODEPOINT_ASSERTFALSE);
        assert(val != CODEPOINT_ENDOFFILE);
        
        ByteEncoderState state;

        ByteEncoder::encodeBytes(stream, val, &state);

        return stream;
    }

    //
    // Graphical
    //
    
    auto val = c.to_point();
    
    switch (val) {
        case CODEPOINT_ENDOFFILE: {
            
            assert(false);
            
            return stream;
        }
        case '\b': {
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_BACKSPACE, ESCAPE_SINGLE);
            
            return stream;
        }
        case '\f': {
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_FORMFEED, ESCAPE_SINGLE);
            
            return stream;
        }
            //
            // whitespace and newline characters
            //
        case '\t': {
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
            
            return stream;
        }
        case '\n': {
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            
            return stream;
        }
        case '\r': {
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_STRINGMETA_LINEFEED: {
            
            //
            // no single SourceCharacter for \<LF>
            //
            assert(false);
            
            return stream;
        }
        case CODEPOINT_STRINGMETA_CARRIAGERETURN: {
            
            //
            // no single SourceCharacter for \<CR>
            //
            assert(false);
            
            return stream;
        }
        case CODEPOINT_STRINGMETA_TAB: {
            
            //
            // no single SourceCharacter for \t
            //
            assert(false);
            
            return stream;
        }
        case CODEPOINT_STRINGMETA_DOUBLEQUOTE: {
            
            //
            // Coming from \[RawDoubleQuote]
            //
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_DOUBLEQUOTE, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_STRINGMETA_BACKSLASH: {
            
            //
            // Coming from \[RawBackslash]
            //
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_BACKSLASH, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_CRLF: {
            
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_BANG: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_BANG, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_PERCENT: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_PERCENT, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_AMP: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_AMP, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_OPENPAREN: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_OPENPAREN, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_CLOSEPAREN: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_CLOSEPAREN, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_STAR: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_STAR, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_PLUS: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_PLUS, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_SLASH: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_SLASH, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_AT: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_AT, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_CARET: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_CARET, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_UNDER: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_UNDER, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_BACKTICK: {
            
            stream << WLCharacter(CODEPOINT_LINEARSYNTAX_BACKTICK, ESCAPE_SINGLE);
            
            return stream;
        }
        case CODEPOINT_LINEARSYNTAX_SPACE: {
            
            //
            // no single SourceCharacter for \<space>
            //
            assert(false);
            
            return stream;
        }
            //
            // escape
            //
        case '\x1b': {
            
            stream << WLCharacter(CODEPOINT_ESC, ESCAPE_LONGNAME);
            
            return stream;
        }
            //
            // C0 control characters
            //
            //
            // Skip BS, TAB, LF, FF, CR, and ESC. They are handled above
            //
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': case '\x07':
        /*    \x08*/ /*    \x09*/ /*    \x0a*/ case '\x0b': /*    \x0c*/ /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': /*    \x1b*/ case '\x1c': case '\x1d': case '\x1e': case '\x1f':
            //
            // Make sure to include DEL
            //
        case CODEPOINT_DEL:
            //
            // C1 control characters
            //
        case '\x80': case '\x81': case '\x82': case '\x83': case '\x84': case '\x85': case '\x86': case '\x87':
        case '\x88': case '\x89': case '\x8a': case '\x8b': case '\x8c': case '\x8d': case '\x8e': case '\x8f':
        case '\x90': case '\x91': case '\x92': case '\x93': case '\x94': case '\x95': case '\x96': case '\x97':
        case '\x98': case '\x99': case '\x9a': case '\x9b': case '\x9c': case '\x9d': case '\x9e': case '\x9f': {
            
            stream << WLCharacter(val, ESCAPE_2HEX);
            
            return stream;
        }
    }
    
    assert(val >= 0);
    
    if (val > 0xffff) {
        
        auto it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), val);
        
        if (it != CodePointToLongNameMap_points.end() && *it == val) {
            
            //
            // Use LongName if available
            //
            stream << WLCharacter(val, ESCAPE_LONGNAME);
            
            return stream;
        }
        
        stream << WLCharacter(val, ESCAPE_6HEX);
        
        return stream;
    }
    
    if (val > 0xff) {
        
        auto it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), val);
        
        if (it != CodePointToLongNameMap_points.end() && *it == val) {
            
            //
            // Use LongName if available
            //
            stream << WLCharacter(val, ESCAPE_LONGNAME);
            
            return stream;
        }
        
        stream << WLCharacter(val, ESCAPE_4HEX);
        
        return stream;
    }
    
    if (val > 0x7f) {
        
        auto it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), val);
        
        if (it != CodePointToLongNameMap_points.end() && *it == val) {
            
            //
            // Use LongName if available
            //
            stream << WLCharacter(val, ESCAPE_LONGNAME);
            
            return stream;
        }
        
        stream << WLCharacter(val, ESCAPE_2HEX);
        
        return stream;
    }
        
    //
    // ASCII is untouched
    // Do not use CodePointToLongNameMap to find Raw names
    //
    
    ByteEncoderState state;
    
    ByteEncoder::encodeBytes(stream, val, &state);
    
    return stream;
}



#if USE_MATHLINK
void SourceLocation::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_LIST.Name, 2)) {
        assert(false);
    }

    if (!MLPutInteger(callLink, static_cast<int>(first))) {
        assert(false);
    }
    
    if (!MLPutInteger(callLink, static_cast<int>(second))) {
        assert(false);
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void Source::put(ParserSessionPtr session, MLINK callLink) const {
    
    if (!MLPutFunction(callLink, SYMBOL_RULE.Name, 2)) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_SOURCE.put(session, callLink);
    
    switch (session->opts.srcConvention) {
        case SOURCECONVENTION_LINECOLUMN: {
            
            if (!MLPutFunction(callLink, SYMBOL_LIST.Name, 2)) {
                assert(false);
            }

            if (!MLPutFunction(callLink, SYMBOL_LIST.Name, 2)) {
                assert(false);
            }
            
            if (!MLPutInteger(callLink, static_cast<int>(Start.first))) {
                assert(false);
            }
            
            if (!MLPutInteger(callLink, static_cast<int>(Start.second))) {
                assert(false);
            }
            
            if (!MLPutFunction(callLink, SYMBOL_LIST.Name, 2)) {
                assert(false);
            }
            
            if (!MLPutInteger(callLink, static_cast<int>(End.first))) {
                assert(false);
            }
            
            if (!MLPutInteger(callLink, static_cast<int>(End.second))) {
                assert(false);
            }
            
            return;
        }
        case SOURCECONVENTION_SOURCECHARACTERINDEX: {
            
            if (!MLPutFunction(callLink, SYMBOL_LIST.Name, 2)) {
                assert(false);
            }
            
            if (!MLPutInteger(callLink, static_cast<int>(Start.second))) {
                assert(false);
            }
            
            if (!MLPutInteger(callLink, static_cast<int>(End.second-1))) {
                assert(false);
            }
            
            return;
        }
    }
    
    assert(false);
}
#endif // USE_MATHLINK



#if USE_EXPR_LIB
expr SourceLocation::toExpr(ParserSessionPtr session) const {

    auto head = SYMBOL_LIST.toExpr(session);

    auto e = Expr_BuildExprA(head, 2);
    
    auto FirstExpr = Expr_FromInteger64(first);
    Expr_InsertA(e, 0 + 1, FirstExpr);
    
    auto SecondExpr = Expr_FromInteger64(second);
    Expr_InsertA(e, 1 + 1, SecondExpr);

    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr Source::toExpr(ParserSessionPtr session) const {
    
    auto head = SYMBOL_RULE.toExpr(session);

    auto e = Expr_BuildExprA(head, 2);
    
    auto SourceSymExpr = SYMBOL_CODEPARSER_SOURCE.toExpr(session);
    Expr_InsertA(e, 0 + 1, SourceSymExpr);
    
    switch (session->opts.srcConvention) {
        case SOURCECONVENTION_LINECOLUMN: {
            
            auto head = SYMBOL_LIST.toExpr(session);

            auto SrcExpr = Expr_BuildExprA(head, 2);
            
            {
                auto head = SYMBOL_LIST.toExpr(session);

                auto StartExpr = Expr_BuildExprA(head, 2);
                
                auto StartFirstExpr = Expr_FromInteger64(Start.first);
                Expr_InsertA(StartExpr, 0 + 1, StartFirstExpr);

                auto StartSecondExpr = Expr_FromInteger64(Start.second);
                Expr_InsertA(StartExpr, 1 + 1, StartSecondExpr);
                
                Expr_InsertA(SrcExpr, 0 + 1, StartExpr);
            }
            
            {
                auto head = SYMBOL_LIST.toExpr(session);

                auto EndExpr = Expr_BuildExprA(head, 2);
                
                auto EndFirstExpr = Expr_FromInteger64(End.first);
                Expr_InsertA(EndExpr, 0 + 1, EndFirstExpr);

                auto EndSecondExpr = Expr_FromInteger64(End.second);
                Expr_InsertA(EndExpr, 1 + 1, EndSecondExpr);
                
                Expr_InsertA(SrcExpr, 1 + 1, EndExpr);
            }

            Expr_InsertA(e, 1 + 1, SrcExpr);
            
            return e;
        }
        case SOURCECONVENTION_SOURCECHARACTERINDEX: {
            
            auto head = SYMBOL_LIST.toExpr(session);

            auto SrcExpr = Expr_BuildExprA(head, 2);
            
            auto StartSecondExpr = Expr_FromInteger64(Start.second);
            Expr_InsertA(SrcExpr, 0 + 1, StartSecondExpr);
            
            auto EndSecondExpr = Expr_FromInteger64(End.second-1);
            Expr_InsertA(SrcExpr, 1 + 1, EndSecondExpr);

            Expr_InsertA(e, 1 + 1, SrcExpr);
            
            return e;
        }
    }
    
    assert(false);
    
    return nullptr;
}
#endif // USE_EXPR_LIB
