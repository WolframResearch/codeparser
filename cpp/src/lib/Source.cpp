
#include "Source.h"

#include "ByteDecoder.h" // for ByteDecoder
#include "ByteEncoder.h" // for ByteEncoder
#include "ByteBuffer.h" // for ByteBuffer
#include "Symbol.h" // for SYMBOL_CODEPARSER_LIBRARY_MAKESYNTAXISSUE, etc.
#include "Utils.h" // for isMBNewline, etc.
//#include "WLCharacter.h" // for set_graphical
#include "LongNames.h" // for CodePointToLongNameMap

#include <cctype> // for isalnum, isxdigit, isupper, isdigit, isalpha, ispunct, iscntrl with GCC and MSVC
#include <sstream> // for ostringstream


BufferAndLength::BufferAndLength() : buffer(), end(), status() {}

BufferAndLength::BufferAndLength(Buffer buffer, size_t length, UTF8Status status) : buffer(buffer), end(buffer + length), status(status) {}

size_t BufferAndLength::length() const {
    return end - buffer;
}

void BufferAndLength::printUTF8String(std::ostream& s) const {
    
    if (status == UTF8STATUS_NORMAL) {
        s.write(reinterpret_cast<const char *>(buffer), length());
        return;
    }
    
    std::string str;
    
    auto niceBufAndLen = createNiceBufferAndLength(&str);
    
    niceBufAndLen.printUTF8String(s);
}

#if USE_MATHLINK
void BufferAndLength::putUTF8String(MLINK mlp) const {
    
    if (status == UTF8STATUS_NORMAL) {
        if (!MLPutUTF8String(mlp, buffer, static_cast<int>(length()))) {
            assert(false);
        }
        
        return;
    }
    
    std::string str;
    
    auto niceBufAndLen = createNiceBufferAndLength(&str);
    
    niceBufAndLen.putUTF8String(mlp);
}
#endif // USE_MATHLINK


BufferAndLength BufferAndLength::createNiceBufferAndLength(std::string *str) const {
    
    //
    // make new Buffer
    //
    
    auto oldBuf = TheByteBuffer->buffer;
    auto oldStatus = TheByteDecoder->getStatus();
    
    //
    // This is an error path, so fine to use things like ostringstream
    // that might be frowned upon in happier paths
    //
    std::ostringstream newStrStream;
    
    if (status == UTF8STATUS_NONCHARACTER_OR_BOM) {
        newStrStream << set_graphical;
    }
    
    NextCharacterPolicy policy = 0;
    
    TheByteBuffer->buffer = buffer;
    while (true) {
        
        if (TheByteBuffer->buffer == end) {
            break;
        }
        
        auto c = TheByteDecoder->currentSourceCharacter(policy);
        
        if (status == UTF8STATUS_NONCHARACTER_OR_BOM) {
            
            //
            // Convert to a WLCharacter to allowing making graphical
            //
            
            newStrStream << WLCharacter(c.to_point());
        } else {
            newStrStream << c;
        }
        
        TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    }
    
    TheByteBuffer->buffer = oldBuf;
    TheByteDecoder->setStatus(oldStatus);
    
    *str = newStrStream.str();
    
    auto newB = reinterpret_cast<Buffer>(str->c_str());
    
    auto newLength = str->size();
    
    auto newBufAndLen = BufferAndLength(newB, newLength);
    
    return newBufAndLen;
}

bool operator==(BufferAndLength a, BufferAndLength b) {
    return a.buffer == b.buffer && a.end == b.end;
}




Issue::Issue(std::string Tag, std::string Msg, std::string Sev, Source Src, double Con, std::vector<CodeActionPtr> Actions) : Tag(Tag), Msg(Msg), Sev(Sev), Src(Src), Con(Con), Actions(std::move(Actions)) {}

Source Issue::getSource() const {
    return Src;
}

void SyntaxIssue::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKESYNTAXISSUE->name() << "[";
    
    s << Tag.c_str() << ", ";
    
    s << Msg.c_str() << ", ";
    
    s << Sev.c_str() << ", ";
    
    getSource().print(s);
    
    s << ", ";
    
    s << Con;
    s << ", ";
    
    for (auto& A : Actions) {
        A->print(s);
        s << ", ";
    }
    
    s << "]";
}



CodeAction::CodeAction(std::string Label, Source Src) : Label(Label), Src(Src) {}

Source CodeAction::getSource() const {
    return Src;
}

void ReplaceTextCodeAction::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEREPLACETEXTCODEACTION->name() << "[";
    
    s << Label;
    s << ", ";
    
    getSource().print(s);
    s << ", ";
    
    s << ReplacementText;
    s << ", ";
    
    s << "]";
}

void InsertTextCodeAction::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEINSERTTEXTCODEACTION->name() << "[";
    
    s << Label;
    s << ", ";
    
    getSource().print(s);
    s << ", ";
    
    s << InsertionText;
    s << ", ";
    
    s << "]";
}

void InsertTextAfterCodeAction::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEINSERTTEXTAFTERCODEACTION->name() << "[";
    
    s << Label;
    s << ", ";
    
    getSource().print(s);
    s << ", ";
    
    s << InsertionText;
    s << ", ";
    
    s << "]";
}

void DeleteTextCodeAction::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEDELETETEXTCODEACTION->name() << "[";
    
    s << Label;
    s << ", ";
    
    getSource().print(s);
    s << ", ";
    
    s << "]";
}

void DeleteTriviaCodeAction::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEDELETETRIVIACODEACTION->name() << "[";
    
    s << Label;
    s << ", ";
    
    getSource().print(s);
    s << ", ";
    
    s << "]";
}

void FormatIssue::print(std::ostream& s) const {
    
    s << SYMBOL_CODEPARSER_LIBRARY_MAKEFORMATISSUE->name() << "[";
    
    s << Tag.c_str() << ", ";
    
    s << Msg.c_str() << ", ";
    
    s << Sev.c_str() << ", ";
    
    getSource().print(s);
    
    s << ", ";
    
    s << Con;
    s << ", ";
    
    for (auto& A : Actions) {
        A->print(s);
        s << ", ";
    }
    
    s << "]";
}



//
// SyntaxError
//

std::string SyntaxErrorToString(SyntaxError Err) {
    switch (Err) {
        case SYNTAXERROR_UNKNOWN: return "SyntaxError`Unknown";
        case SYNTAXERROR_EXPECTEDTILDE: return "SyntaxError`ExpectedTilde";
        case SYNTAXERROR_EXPECTEDSET: return "SyntaxError`ExpectedSet";
        case SYNTAXERROR_EXPECTEDOPERAND: return "SyntaxError`ExpectedOperand";
        case SYNTAXERROR_EXPECTEDINTEGRAND: return "SyntaxError`ExpectedIntegrand";
        case SYNTAXERROR_UNEXPECTEDCLOSER: return "SyntaxError`UnexpectedCloser";
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
        case SYNTAXERROR_TOKEN_UNRECOGNIZEDDIGIT: return "SyntaxError`UnrecognizedDigit";
        default:
            assert(false);
            return "";
    }
}

SyntaxError TokenErrorToSyntaxError(TokenEnum T) {
    switch (T.value()) {
        case TOKEN_ERROR_EXPECTEDEQUAL.value(): return SYNTAXERROR_TOKEN_EXPECTEDEQUAL;
        case TOKEN_ERROR_UNHANDLEDCHARACTER.value(): return SYNTAXERROR_TOKEN_UNHANDLEDCHARACTER;
        case TOKEN_ERROR_EXPECTEDLETTERLIKE.value(): return SYNTAXERROR_TOKEN_EXPECTEDLETTERLIKE;
        case TOKEN_ERROR_UNTERMINATEDCOMMENT.value(): return SYNTAXERROR_TOKEN_UNTERMINATEDCOMMENT;
        case TOKEN_ERROR_UNTERMINATEDSTRING.value(): return SYNTAXERROR_TOKEN_UNTERMINATEDSTRING;
        case TOKEN_ERROR_EXPECTEDACCURACY.value(): return SYNTAXERROR_TOKEN_EXPECTEDACCURACY;
        case TOKEN_ERROR_EXPECTEDEXPONENT.value(): return SYNTAXERROR_TOKEN_EXPECTEDEXPONENT;
        case TOKEN_ERROR_EMPTYSTRING.value(): return SYNTAXERROR_TOKEN_EMPTYSTRING;
        case TOKEN_ERROR_UNHANDLEDDOT.value(): return SYNTAXERROR_TOKEN_UNHANDLEDDOT;
        case TOKEN_ERROR_UNRECOGNIZEDDIGIT.value(): return SYNTAXERROR_TOKEN_UNRECOGNIZEDDIGIT;
        default:
            assert(false);
            return SYNTAXERROR_UNKNOWN;
    }
}



//
// SourceLocation
//



SourceLocation::SourceLocation() {}

SourceLocation::SourceLocation(uint32_t first, uint32_t second) : first(first), second(second) {}

bool operator==(SourceLocation a, SourceLocation b) {
    return a.first == b.first && a.second == b.second;
}

bool operator<=(SourceLocation a, SourceLocation b) {

    if (a.first < b.first) {
        return true;
    }

    if (a.first == b.first) {

        if (a.second <= b.second) {
            return true;
        }
    }

    return false;
}

SourceLocation SourceLocation::next() {
    return SourceLocation(first, second + 1);
}

SourceLocation SourceLocation::previous() {
    return SourceLocation(first, second - 1);
}

void SourceLocation::print(std::ostream& s) const {
    s << first;
    s << second;
}

//
// For googletest
//
void PrintTo(const SourceLocation& Loc, std::ostream* s) {
    Loc.print(*s);
}












//
// Source
//

Source::Source() : Start(), End() {}

Source::Source(SourceLocation loc) : Start(loc), End(loc) {}

Source::Source(SourceLocation start, SourceLocation end) : Start(start), End(end) {
    assert(start <= end);
}

Source::Source(Source start, Source end) : Start(start.Start), End(end.End) {
    assert(Start <= End);
}

bool operator==(Source a, Source b) {
    return a.Start == b.Start && a.End == b.End;
}

void Source::print(std::ostream& s) const {
    Start.print(s);
    End.print(s);
}

size_t Source::size() const {
    assert(Start.first == End.first);
    return End.second - Start.second;
}

//
// For googletest
//
void PrintTo(const Source& Src, std::ostream* s) {
    Src.print(*s);
}



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

bool SourceCharacter::isDigit() const {
    if (!(0x00 <= val && val <= 0x7f)) {
        return false;
    }
    return std::isdigit(val);
}

bool SourceCharacter::isEndOfFile() const {
    
    auto val = to_point();
    
    return val == EOF;
}

bool SourceCharacter::isNewline() const {
    
    auto val = to_point();
    
    switch (val) {
        case '\n': case '\r':
            return true;
        default:
            return false;
    }
}

bool SourceCharacter::isWhitespace() const {
    
    auto val = to_point();
    
    switch (val) {
        case ' ': case '\t': case '\v': case '\f':
            return true;
        default:
            return false;
    }
}

bool SourceCharacter::isMBNewline() const {
    
    auto val = to_point();
    
    return LongNames::isMBNewline(val);
}

bool SourceCharacter::isMBWhitespace() const {
    
    auto val = to_point();
    
    return LongNames::isMBWhitespace(val);
}

std::string SourceCharacter::graphicalString() const {
    
    std::ostringstream String;
    
    String << set_graphical << *this << clear_graphical;
    
    return String.str();
}

std::ostream& operator<<(std::ostream& stream, const SourceCharacter c) {
    
    if (stream.iword(get_graphical_i()) == 0) {

        if (c.isEndOfFile()) {
            //
            // Do not print anything for EOF
            //
            return stream;
        }

        auto val = c.to_point();

        assert(val != CODEPOINT_UNKNOWN);

        ByteEncoderState state;

        ByteEncoder::encodeBytes(stream, val, &state);

        return stream;
    }

    //
    // Graphical
    //
    
    auto val = c.to_point();
    
    switch (val) {
        case CODEPOINT_ENDOFFILE:
            //
            // Invent something for EOF
            //
            stream << SourceCharacter('<');
            stream << SourceCharacter('E');
            stream << SourceCharacter('O');
            stream << SourceCharacter('F');
            stream << SourceCharacter('>');
            break;
            //
            // whitespace and newline characters
            //
        case '\t':
            stream << WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
            break;
        case '\n':
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            break;
        case '\r':
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            break;
        case CODEPOINT_STRINGMETA_LINEFEED:
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            break;
        case CODEPOINT_STRINGMETA_CARRIAGERETURN:
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            break;
        case CODEPOINT_STRINGMETA_TAB:
            stream << WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
            break;
        case CODEPOINT_STRINGMETA_DOUBLEQUOTE:
            stream << WLCharacter(CODEPOINT_STRINGMETA_DOUBLEQUOTE, ESCAPE_SINGLE);
            break;
        case CODEPOINT_STRINGMETA_BACKSLASH:
            stream << WLCharacter(CODEPOINT_STRINGMETA_BACKSLASH, ESCAPE_SINGLE);
            break;
        case CODEPOINT_CRLF:
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            break;
            //
            // escape
            //
        case '\x1b':
            stream << WLCharacter(CODEPOINT_ESC, ESCAPE_LONGNAME);
            break;
            //
            // C0 control characters
            //
            //
            // Skip TAB, LF, CR, and ESC. They are handled above
            //
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': case '\x07':
        case '\x08': /*    \x09*/ /*    \x0a*/ case '\x0b': case '\x0c': /*    \x0d*/ case '\x0e': case '\x0f':
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
        case '\x98': case '\x99': case '\x9a': case '\x9b': case '\x9c': case '\x9d': case '\x9e': case '\x9f':
            stream << WLCharacter(val, ESCAPE_2HEX);
            break;
        case CODEPOINT_VIRTUAL_BOM:
            stream << WLCharacter(CODEPOINT_ACTUAL_BOM, ESCAPE_4HEX);
            break;
        default:
            if (val > 0xffff) {
                
                if (CodePointToLongNameMap.find(val) != CodePointToLongNameMap.end()) {
                    //
                    // Use LongName if available
                    //
                    stream << WLCharacter(val, ESCAPE_LONGNAME);
                    break;
                }
                
                stream << WLCharacter(val, ESCAPE_6HEX);
                break;
            } else if (val > 0xff) {
                
                if (CodePointToLongNameMap.find(val) != CodePointToLongNameMap.end()) {
                    //
                    // Use LongName if available
                    //
                    stream << WLCharacter(val, ESCAPE_LONGNAME);
                    break;
                }
                
                stream << WLCharacter(val, ESCAPE_4HEX);
                break;
            } else if (val > 0x7f) {
                
                if (CodePointToLongNameMap.find(val) != CodePointToLongNameMap.end()) {
                    //
                    // Use LongName if available
                    //
                    stream << WLCharacter(val, ESCAPE_LONGNAME);
                    break;
                }
                
                stream << WLCharacter(val, ESCAPE_2HEX);
                break;
            } else {
                
                //
                // ASCII is untouched
                // Do not use CodePointToLongNameMap to find Raw names
                //
                
                ByteEncoderState state;
                
                ByteEncoder::encodeBytes(stream, val, &state);
                
                break;
            }
    }
    
    return stream;
}



//
// SourceCharacter_iterator
//

SourceCharacter::SourceCharacter_iterator::SourceCharacter_iterator(codepoint val) : size(0), idx(0), val(val), arr() {
    
    size = ByteEncoder::size(val);
    
    ByteEncoderState state;
    
    ByteEncoder::encodeBytes(arr, val, &state);
}

SourceCharacter::SourceCharacter_iterator SourceCharacter::begin() {
    assert(!isEndOfFile());
    auto it = SourceCharacter_iterator(val);
    it.idx = 0;
    return it;
}

SourceCharacter::SourceCharacter_iterator SourceCharacter::end() {
    assert(!isEndOfFile());
    auto it = SourceCharacter_iterator(val);
    //
    // 1 past
    //
    it.idx = it.size;
    return it;
}




#if USE_MATHLINK
void SyntaxIssue::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKESYNTAXISSUE->name(), static_cast<int>(3 + 4 + 1 + Actions.size()))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Tag.c_str()), static_cast<int>(Tag.size()))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Msg.c_str()), static_cast<int>(Msg.size()))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Sev.c_str()), static_cast<int>(Sev.size()))) {
        assert(false);
    }
    
    Src.put(mlp);
    
    if (!MLPutReal(mlp, Con)) {
        assert(false);
    }
    
    for (auto& A : Actions) {
        A->put(mlp);
    }
}

void ReplaceTextCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEREPLACETEXTCODEACTION->name(), static_cast<int>(1 + 4 + 1))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    Src.put(mlp);
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(ReplacementText.c_str()), static_cast<int>(ReplacementText.size()))) {
        assert(false);
    }
}

void InsertTextCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEINSERTTEXTCODEACTION->name(), static_cast<int>(1 + 4 + 1))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    Src.put(mlp);
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(InsertionText.c_str()), static_cast<int>(InsertionText.size()))) {
        assert(false);
    }
}

void InsertTextAfterCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEINSERTTEXTAFTERCODEACTION->name(), static_cast<int>(1 + 4 + 1))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    Src.put(mlp);
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(InsertionText.c_str()), static_cast<int>(InsertionText.size()))) {
        assert(false);
    }
}

void DeleteTextCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEDELETETEXTCODEACTION->name(), static_cast<int>(1 + 4))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    Src.put(mlp);
}

void DeleteTriviaCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEDELETETRIVIACODEACTION->name(), static_cast<int>(1 + 4))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    Src.put(mlp);
}

void FormatIssue::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_LIBRARY_MAKEFORMATISSUE->name(), static_cast<int>(3 + 4 + 1 + Actions.size()))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Tag.c_str()), static_cast<int>(Tag.size()))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Msg.c_str()), static_cast<int>(Msg.size()))) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Sev.c_str()), static_cast<int>(Sev.size()))) {
        assert(false);
    }
    
    Src.put(mlp);
    
    if (!MLPutReal(mlp, Con)) {
        assert(false);
    }
    
    for (auto& A : Actions) {
        A->put(mlp);
    }
}

void SourceLocation::put(MLINK mlp) const {
    if (!MLPutInteger(mlp, static_cast<int>(first))) {
        assert(false);
    }
    
    if (!MLPutInteger(mlp, static_cast<int>(second))) {
        assert(false);
    }
}

void Source::put(MLINK mlp) const {
    
    Start.put(mlp);
    End.put(mlp);
}

#endif // USE_MATHLINK

