
#include "Source.h"

#include "ByteDecoder.h" // for ByteDecoder
#include "ByteEncoder.h" // for ByteEncoder
#include "ByteBuffer.h" // for ByteBuffer
#include "Symbol.h" // for SYMBOL_CODEPARSER_LIBRARY_MAKESYNTAXISSUE, etc.
#include "Utils.h" // for isMBNewline, etc.
#include "LongNames.h" // for CodePointToLongNameMap
#include "MyString.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#include <cctype> // for isalnum, isxdigit, isupper, isdigit, isalpha, ispunct, iscntrl with GCC and MSVC
#include <sstream> // for ostringstream


BufferAndLength::BufferAndLength() : buffer(), end() {}

BufferAndLength::BufferAndLength(Buffer buffer, size_t length) : buffer(buffer), end(buffer + length) {}

size_t BufferAndLength::length() const {
    return end - buffer;
}

void BufferAndLength::print(std::ostream& s) const {
    s.write(reinterpret_cast<const char *>(buffer), length());
}

#if USE_MATHLINK
void BufferAndLength::put(MLINK mlp) const {
    
    if (!MLPutUTF8String(mlp, buffer, static_cast<int>(length()))) {
        assert(false);
    }
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr BufferAndLength::toExpr() const {

    auto StrExpr = Expr_UTF8BytesToStringExpr(buffer, static_cast<int>(length()));
    
    return StrExpr;
}
#endif // USE_EXPR_LIB


bool operator==(BufferAndLength a, BufferAndLength b) {
    return a.buffer == b.buffer && a.end == b.end;
}

bool operator!=(BufferAndLength a, BufferAndLength b) {
    return a.buffer != b.buffer || a.end != b.end;
}



bool IssuePtrCompare::operator() (const IssuePtr& L, const IssuePtr& R) const {
    
    if (L->Src < R->Src) {
        return true;
    }
    
    if (L->Tag < R->Tag) {
        return true;
    }
    
    return false;
}


Issue::Issue(const Symbol MakeSym, const MyString Tag, std::string Msg, const MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : MakeSym(MakeSym), Tag(Tag), Msg(Msg), Sev(Sev), Src(Src), Val(Val), Actions(std::move(Actions)), AdditionalDescriptions(AdditionalDescriptions) {}

void Issue::print(std::ostream& s) const {
    
    MakeSym.print(s);
    s << "[";
    
    Tag.print(s);
    s << ", ";
    
    s << Msg;
    s << ", ";
    
    Sev.print(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << Val;
    s << ", ";
    
    for (auto& A : Actions) {
        A->print(s);
        s << ", ";
    }
    
    for (auto& D : AdditionalDescriptions) {
        s << D.c_str();
        s << ", ";
    }
    
    s << "]";
}

bool Issue::check() const {
    return Sev != STRING_FATAL;
}


SyntaxIssue::SyntaxIssue(const MyString Tag, std::string Msg, const MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : Issue(SYMBOL_CODEPARSER_SYNTAXISSUE, Tag, Msg, Sev, Src, Val, std::move(Actions), std::move(AdditionalDescriptions)) {}

FormatIssue::FormatIssue(const MyString Tag, std::string Msg, const MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : Issue(SYMBOL_CODEPARSER_FORMATISSUE, Tag, Msg, Sev, Src, Val, std::move(Actions), std::move(AdditionalDescriptions)) {}

EncodingIssue::EncodingIssue(const MyString Tag, std::string Msg, const MyString Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions) : Issue(SYMBOL_CODEPARSER_ENCODINGISSUE, Tag, Msg, Sev, Src, Val, std::move(Actions), std::move(AdditionalDescriptions)) {}


CodeAction::CodeAction(std::string Label, Source Src) : Label(Label), Src(Src) {}


void ReplaceTextCodeAction::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CODEACTION.print(s);
    s << "[";
    
    s << Label;
    s << ", ";
    
    SYMBOL_CODEPARSER_REPLACETEXT.print(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << ReplacementText;
    
    s << "]";
}

void InsertTextCodeAction::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CODEACTION.print(s);
    s << "[";
    
    s << Label;
    s << ", ";
    
    SYMBOL_CODEPARSER_INSERTTEXT.print(s);
    s << ", ";
    
    Src.print(s);
    s << ", ";
    
    s << InsertionText;
    
    s << "]";
}

void DeleteTextCodeAction::print(std::ostream& s) const {
    
    SYMBOL_CODEPARSER_CODEACTION.print(s);
    s << "[";
    
    s << Label;
    s << ", ";
    
    SYMBOL_CODEPARSER_DELETETEXT.print(s);
    s << ", ";
    
    Src.print(s);
    
    s << "]";
}


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
    
    if (a.first == b.first) {
        
        if (a.second < b.second) {
            return true;
        }
    }
    
    return false;
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
void PrintTo(const SourceLocation& Loc, std::ostream *s) {
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

//
// For googletest
//
void PrintTo(const Source& Src, std::ostream *s) {
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
        case ' ': case '\t': case '\v': case '\f': {
            return true;
        }
        default: {
            return false;
        }
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

std::string SourceCharacter::safeAndGraphicalString() const {

    std::ostringstream String;

    String << "\"" << *this << "\" (" << set_graphical << *this << clear_graphical << ")";
    
    return String.str();
}

std::ostream& operator<<(std::ostream& stream, const SourceCharacter c) {
    
    auto graphicalFlag = stream.iword(get_graphical_i()) == 1;
    
    if (!graphicalFlag) {

        if (c.isEndOfFile()) {
            //
            // Do not print anything for EOF
            //
            return stream;
        }

        auto val = c.to_point();

        assert(val != CODEPOINT_ASSERTFALSE);
        
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
            //
            // Invent something for EOF
            //
            stream << SourceCharacter('<');
            stream << SourceCharacter('E');
            stream << SourceCharacter('O');
            stream << SourceCharacter('F');
            stream << SourceCharacter('>');
            break;
        }
            //
            // whitespace and newline characters
            //
        case '\t': {
            stream << WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
            break;
        }
        case '\n': {
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            break;
        }
        case '\r': {
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            break;
        }
        case CODEPOINT_STRINGMETA_LINEFEED: {
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            break;
        }
        case CODEPOINT_STRINGMETA_CARRIAGERETURN: {
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            break;
        }
        case CODEPOINT_STRINGMETA_TAB: {
            stream << WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
            break;
        }
        case CODEPOINT_STRINGMETA_DOUBLEQUOTE: {
            stream << WLCharacter(CODEPOINT_STRINGMETA_DOUBLEQUOTE, ESCAPE_SINGLE);
            break;
        }
        case CODEPOINT_STRINGMETA_BACKSLASH: {
            stream << WLCharacter(CODEPOINT_STRINGMETA_BACKSLASH, ESCAPE_SINGLE);
            break;
        }
        case CODEPOINT_CRLF: {
            stream << WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
            stream << WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
            break;
        }
            //
            // escape
            //
        case '\x1b': {
            stream << WLCharacter(CODEPOINT_ESC, ESCAPE_LONGNAME);
            break;
        }
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
        case '\x98': case '\x99': case '\x9a': case '\x9b': case '\x9c': case '\x9d': case '\x9e': case '\x9f': {
            stream << WLCharacter(val, ESCAPE_2HEX);
            break;
        }
        default: {
            
            assert(val >= 0);
            
            if (val > 0xffff) {
                
                auto it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), val);
                
                if (it != CodePointToLongNameMap_points.end() && *it == val) {
                    //
                    // Use LongName if available
                    //
                    stream << WLCharacter(val, ESCAPE_LONGNAME);
                    break;
                }
                
                stream << WLCharacter(val, ESCAPE_6HEX);
                break;
                
            } else if (val > 0xff) {
                
                auto it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), val);
                
                if (it != CodePointToLongNameMap_points.end() && *it == val) {
                    //
                    // Use LongName if available
                    //
                    stream << WLCharacter(val, ESCAPE_LONGNAME);
                    break;
                }
                
                stream << WLCharacter(val, ESCAPE_4HEX);
                break;
                
            } else if (val > 0x7f) {
                
                auto it = std::lower_bound(CodePointToLongNameMap_points.begin(), CodePointToLongNameMap_points.end(), val);
                
                if (it != CodePointToLongNameMap_points.end() && *it == val) {
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
void Issue::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, MakeSym.name(), 4)) {
        assert(false);
    }
    
    Tag.put(mlp);
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Msg.c_str()), static_cast<int>(Msg.size()))) {
        assert(false);
    }
    
    Sev.put(mlp);
    
    {
        if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 2 + (Actions.empty() ? 0 : 1) + (AdditionalDescriptions.empty() ? 0 : 1))) {
            assert(false);
        }
        
        Src.put(mlp);
        
        {
            if (!MLPutFunction(mlp, SYMBOL_RULE.name(), 2)) {
                assert(false);
            }
            
            SYMBOL_CONFIDENCELEVEL.put(mlp);
            
            if (!MLPutReal(mlp, Val)) {
                assert(false);
            }
        }
        
        if (!Actions.empty()) {
            
            if (!MLPutFunction(mlp, SYMBOL_RULE.name(), 2)) {
                assert(false);
            }
            
            SYMBOL_CODEPARSER_CODEACTIONS.put(mlp);
            
            if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(Actions.size()))) {
                assert(false);
            }
            
            for (auto& A : Actions) {
                A->put(mlp);
            }
        }
        
        if (!AdditionalDescriptions.empty()) {
            
            if (!MLPutFunction(mlp, SYMBOL_RULE.name(), 2)) {
                assert(false);
            }
            
            STRING_ADDITIONALDESCRIPTIONS.put(mlp);
            
            if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(AdditionalDescriptions.size()))) {
                assert(false);
            }
            
            for (auto& D : AdditionalDescriptions) {
                if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(D.c_str()), static_cast<int>(D.size()))) {
                    assert(false);
                }
            }
        }
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void ReplaceTextCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_CODEACTION.name(), 3)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_REPLACETEXT.put(mlp);
    
    {
        if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 2)) {
            assert(false);
        }
        
        Src.put(mlp);
        
        if (!MLPutFunction(mlp, SYMBOL_RULE.name(), 2)) {
            assert(false);
        }
        
        STRING_REPLACEMENTTEXT.put(mlp);
        
        if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(ReplacementText.c_str()), static_cast<int>(ReplacementText.size()))) {
            assert(false);
        }
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void InsertTextCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_CODEACTION.name(), 3)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_INSERTTEXT.put(mlp);
    
    {
        if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 2)) {
            assert(false);
        }
        
        Src.put(mlp);
        
        if (!MLPutFunction(mlp, SYMBOL_RULE.name(), 2)) {
            assert(false);
        }
        
        STRING_INSERTIONTEXT.put(mlp);
        
        if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(InsertionText.c_str()), static_cast<int>(InsertionText.size()))) {
            assert(false);
        }
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void DeleteTextCodeAction::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_CODEPARSER_CODEACTION.name(), 3)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(mlp, reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()))) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_DELETETEXT.put(mlp);
    
    {
        if (!MLPutFunction(mlp, SYMBOL_ASSOCIATION.name(), 1)) {
            assert(false);
        }
        
        Src.put(mlp);
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void SourceLocation::put(MLINK mlp) const {

    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), 2)) {
        assert(false);
    }

    if (!MLPutInteger(mlp, static_cast<int>(first))) {
        assert(false);
    }
    
    if (!MLPutInteger(mlp, static_cast<int>(second))) {
        assert(false);
    }
}
#endif // USE_MATHLINK

#if USE_MATHLINK
void Source::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_RULE.name(), 2)) {
        assert(false);
    }
    
    SYMBOL_CODEPARSER_SOURCE.put(mlp);
    
    switch (TheParserSession->srcConvention) {
        case SOURCECONVENTION_LINECOLUMN: {
            if (!MLPutFunction(mlp, SYMBOL_LIST.name(), 2)) {
                assert(false);
            }

            if (!MLPutFunction(mlp, SYMBOL_LIST.name(), 2)) {
                assert(false);
            }
            
            if (!MLPutInteger(mlp, static_cast<int>(Start.first))) {
                assert(false);
            }
            
            if (!MLPutInteger(mlp, static_cast<int>(Start.second))) {
                assert(false);
            }
            
            if (!MLPutFunction(mlp, SYMBOL_LIST.name(), 2)) {
                assert(false);
            }
            
            if (!MLPutInteger(mlp, static_cast<int>(End.first))) {
                assert(false);
            }
            
            if (!MLPutInteger(mlp, static_cast<int>(End.second))) {
                assert(false);
            }
            
            break;
        }
        case SOURCECONVENTION_SOURCECHARACTERINDEX: {
            if (!MLPutFunction(mlp, SYMBOL_LIST.name(), 2)) {
                assert(false);
            }
            
            if (!MLPutInteger(mlp, static_cast<int>(Start.second))) {
                assert(false);
            }
            
            if (!MLPutInteger(mlp, static_cast<int>(End.second-1))) {
                assert(false);
            }
            
            break;
        }
        default: {
            assert(false);
            break;
        }
    }
}
#endif // USE_MATHLINK


#if USE_EXPR_LIB
expr Issue::toExpr() const {
    
    auto head = MakeSym.toExpr();
    
    auto e = Expr_BuildExprA(head, 4);
    
    auto TagExpr = Tag.toExpr();
    Expr_InsertA(e, 0 + 1, TagExpr);
    
    auto MsgExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Msg.c_str()), static_cast<int>(Msg.size()));
    Expr_InsertA(e, 1 + 1, MsgExpr);
    
    auto SevExpr = Sev.toExpr();
    Expr_InsertA(e, 2 + 1, SevExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 2 + (Actions.empty() ? 0 : 1) + (AdditionalDescriptions.empty() ? 0 : 1));
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        {
            auto head = SYMBOL_RULE.toExpr();
                    
            auto ConfidenceLevelRuleExpr = Expr_BuildExprA(head, 2);
            
            auto ConfidenceLevelSymExpr = SYMBOL_CONFIDENCELEVEL.toExpr();
            Expr_InsertA(ConfidenceLevelRuleExpr, 0 + 1, ConfidenceLevelSymExpr);
            
            auto ValExpr = Expr_FromReal64(Val);
            Expr_InsertA(ConfidenceLevelRuleExpr, 1 + 1, ValExpr);
            
            Expr_InsertA(DataExpr, 1 + 1, ConfidenceLevelRuleExpr);
        }
        
        int i = 2;
        
        if (!Actions.empty()) {
            
            auto head = SYMBOL_RULE.toExpr();
                    
            auto CodeActionsRuleExpr = Expr_BuildExprA(head, 2);
            
            auto CodeActionsSymExpr = SYMBOL_CODEPARSER_CODEACTIONS.toExpr();
            Expr_InsertA(CodeActionsRuleExpr, 0 + 1, CodeActionsSymExpr);
            
            {
                auto head = SYMBOL_LIST.toExpr();
                        
                auto CodeActionsListExpr = Expr_BuildExprA(head, static_cast<int>(Actions.size()));
                
                int j = 0;
                for (auto& A : Actions) {
                    
                    auto AExpr = A->toExpr();
                    
                    Expr_InsertA(CodeActionsListExpr, j + 1, AExpr);
                    
                    j++;
                }
                
                Expr_InsertA(CodeActionsRuleExpr, 1 + 1, CodeActionsListExpr);
            }
            
            Expr_InsertA(DataExpr, i + 1, CodeActionsRuleExpr);
            i++;
        }
        
        if (!AdditionalDescriptions.empty()) {
            
            auto head = SYMBOL_RULE.toExpr();
                    
            auto AdditionalDescriptionsRuleExpr = Expr_BuildExprA(head, 2);
            
            auto AdditionalDescriptionsStrExpr = STRING_ADDITIONALDESCRIPTIONS.toExpr();
            Expr_InsertA(AdditionalDescriptionsRuleExpr, 0 + 1, AdditionalDescriptionsStrExpr);
            
            {
                auto head = SYMBOL_LIST.toExpr();
                        
                auto AdditionalDescriptionsListExpr = Expr_BuildExprA(head, static_cast<int>(AdditionalDescriptions.size()));
                
                int j = 0;
                for (auto& D : AdditionalDescriptions) {
                    
                    auto DExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(D.c_str()), static_cast<int>(D.size()));
                    
                    Expr_InsertA(AdditionalDescriptionsListExpr, j + 1, DExpr);
                    
                    j++;
                }
                
                Expr_InsertA(AdditionalDescriptionsRuleExpr, 1 + 1, AdditionalDescriptionsListExpr);
            }
            
            Expr_InsertA(DataExpr, i + 1, AdditionalDescriptionsRuleExpr);
            i++;
        }
        
        Expr_InsertA(e, 3 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr ReplaceTextCodeAction::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_CODEACTION.toExpr();
            
    auto e = Expr_BuildExprA(head, 3);
    
    auto LabelExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()));
    Expr_InsertA(e, 0 + 1, LabelExpr);
    
    auto CommandExpr = SYMBOL_CODEPARSER_REPLACETEXT.toExpr();
    Expr_InsertA(e, 1 + 1, CommandExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 2);
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        {
            auto head = SYMBOL_RULE.toExpr();
                    
            auto ReplacementTextRuleExpr = Expr_BuildExprA(head, 2);
            
            auto ReplacementTextStrExpr = STRING_REPLACEMENTTEXT.toExpr();
            Expr_InsertA(ReplacementTextRuleExpr, 0 + 1, ReplacementTextStrExpr);
            
            auto ReplacementTextExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(ReplacementText.c_str()), static_cast<int>(ReplacementText.size()));
            Expr_InsertA(ReplacementTextRuleExpr, 1 + 1, ReplacementTextExpr);
            
            Expr_InsertA(DataExpr, 1 + 1, ReplacementTextRuleExpr);
        }
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr InsertTextCodeAction::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_CODEACTION.toExpr();
            
    auto e = Expr_BuildExprA(head, 3);
    
    auto LabelExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()));
    Expr_InsertA(e, 0 + 1, LabelExpr);
    
    auto CommandExpr = SYMBOL_CODEPARSER_INSERTTEXT.toExpr();
    Expr_InsertA(e, 1 + 1, CommandExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 2);
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        {
            auto head = SYMBOL_RULE.toExpr();
                    
            auto InsertionTextRuleExpr = Expr_BuildExprA(head, 2);
            
            auto InsertionTextStrExpr = STRING_INSERTIONTEXT.toExpr();
            Expr_InsertA(InsertionTextRuleExpr, 0 + 1, InsertionTextStrExpr);
            
            auto InsertionTextExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(InsertionText.c_str()), static_cast<int>(InsertionText.size()));
            Expr_InsertA(InsertionTextRuleExpr, 1 + 1, InsertionTextExpr);
            
            Expr_InsertA(DataExpr, 1 + 1, InsertionTextRuleExpr);
        }
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr DeleteTextCodeAction::toExpr() const {
    
    auto head = SYMBOL_CODEPARSER_CODEACTION.toExpr();
            
    auto e = Expr_BuildExprA(head, 3);
    
    auto LabelExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(Label.c_str()), static_cast<int>(Label.size()));
    Expr_InsertA(e, 0 + 1, LabelExpr);
    
    auto CommandExpr = SYMBOL_CODEPARSER_DELETETEXT.toExpr();
    Expr_InsertA(e, 1 + 1, CommandExpr);
    
    {
        auto head = SYMBOL_ASSOCIATION.toExpr();
        
        auto DataExpr = Expr_BuildExprA(head, 1);
        
        auto SrcExpr = Src.toExpr();
        Expr_InsertA(DataExpr, 0 + 1, SrcExpr);
        
        Expr_InsertA(e, 2 + 1, DataExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr SourceLocation::toExpr() const {

    auto head = SYMBOL_LIST.toExpr();

    auto e = Expr_BuildExprA(head, 2);
    
    auto FirstExpr = Expr_FromInteger64(first);
    Expr_InsertA(e, 0 + 1, FirstExpr);
    
    auto SecondExpr = Expr_FromInteger64(second);
    Expr_InsertA(e, 1 + 1, SecondExpr);

    return e;
}
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
expr Source::toExpr() const {
    
    auto head = SYMBOL_RULE.toExpr();

    auto e = Expr_BuildExprA(head, 2);
    
    auto SourceSymExpr = SYMBOL_CODEPARSER_SOURCE.toExpr();
    Expr_InsertA(e, 0 + 1, SourceSymExpr);
    
    switch (TheParserSession->srcConvention) {
        case SOURCECONVENTION_LINECOLUMN: {
            
            auto head = SYMBOL_LIST.toExpr();

            auto SrcExpr = Expr_BuildExprA(head, 2);
            
            {
                auto head = SYMBOL_LIST.toExpr();

                auto StartExpr = Expr_BuildExprA(head, 2);
                
                auto StartFirstExpr = Expr_FromInteger64(Start.first);
                Expr_InsertA(StartExpr, 0 + 1, StartFirstExpr);

                auto StartSecondExpr = Expr_FromInteger64(Start.second);
                Expr_InsertA(StartExpr, 1 + 1, StartSecondExpr);
                
                Expr_InsertA(SrcExpr, 0 + 1, StartExpr);
            }
            
            {
                auto head = SYMBOL_LIST.toExpr();

                auto EndExpr = Expr_BuildExprA(head, 2);
                
                auto EndFirstExpr = Expr_FromInteger64(End.first);
                Expr_InsertA(EndExpr, 0 + 1, EndFirstExpr);

                auto EndSecondExpr = Expr_FromInteger64(End.second);
                Expr_InsertA(EndExpr, 1 + 1, EndSecondExpr);
                
                Expr_InsertA(SrcExpr, 1 + 1, EndExpr);
            }

            Expr_InsertA(e, 1 + 1, SrcExpr);
            break;
        }
        case SOURCECONVENTION_SOURCECHARACTERINDEX: {
            
            auto head = SYMBOL_LIST.toExpr();

            auto SrcExpr = Expr_BuildExprA(head, 2);
            
            auto StartSecondExpr = Expr_FromInteger64(Start.second);
            Expr_InsertA(SrcExpr, 0 + 1, StartSecondExpr);
            
            auto EndSecondExpr = Expr_FromInteger64(End.second-1);
            Expr_InsertA(SrcExpr, 1 + 1, EndSecondExpr);

            Expr_InsertA(e, 1 + 1, SrcExpr);
            break;
        }
        default: {
            assert(false);
            return nullptr;
        }
    }
    
    return e;
}
#endif // USE_EXPR_LIB
