
//
// Characters:
// Manage the start and end of WL characters like \[Alpha] with setWLCharacterStart() and setWLCharacterEnd()
//
// Tokens:
// Manage the start and end of tokens like @@@ with setTokenStart() and setTokenEnd()
// Linear syntax like \! and \( is a single token
//

#include "Source.h"

#include "ByteEncoder.h"
#include "Utils.h"

#include <vector>
#include <cctype>

SourceLocation::SourceLocation() : Line(0), Col(0) {}

SourceLocation::SourceLocation(size_t Line, size_t Col) : Line(Line), Col(Col) {}

bool isContiguous(SourceLocation a, SourceLocation b) {
    return a.Line == b.Line && a.Col + 1 == b.Col;
}

bool isContiguous(Source a, Source b) {
    return isContiguous(a.lines.end, b.lines.start);
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

void Source::putSourceRule(MLINK mlp) const {
    
    assert(lines.start <= lines.end);
    
    MLPutFunction(mlp, SYMBOL_RULE->name(), 2);
    
    MLPutSymbol(mlp, SYMBOL_SOURCE->name());
    
    const int data[4] = {
        static_cast<int>(lines.start.Line), static_cast<int>(lines.start.Col),
        static_cast<int>(lines.end.Line), static_cast<int>(lines.end.Col)};
    
    static const long SOURCELOCATION_DIMS[2] = {2, 2};
    
    MLPutIntegerArray(mlp, data, SOURCELOCATION_DIMS, nullptr, 2);
}


SourceManager::SourceManager() : lastCharacterWasCarriageReturn(false), eof(false), Issues(), SourceLoc(1, 0), TokenStartLoc(0, 0),
WLCharacterStartLoc(0, 0), WLCharacterEndLoc(0, 0), PrevWLCharacterStartLoc(0, 0), PrevWLCharacterEndLoc(0, 0) {}

void SourceManager::init() {
    lastCharacterWasCarriageReturn = false;
    eof = false;
    
    Issues.clear();
    
    SourceLoc = SourceLocation(1, 0);
    TokenStartLoc = SourceLocation(0, 0);
    WLCharacterStartLoc = SourceLocation(0 ,0);
    WLCharacterEndLoc = SourceLocation(0, 0);
    PrevWLCharacterStartLoc = SourceLocation(0, 0);
    PrevWLCharacterEndLoc = SourceLocation(0, 0);
}

void SourceManager::deinit() {
    Issues.clear();
}

//
// Update SourceLoc according to the next source character c
//
// Precondition: SourceLoc is at the character before c
//
// Postcondition: SourceLoc is now at c
//
void SourceManager::advanceSourceLocation(SourceCharacter c) {
    
    if (c == SOURCECHARACTER_ENDOFFILE) {
        
        eof = true;
        
        //
        // It can happen that single \r occurs.
        // Then make sure to treat it as a newline.
        //
        if (lastCharacterWasCarriageReturn) {
            auto Loc = SourceLoc;
            
            //
            // Do not need to advance Col here
            //
            
            auto Issue = SyntaxIssue(SYNTAXISSUETAG_STRAYCARRIAGERETURN, "Stray ``\\r`` character.\nMac OS 9 line ending?\nTry resaving the file.", SYNTAXISSUESEVERITY_REMARK, Source(Loc, Loc));
            
            Issues.push_back(Issue);
        }
        
        lastCharacterWasCarriageReturn = false;
        
        SourceLoc = SourceLocation(SourceLoc.Line+1, 0);
        
        return;
    }
    
    if (c == SourceCharacter('\n')) {
        
        //
        // if lastCharacterWasCarriageReturn, then newline was already handled
        //
        if (!lastCharacterWasCarriageReturn) {
            
            SourceLoc = SourceLocation(SourceLoc.Line+1, 0);
        }
        
        lastCharacterWasCarriageReturn = false;
        
        return;
    }
    
    //
    // It can happen that single \r occurs.
    // Then make sure to treat it as a newline.
    //
    if (lastCharacterWasCarriageReturn) {
        
        auto Loc = SourceLoc;
        
        //
        // Do not need to advance Col here
        //
        
        auto Issue = SyntaxIssue(SYNTAXISSUETAG_STRAYCARRIAGERETURN, "Stray ``\\r`` character.\nMac OS 9 line ending?\nTry resaving the file.", SYNTAXISSUESEVERITY_REMARK, Source(Loc, Loc));
        
        Issues.push_back(Issue);
    }
    
    if (c == SourceCharacter('\r')) {
        lastCharacterWasCarriageReturn = true;
        
        SourceLoc = SourceLocation(SourceLoc.Line+1, 0);
        
        return;
    }
    
    lastCharacterWasCarriageReturn = false;
    
    SourceLoc = SourceLocation(SourceLoc.Line, SourceLoc.Col+1);
}

void SourceManager::setWLCharacterStart() {
    
    PrevWLCharacterStartLoc = WLCharacterStartLoc;
    PrevWLCharacterEndLoc = WLCharacterEndLoc;
    
    WLCharacterStartLoc = SourceLoc;
}

void SourceManager::setWLCharacterEnd() {
    
    WLCharacterEndLoc = SourceLoc;
    
    assert(WLCharacterStartLoc <= WLCharacterEndLoc);
}

void SourceManager::setTokenStart() {
    TokenStartLoc = WLCharacterStartLoc;
}

Source SourceManager::getTokenSpan() const {
    return Source(TokenStartLoc, PrevWLCharacterEndLoc);
}

SourceLocation SourceManager::getWLCharacterStart() const {
    return WLCharacterStartLoc;
}

Source SourceManager::getWLCharacterSpan() const {
    return Source(WLCharacterStartLoc, WLCharacterEndLoc);
}

SourceLocation SourceManager::getTokenStart() const {
    return TokenStartLoc;
}

void SourceManager::setSourceLocation(SourceLocation Loc) {
    SourceLoc = Loc;
}

SourceLocation SourceManager::getSourceLocation() const {
    return SourceLoc;
}

bool SourceCharacter::isDigitOrAlpha() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    return std::isalnum(value_);
}

bool SourceCharacter::isHex() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    return std::isxdigit(value_);
}

bool SourceCharacter::isOctal() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    return  '0' <= value_ && value_ <= '7';
}

bool SourceCharacter::isUpper() const {
    if (!(0 <= value_ && value_ <= 0x7f)) {
        return false;
    }
    return std::isupper(value_);
}

std::string SourceCharacter::string() const {
    
    std::ostringstream String;
    
    String << *this;
    
    return String.str();
}

std::ostream& operator<<(std::ostream& stream, const SourceCharacter c) {
    
    auto val = c.to_point();
    
    if (val == EOF) {
        return stream;
    }

    ByteEncoder::encodeBytes(stream, val);
    
    return stream;
}

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
        case TOKEN_FAKE_NULL:
        case TOKEN_FAKE_ONE:
        case TOKEN_FAKE_ALL:
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
                    assert(!Utils::containsOnlyASCII(Str));
                }
            }
            break;
    }
    
}

std::vector<SyntaxIssue> SourceManager::getIssues() const {
    return Issues;
}

SourceManager *TheSourceManager = nullptr;
