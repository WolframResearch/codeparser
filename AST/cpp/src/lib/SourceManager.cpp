
//
// Characters:
// Manage the start and end of characters like \[Alpha] with setWLCharacterStart() and setWLCharacterEnd()
//
// Tokens:
// Manage the start and end of tokens like @@@ with setTokenStart() and setTokenEnd()
// Linear syntax like \! and \( is a single token
//

#include "SourceManager.h"

#include "ByteEncoder.h"

#include <vector>
#include <cctype>

bool isContiguous(SourceLocation a, SourceLocation b) {
    return a.Line == b.Line && a.Col + 1 == b.Col;
}

bool isContiguous(SourceSpan a, SourceSpan b) {
    return isContiguous(a.end, b.start);
}

SourceManager::SourceManager() : eof(false), SourceLoc{1, 0}, TokenStartLoc{0, 0}, TokenEndLoc{0, 0},
    WLCharacterStartLoc{0, 0}, WLCharacterEndLoc{0, 0}, CurLineWidth(0) {}

void SourceManager::init() {
    eof = false;
    SourceLoc = SourceLocation{1, 0};
    TokenStartLoc = SourceLocation{0, 0};
    TokenEndLoc = SourceLocation{0, 0};
    WLCharacterStartLoc = SourceLocation{0 ,0};
    WLCharacterEndLoc = SourceLocation{0, 0};
    CurLineWidth = 0;
}

void SourceManager::deinit() {

}

void SourceManager::advanceSourceLocation(SourceCharacter c) {
    
    if (c == SourceCharacter(EOF)) {
        
        CurLineWidth = SourceLoc.Col;
        
        SourceLoc.Line++;
        SourceLoc.Col = 0;
        
        return;
    }
    
    if (c == SourceCharacter('\n')) {
        
        CurLineWidth = SourceLoc.Col;
        
        SourceLoc.Line++;
        SourceLoc.Col = 0;
        
        return;
    }

    SourceLoc.Col++;
}

void SourceManager::setWLCharacterStart() {
    
    WLCharacterStartLoc = SourceLoc;
}

void SourceManager::setWLCharacterEnd() {
    
    WLCharacterEndLoc = SourceLoc;
}

void SourceManager::setTokenStart() {
    
    TokenStartLoc = WLCharacterStartLoc;
}

void SourceManager::setTokenEnd() {
    
   if (eof) {

       TokenEndLoc = WLCharacterStartLoc;

       return;
   }
    
    //
    // Actually use CharacterStartLoc - 1, because we have already advanced by 1
    //

    if (WLCharacterStartLoc.Col == 0) {

        //
        // Back up to previous line
        //

        SourceLocation Rewind{WLCharacterStartLoc.Line-1, CurLineWidth};
        
        TokenEndLoc = Rewind;
        
        return;
    }
    
    SourceLocation Rewind = WLCharacterStartLoc;
    Rewind.Col--;
    
    TokenEndLoc = Rewind;
}

void SourceManager::setEOF() {

    eof = true;
}

SourceSpan SourceManager::getTokenSpan() {
    
    return SourceSpan{TokenStartLoc, TokenEndLoc};
}

SourceLocation SourceManager::getWLCharacterStart() {
    
    return WLCharacterStartLoc;
}

SourceSpan SourceManager::getWLCharacterSpan() {
    
    return SourceSpan{WLCharacterStartLoc, WLCharacterEndLoc};
}

void SourceManager::setSourceLocation(SourceLocation Loc) {
    
    SourceLoc = Loc;
}

SourceLocation SourceManager::getSourceLocation() {

    return SourceLoc;
}

size_t SourceManager::getCurrentLineWidth() {

    return CurLineWidth;
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

std::vector<unsigned char> SourceCharacter::bytes() const {
    
    if (value_ == EOF) {
        return {};
    }
    
    return ByteEncoder::encodeBytes(value_);
}

std::string SourceCharacter::string() const {
    
    std::ostringstream String;
    
    String << *this;
    
    return String.str();
}

std::ostream& operator<<(std::ostream& s, const SourceCharacter& c) {
    
    for (auto b : c.bytes()) {
        s.put(b);
    }
    
    return s;
}

SourceManager *TheSourceManager = nullptr;
