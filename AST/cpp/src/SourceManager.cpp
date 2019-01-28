
//
// Characters:
// Manage the start and end of characters like \[Alpha] with setWLCharacterStart() and setWLCharacterEnd()
//
// Tokens:
// Manage the start and end of tokens like @@@ with setTokenStart() and setTokenEnd()
// Linear syntax like \! and \( is a single token
//

#include "SourceManager.h"

#include "Utils.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <iomanip>

bool isContiguous(SourceLocation a, SourceLocation b) {
    return a.Line == b.Line && a.Col + 1 == b.Col;
}

bool isContiguous(SourceSpan a, SourceSpan b) {
    return isContiguous(a.end, b.start);
}

SourceManager::SourceManager() : eof(false), SourceLoc{1, 0}, TokenStartLoc{0, 0}, TokenEndLoc{0, 0},
    WLCharacterStartLoc{0, 0}, WLCharacterEndLoc{0, 0}, CurLineWidth(0) {}

void SourceManager::advanceSourceLocation(SourceCharacter c) {
    
    if (c == EOF) {
        
        CurLineWidth = SourceLoc.Col;
        
        SourceLoc.Line++;
        SourceLoc.Col = 0;
        
        return;
    }
    
    if (c == '\n') {
        
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

std::string SourceCharacter::string() const {
    
    std::ostringstream String;
    
    auto i = value_;
    if (i >= 0x80 || i < 0) {
        
        //
        // non-ASCII
        //
        
        if (i == EOF) {
            
            //
            // Do not return a string for EOF
            //
            
        } else {
            
            assert(i > 0);

            String << "0x" << std::setfill('0') << std::setw(4) << std::hex << i << std::dec;
        }
        
    } else if (isSpace()) {
        
        // ASCII
        //
        // \f, \n, \r, \t, \v, or (space) and it is ok to write directly
        //
        
        switch (i) {
            case '\f':
                String.put('\\');
                String.put('f');
                break;
            case '\n':
                String.put('\\');
                String.put('n');
                break;
            case '\r':
                String.put('\\');
                String.put('r');
                break;
            case '\t':
                String.put('\\');
                String.put('t');
                break;
            case '\v':
                String.put('\\');
                String.put('v');
                break;
            default:
                String.put(i);
                break;
        }
        
    } else if (isControl()) {
        
        // ASCII
        //
        // something nasty like '\0'
        //
        
        if (i == '\b') {
            String.put('\\');
            String.put('b');
        } else {
            String << "0x" << std::setfill('0') << std::setw(4) << std::hex << i << std::dec;
        }
        
    } else {
        
        // ASCII
        //
        
        String.put(i);
    }
    
    return String.str();
}

bool SourceCharacter::isDigitOrAlpha() const {
    return std::isalnum(value_);
}

bool SourceCharacter::isAlphaOrDollar() const {
    return std::isalpha(value_) || value_ == '$';
}

bool SourceCharacter::isDigitOrAlphaOrDollar() const {
    return std::isalnum(value_) || value_ == '$';
}

bool SourceCharacter::isHex() const {
    return std::isxdigit(value_);
}

bool SourceCharacter::isOctal() const {
    return  '0' <= value_ && value_ <= '7';
}

bool SourceCharacter::isDigit() const {
    return std::isdigit(value_);
}

bool SourceCharacter::isAlpha() const {
    return std::isalpha(value_);
}

bool SourceCharacter::isSpace() const {
    return std::isspace(value_);
}

bool SourceCharacter::isControl() const {
    return iscntrl(value_);
}

SourceManager *TheSourceManager = nullptr;
