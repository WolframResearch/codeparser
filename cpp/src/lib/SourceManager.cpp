
#include "SourceManager.h"

#include "CodePoint.h"

SourceManager::SourceManager() : buffer(), length(), idx(), lastCharacterWasCarriageReturn(false), Issues(), SourceLoc(1, 0), TokenStartLoc(0, 0),
WLCharacterStartLoc(0, 0), WLCharacterEndLoc(0, 0), PrevWLCharacterStartLoc(0, 0), PrevWLCharacterEndLoc(0, 0) {}

void SourceManager::init(std::istream& is, WolframLibraryData libDataIn) {
    
    is.seekg(0, is.end);
    length = is.tellg();
    is.seekg(0, is.beg);
    
    buffer = new char[length];
    
    is.read(buffer, length);
    
    idx = 0;
    
    lastCharacterWasCarriageReturn = false;
    
    Issues.clear();
    
    SourceLoc = SourceLocation(1, 0);
    TokenStartLoc = SourceLocation(0, 0);
    WLCharacterStartLoc = SourceLocation(0 ,0);
    WLCharacterEndLoc = SourceLocation(0, 0);
    PrevWLCharacterStartLoc = SourceLocation(0, 0);
    PrevWLCharacterEndLoc = SourceLocation(0, 0);
    
    libData = libDataIn;
}

void SourceManager::deinit() {
    Issues.clear();
    
    delete[] buffer;
}

unsigned char SourceManager::nextByte() {
    
    assert(idx < length);
    
    auto oldProgress = (100 * idx / length);
    
    auto b = buffer[idx];
    idx++;
    
    auto progress = (100 * idx / length);
    
    if (progress != oldProgress) {
        if (libData) {
            MLINK link = libData->getMathLink(libData);
            MLPutFunction(link, "EvaluatePacket", 1);
            MLPutFunction(link, "AST`Library`SetConcreteParseProgress", 1);
            MLPutInteger(link, static_cast<int>(progress));
            libData->processMathLink(link);
            auto pkt = MLNextPacket(link);
            if (pkt == RETURNPKT) {
                if(!MLNewPacket(link)) {
                    b = -1;
                }
            } else {
                b = -1;
            }
        }
    }
    
    return b;
}

bool SourceManager::isEndOfFile() const {
    return (idx == length);
}

//
// Update SourceLoc according to the next source character c
//
// Precondition: SourceLoc is at the character before c
//
// Postcondition: SourceLoc is now at c
//
void SourceManager::advanceSourceLocation(SourceCharacter c) {
    
    if (c == SourceCharacter(CODEPOINT_ENDOFFILE)) {
        
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

std::vector<SyntaxIssue> SourceManager::getIssues() const {
    return Issues;
}

SourceManager *TheSourceManager = nullptr;

