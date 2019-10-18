
#include "SourceManager.h"

#include "CodePoint.h"

SourceManager::SourceManager() : buffer(), length(), idx(), lastCharacterWasCarriageReturn(false), Issues(), SrcLoc(), TokenStartLoc(), WLCharacterStartLoc(), WLCharacterEndLoc(), PrevWLCharacterStartLoc(), PrevWLCharacterEndLoc(), libData() {}

void SourceManager::init(SourceStyle style, std::istream& is, WolframLibraryData libDataIn) {
    
    is.seekg(0, is.end);
    
    auto off = is.tellg();
    if (off == -1) {
        //
        // FIXME: need to handle better
        //
        return;
    }
    
    length = static_cast<size_t>(off);
    
    is.seekg(0, is.beg);
    
    buffer = std::unique_ptr<char[]>(new char[length]);
    
    is.read(buffer.get(), length);
    
    idx = 0;
    
    lastCharacterWasCarriageReturn = false;
    
    Issues.clear();
    
    SrcLoc = SourceLocation(style);
    TokenStartLoc = SourceLocation(style);
    WLCharacterStartLoc = SourceLocation(style);
    WLCharacterEndLoc = SourceLocation(style);
    PrevWLCharacterStartLoc = SourceLocation(style);
    PrevWLCharacterEndLoc = SourceLocation(style);
    
    libData = libDataIn;
}

void SourceManager::deinit() {
    
    Issues.clear();
    
    buffer.reset(nullptr);
    
    libData = nullptr;
}

unsigned char SourceManager::nextByte() {
    
    assert(idx < length);
    
#ifndef NDEBUG
    auto oldProgress = (100 * idx / length);
#endif
    
    auto b = buffer[idx];
    idx++;

#ifndef NDEBUG
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
#endif
    
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
            auto Loc = SrcLoc;
            
            //
            // Do not need to advance Col here
            //
            
            auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
            
            Issues.push_back(std::move(I));
        }
        
        lastCharacterWasCarriageReturn = false;
        
        SrcLoc = SrcLoc.nextLine();
        
        return;
    }
    
    if (c == SourceCharacter('\n')) {
        
        //
        // if lastCharacterWasCarriageReturn, then newline was already handled
        //
        if (!lastCharacterWasCarriageReturn) {
            
            SrcLoc = SrcLoc.nextLine();
        }
        
        lastCharacterWasCarriageReturn = false;
        
        return;
    }
    
    //
    // It can happen that single \r occurs.
    // Then make sure to treat it as a newline.
    //
    if (lastCharacterWasCarriageReturn) {
        
        auto Loc = SrcLoc;
        
        //
        // Do not need to advance Col here
        //
        
        auto I = std::unique_ptr<Issue>(new FormatIssue(FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN, "Unexpected ``\\r`` character.", FORMATISSUESEVERITY_FORMATTING, Source(Loc)));
        
        Issues.push_back(std::move(I));
    }
    
    if (c == SourceCharacter('\r')) {
        lastCharacterWasCarriageReturn = true;
        
        SrcLoc = SrcLoc.nextLine();
        
        return;
    }
    
    lastCharacterWasCarriageReturn = false;
    
    SrcLoc++;
}

void SourceManager::setWLCharacterStart() {
    
    PrevWLCharacterStartLoc = WLCharacterStartLoc;
    PrevWLCharacterEndLoc = WLCharacterEndLoc;
    
    WLCharacterStartLoc = SrcLoc;
}

void SourceManager::setWLCharacterEnd() {
    
    WLCharacterEndLoc = SrcLoc;
    
    switch (WLCharacterStartLoc.style) {
        case SOURCESTYLE_UNKNOWN:
            break;
        case SOURCESTYLE_LINECOL:
            assert(WLCharacterStartLoc <= WLCharacterEndLoc);
            break;
        case SOURCESTYLE_OFFSETLEN:
            assert(WLCharacterStartLoc <= WLCharacterEndLoc);
            break;
    }
}

void SourceManager::setTokenStart() {
    TokenStartLoc = WLCharacterStartLoc;
}

Source SourceManager::getTokenSource() const {
    return Source(TokenStartLoc, PrevWLCharacterEndLoc);
}

SourceLocation SourceManager::getWLCharacterStart() const {
    return WLCharacterStartLoc;
}

Source SourceManager::getWLCharacterSource() const {
    return Source(WLCharacterStartLoc, WLCharacterEndLoc);
}

SourceLocation SourceManager::getTokenStart() const {
    return TokenStartLoc;
}

void SourceManager::setSourceLocation(SourceLocation Loc) {
    SrcLoc = Loc;
}

SourceLocation SourceManager::getSourceLocation() const {
    return SrcLoc;
}

std::vector<std::unique_ptr<Issue>>& SourceManager::getIssues() {
    return Issues;
}

std::unique_ptr<SourceManager> TheSourceManager = nullptr;

