
#include "SourceManager.h"

#include "CodePoint.h"

SourceManager::SourceManager() : data(), dataLength(), idx(), Issues(), state(), SrcLoc(), TokenStartLoc(), WLCharacterStartLoc(), WLCharacterEndLoc(), PrevWLCharacterStartLoc(), PrevWLCharacterEndLoc(), libData() {}

void SourceManager::init(const unsigned char *dataIn, size_t dataLengthIn, SourceStyle style, WolframLibraryData libDataIn) {
  
    data = dataIn;
    dataLength = dataLengthIn;
    
    idx = 0;
    
    state.reset();
    
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
    
    libData = nullptr;
}

unsigned char SourceManager::nextByte() {
    
    assert(idx < dataLength);
    
#ifndef NDEBUG
    auto oldProgress = (100 * idx / dataLength);
#endif
    
    auto b = data[idx];
    idx++;

#ifndef NDEBUG
    auto progress = (100 * idx / dataLength);
    
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
    return (idx == dataLength);
}

//
// Update SourceLoc according to the next source character c
//
// Precondition: SourceLoc is at the character before c
//
// Postcondition: SourceLoc is now at c
//
void SourceManager::advanceSourceLocation(SourceCharacter c) {
    
    SrcLoc = state.advance(c, SrcLoc);
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



void AdvancementState::reset() {
    
    lastCharacterWasCarriageReturn = false;
    
    Issues.clear();
}

SourceLocation AdvancementState::advance(SourceCharacter c, SourceLocation SrcLoc) {
    
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
        
        return SrcLoc.nextLine();
    }
    
    if (c == SourceCharacter('\n')) {
        
        //
        // if lastCharacterWasCarriageReturn, then newline was already handled
        //
        if (lastCharacterWasCarriageReturn) {
            
            lastCharacterWasCarriageReturn = false;
            
            return SrcLoc;
        }
        
        return SrcLoc.nextLine();
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
        
        return SrcLoc.nextLine();
    }
    
    lastCharacterWasCarriageReturn = false;
    
    return SrcLoc+1;
}


std::vector<std::unique_ptr<Issue>> SourceManager::getIssues() {
    
    std::vector<std::unique_ptr<Issue>> TmpIssues;
    
    std::move(Issues.begin(), Issues.end(), std::back_inserter(TmpIssues));
    
    std::move(state.Issues.begin(), state.Issues.end(), std::back_inserter(TmpIssues));
    
    return TmpIssues;
}

std::unique_ptr<SourceManager> TheSourceManager = nullptr;

