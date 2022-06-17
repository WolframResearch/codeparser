
#include "CharacterDecoder.h"

#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "Utils.h" // for isMBStrange, etc.
#include "LongNames.h" // for LongNameToCodePointMap, etc.
#include "API.h" // for ScopedMLUTF8String
#include "MyString.h"
#include "Symbol.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB


CharacterDecoder::CharacterDecoder() : SimpleLineContinuations(), ComplexLineContinuations(), EmbeddedTabs(), lastBuf(), lastLoc() {}

void CharacterDecoder::init() {
    
    SimpleLineContinuations.clear();
    ComplexLineContinuations.clear();
    EmbeddedTabs.clear();
    
    lastBuf = nullptr;
    lastLoc = SourceLocation();
}


void CharacterDecoder::deinit() {
    
    SimpleLineContinuations.clear();
    ComplexLineContinuations.clear();
    EmbeddedTabs.clear();
}


WLCharacter CharacterDecoder::nextWLCharacter0(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy) {
    
    auto currentWLCharacterStartBuf = TheByteBuffer->buffer;
    auto currentWLCharacterStartLoc = TheByteDecoder->SrcLoc;
    
    auto curSource = TheByteDecoder->nextSourceCharacter0(policy);
    
    if (curSource.to_point() != '\\') {
        
        return WLCharacter(curSource.to_point());
    }
    
    //
    // Handle \
    //
    // handle escapes like line continuation and special characters
    //
    
    while (true) {
        
        //
        // There was a \
        //
        
        auto escapedBuf = TheByteBuffer->buffer;
        auto escapedLoc = TheByteDecoder->SrcLoc;
        
        curSource = TheByteDecoder->nextSourceCharacter0(policy);
        
        //
        // important to mask off 0xff here because CODEPOINT_CRLF is actually multi-byte,
        // but we can test (CODEPOINT_CRLF & 0xff) which is the same as '\r'
        //
        switch (curSource.to_point() & 0xff) {
            case '[': {
                
                return handleLongName(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
            }
            case '\n':
            case '\r': {
                
                //
                // this also handles CODEPOINT_CRLF
                //
                
                curSource = handleLineContinuation(tokenStartBuf, tokenStartLoc, curSource, policy);
                
                if (curSource.to_point() != '\\') {
                    
                    return WLCharacter(curSource.to_point());
                }
                
                //
                // Do not return
                // loop around again
                //
                continue;
            }
            case ':': {
                
                return handle4Hex(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
            }
            default: {
                
                //
                // all other escapes are uncommon and go here
                //
                return handleUncommon(curSource, currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
            }
        } // switch
    } // while (true)
}


WLCharacter CharacterDecoder::currentWLCharacter(Buffer tokenStartBuf, SourceLocation tokenStartLoc, NextPolicy policy) {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto c = nextWLCharacter0(tokenStartBuf, tokenStartLoc, policy);
    
    lastBuf = TheByteBuffer->buffer;
    lastLoc = TheByteDecoder->SrcLoc;
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    return c;
}

WLCharacter CharacterDecoder::handleLongName(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer openSquareBuf, SourceLocation openSquareLoc, NextPolicy policy) {
    
    assert(*openSquareBuf == '[');
    
#if DIAGNOSTICS
    CharacterDecoder_LongNameCount++;
#endif // DIAGNOSTICS
    
    //
    // Do not write leading \[ or trailing ] to LongName
    //
    auto longNameStartBuf = TheByteBuffer->buffer;
    
    auto curSource = TheByteDecoder->currentSourceCharacter(policy);
    
    auto wellFormed = false;
    
#if !NISSUES
    auto atleast1DigitOrAlpha = false;
#endif // !NISSUES
    
    //
    // Read at least 1 alnum before entering loop
    //
    // Must start with upper
    //
    if (curSource.isUpper()) {
        
#if !NISSUES
        atleast1DigitOrAlpha = true;
#endif // !NISSUES
        
        TheByteBuffer->buffer = TheByteDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
        
        curSource = TheByteDecoder->currentSourceCharacter(policy);
        
        while (true) {
            
            if (curSource.isAlphaOrDigit()) {
                
                TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                
                curSource = TheByteDecoder->currentSourceCharacter(policy);
                
            } else if (curSource == SourceCharacter(']')) {
                
                wellFormed = true;
                
                break;
                
            } else {
                
                //
                // Unrecognized
                //
                // Something like \[A!] which is not a long name
                //
                
                break;
            }
        }
        
    } else if (curSource.to_point() == ']') {
        
        //
        // Handle \[]
        //
        
        TheByteBuffer->buffer = TheByteDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
        
        curSource = TheByteDecoder->currentSourceCharacter(policy);
    }
    
    if (!wellFormed) {
        
        //
        // Not well-formed
        //
        
#if !NISSUES
        if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
            
            auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
            auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
            
            auto longNameEndBuf = currentWLCharacterEndBuf;
            
            auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
            auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.buffer), longNameBufAndLen.length());
            
            if (atleast1DigitOrAlpha) {
                
                //
                // Something like \[Alpha
                //
                // Make the warning message a little more relevant
                //
                
                auto suggestion = longNameSuggestion(longNameStr);
                
                CodeActionPtrVector Actions;
                
                auto it = std::lower_bound(LongNameToCodePointMap_names.begin(), LongNameToCodePointMap_names.end(), longNameStr);
                auto found = (it != LongNameToCodePointMap_names.end() && *it == longNameStr);
                if (found) {
                    Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert ``]`` to form ``\\[" + suggestion + "]``", Source(currentWLCharacterEndLoc), "]")));
                }
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\[") + longNameStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
                
            } else {
                
                //
                // Malformed some other way
                //
                // Something like \[!
                // Something like \[*
                //
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\\\[" + longNameStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\[" + longNameStr)));
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\[") + longNameStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
        }
        //
        // TODO: Should we report "\\[]" as unlikely?
        //
#if 0
        else if (unlikelyEscapeChecking) {

            xxx;
            auto I = IssuePtr(new SyntaxIssue(STRING_UNLIKELYESCAPESEQUENCE, std::string("Unlikely escape sequence: ``\\\\[") + LongNameStr + "``", STRING_REMARK, Source(CharacterStart-1, Loc), 0.33));

            Issues.push_back(std::move(I));
        }
#endif // 0
#endif // !NISSUES
        
        TheByteBuffer->buffer = openSquareBuf;
        TheByteDecoder->SrcLoc = openSquareLoc;
        
        return WLCharacter('\\');
    }
    
    //
    // Well-formed
    //
    
    //
    // if unlikelyEscapeChecking, then make sure to append all of the Source characters again
    //
    
    auto longNameEndBuf = TheByteBuffer->buffer;
    
    auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
    auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.buffer), longNameBufAndLen.length());
    
    auto it = std::lower_bound(LongNameToCodePointMap_names.begin(), LongNameToCodePointMap_names.end(), longNameStr);
    auto found = (it != LongNameToCodePointMap_names.end() && *it == longNameStr);
    if (!found) {
        
        //
        // Unrecognized name
        //
        
#if !NISSUES
        if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
            
            auto longNameEndLoc = TheByteDecoder->SrcLoc;
            
            //
            // Accomodate the ] character
            //
            auto currentWLCharacterEndLoc = longNameEndLoc.next();
            
            auto suggestion = longNameSuggestion(longNameStr);
            
            CodeActionPtrVector Actions;
            
            if (!suggestion.empty()) {
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\[" + suggestion + "]``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]")));
            }
            
            //
            // More specifically: Unrecognized
            //
            auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\[") + longNameStr + "]``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {"``" + longNameStr + "`` is not a recognized long name."}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if ((policy & ENABLE_UNLIKELY_ESCAPE_CHECKING) == ENABLE_UNLIKELY_ESCAPE_CHECKING) {
            
            auto longNameEndLoc = TheByteDecoder->SrcLoc;
            
            //
            // Accomodate the ] character
            //
            auto currentWLCharacterEndLoc = longNameEndLoc.next();
            
            auto previousBackslashLoc = currentWLCharacterStartLoc.previous();
            
            auto suggestion = longNameSuggestion(longNameStr);
            
            CodeActionPtrVector Actions;
            
            if (!suggestion.empty()) {
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\[" + suggestion + "]``", Source(previousBackslashLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]")));
            }
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDESCAPESEQUENCE, std::string("Unexpected escape sequence: ``\\\\[") + longNameStr + "]``.", STRING_REMARK, Source(previousBackslashLoc, currentWLCharacterEndLoc), 0.33, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
#endif // !NISSUES
        
        TheByteBuffer->buffer = openSquareBuf;
        TheByteDecoder->SrcLoc = openSquareLoc;
        
        return WLCharacter('\\');
    }
    
    //
    // Success!
    //
    
#if !NISSUES
    if ((policy & ENABLE_UNLIKELY_ESCAPE_CHECKING) == ENABLE_UNLIKELY_ESCAPE_CHECKING) {
        
        //
        // Unexpected escape sequence
        //
        // If found and unlikelyEscapeChecking, then still come in here.
        //
        
        auto longNameEndLoc = TheByteDecoder->SrcLoc;
        
        //
        // Accomodate the ] character
        //
        auto currentWLCharacterEndLoc = longNameEndLoc.next();
        
        auto previousBackslashLoc = currentWLCharacterStartLoc.previous();
        
        auto suggestion = longNameSuggestion(longNameStr);
        
        CodeActionPtrVector Actions;
        
        if (!suggestion.empty()) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\[" + suggestion + "]``", Source(previousBackslashLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]")));
        }
        
        auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDESCAPESEQUENCE, std::string("Unexpected escape sequence: ``\\\\[") + longNameStr + "]``.", STRING_REMARK, Source(previousBackslashLoc, currentWLCharacterEndLoc), 0.33, std::move(Actions), {}));
        
        TheParserSession->addIssue(std::move(I));
    }
#endif // !NISSUES
    
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
    
    auto idx = it - LongNameToCodePointMap_names.begin();
    auto point = LongNameToCodePointMap_points[idx];
    
#if !NISSUES
    if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
        
        auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
        auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.buffer), longNameBufAndLen.length());
        
        if (Utils::isStrange(point)) {
            
            //
            // Just generally strange character is in the code
            //
            auto c = WLCharacter(point, LongNames::isRaw(longNameStr) ? ESCAPE_RAW : ESCAPE_LONGNAME);
            
            auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
            
            auto graphicalStr = c.graphicalString();
            
            auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
            
            CodeActionPtrVector Actions;
            
            for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                Actions.push_back(std::move(A));
            }
            
            //
            // do not recommend replacing graphical character with literal version
            //
            
            //
            // any ASCII replacements
            //
            for (const auto& r : LongNames::asciiReplacements(point)) {
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
            }
            
            if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
                
                //
                // reduce severity of unexpected characters inside strings or comments
                //
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
                
            } else if (c.isStrangeWhitespace()) {
                
                ;
                
            } else {
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
            
        } else if (Utils::isMBStrange(point)) {
            
            //
            // Just generally strange character is in the code
            //
            auto c = WLCharacter(point, LongNames::isRaw(longNameStr) ? ESCAPE_RAW : ESCAPE_LONGNAME);
            
            auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
            
            auto graphicalStr = c.graphicalString();
            
            auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
            
            CodeActionPtrVector Actions;
            
            for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                Actions.push_back(std::move(A));
            }
            
            //
            // do not recommend replacing graphical character with literal version
            //
            
            //
            // any ASCII replacements
            //
            for (const auto& r : LongNames::asciiReplacements(point)) {
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
            }
            
            if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
                
                //
                // reduce severity of unexpected characters inside strings or comments
                //
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
                
            } else if (c.isMBStrangeWhitespace()) {
                
                ;
                
            } else {
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
        }
    }
#endif // !NISSUES
    
    if (LongNames::isRaw(longNameStr)) {
        return WLCharacter(point, ESCAPE_RAW);
    } else {
        return WLCharacter(point, ESCAPE_LONGNAME);
    }
}


WLCharacter CharacterDecoder::handle4Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer colonBuf, SourceLocation colonLoc, NextPolicy policy) {
    
    assert(*colonBuf == ':');
    
#if DIAGNOSTICS
    CharacterDecoder_4HexCount++;
#endif // DIAGNOSTICS
    
    auto hexStartBuf = TheByteBuffer->buffer;
    
    for (auto i = 0; i < 4; i++) {
        
        auto curSource = TheByteDecoder->currentSourceCharacter(policy);
        
        if (curSource.isHex()) {
            
            TheByteBuffer->buffer = TheByteDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \:z
            //
            
#if !NISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto hexEndBuf = currentWLCharacterEndBuf;
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\\\:" + hexStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\:" + hexStr)));
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\:") + hexStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
            
            TheByteBuffer->buffer = colonBuf;
            TheByteDecoder->SrcLoc = colonLoc;
            
            return WLCharacter('\\');
        }
    }
    
    //
    // Success!
    //
    
    auto d3 = Utils::toDigit(hexStartBuf[0]);
    auto d2 = Utils::toDigit(hexStartBuf[1]);
    auto d1 = Utils::toDigit(hexStartBuf[2]);
    auto d0 = Utils::toDigit(hexStartBuf[3]);
    codepoint point = d3 << 12 | d2 << 8 | d1 << 4 | d0;
    
    switch (point) {
        case CODEPOINT_ACTUAL_DOUBLEQUOTE: {
            point = CODEPOINT_STRINGMETA_DOUBLEQUOTE;
            break;
        }
        case CODEPOINT_ACTUAL_BACKSLASH: {
            point = CODEPOINT_STRINGMETA_BACKSLASH;
            break;
        }
    }
    
#if !NISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_4HEX);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
        
        
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_4HEX);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
    }
#endif // !NISSUES
    
    return WLCharacter(point, ESCAPE_4HEX);
}


WLCharacter CharacterDecoder::handle2Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer dotBuf, SourceLocation dotLoc, NextPolicy policy) {
    
    assert(*dotBuf == '.');
    
#if DIAGNOSTICS
    CharacterDecoder_2HexCount++;
#endif // DIAGNOSTICS
    
    auto hexStartBuf = TheByteBuffer->buffer;
    
    for (auto i = 0; i < 2; i++) {
        
        auto curSource = TheByteDecoder->currentSourceCharacter(policy);
        
        if (curSource.isHex()) {
            
            TheByteBuffer->buffer = TheByteDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \.z
            //
            
#if !NISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto hexEndBuf = currentWLCharacterEndBuf;
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\\\." + hexStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\." + hexStr)));
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, "Unhandled character: ``\\." + hexStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
            
            TheByteBuffer->buffer = dotBuf;
            TheByteDecoder->SrcLoc = dotLoc;
            
            return WLCharacter('\\');
        }
    }
    
    //
    // Success!
    //
    
    auto d1 = Utils::toDigit(hexStartBuf[0]);
    auto d0 = Utils::toDigit(hexStartBuf[1]);
    codepoint point = d1 << 4 | d0;
    
    switch (point) {
        case CODEPOINT_ACTUAL_DOUBLEQUOTE: {
            point = CODEPOINT_STRINGMETA_DOUBLEQUOTE;
            break;
        }
        case CODEPOINT_ACTUAL_BACKSLASH: {
            point = CODEPOINT_STRINGMETA_BACKSLASH;
            break;
        }
    }
    
#if !NISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_2HEX);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_2HEX);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
    };
#endif // !NISSUES
    
    return WLCharacter(point, ESCAPE_2HEX);
}


WLCharacter CharacterDecoder::handleOctal(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextPolicy policy) {
    
    assert(SourceCharacter(*firstOctalBuf).isOctal());
    
#if DIAGNOSTICS
    CharacterDecoder_OctalCount++;
#endif // DIAGNOSTICS
    
    auto octalStartBuf = firstOctalBuf;
    
    for (auto i = 0; i < 3-1; i++) {
        
        auto curSource = TheByteDecoder->currentSourceCharacter(policy);
        
        if (curSource.isOctal()) {
            
            TheByteBuffer->buffer = TheByteDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \1z
            //
            
#if !NISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto octalEndBuf = currentWLCharacterEndBuf;
                
                auto octalBufAndLen = BufferAndLength(octalStartBuf, octalEndBuf - octalStartBuf);
                auto octalStr = std::string(reinterpret_cast<const char *>(octalBufAndLen.buffer), octalBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\\\" + octalStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\" + octalStr)));
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\") + octalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES

            TheByteBuffer->buffer = firstOctalBuf;
            TheByteDecoder->SrcLoc = firstOctalLoc;
            
            return WLCharacter('\\');
        }
    }
    
    //
    // Success!
    //
    
    auto d2 = Utils::toDigit(octalStartBuf[0]);
    auto d1 = Utils::toDigit(octalStartBuf[1]);
    auto d0 = Utils::toDigit(octalStartBuf[2]);
    codepoint point = d2 << 6 | d1 << 3 | d0;
    
    switch (point) {
        case CODEPOINT_ACTUAL_DOUBLEQUOTE: {
            point = CODEPOINT_STRINGMETA_DOUBLEQUOTE;
            break;
        }
        case CODEPOINT_ACTUAL_BACKSLASH: {
            point = CODEPOINT_STRINGMETA_BACKSLASH;
            break;
        }
    }
    
#if !NISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_OCTAL);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_OCTAL);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
    };
#endif // !NISSUES
    
    return WLCharacter(point, ESCAPE_OCTAL);
}


WLCharacter CharacterDecoder::handle6Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer barBuf, SourceLocation barLoc, NextPolicy policy) {
    
    assert(*barBuf == '|');
    
#if DIAGNOSTICS
    CharacterDecoder_6HexCount++;
#endif // DIAGNOSTICS
    
    auto hexStartBuf = TheByteBuffer->buffer;
    
    for (auto i = 0; i < 6; i++) {
        
        auto curSource = TheByteDecoder->currentSourceCharacter(policy);
        
        if (curSource.isHex()) {
            
            TheByteBuffer->buffer = TheByteDecoder->lastBuf;
            TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \|z
            //
            
#if !NISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto hexEndBuf = currentWLCharacterEndBuf;
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\\\|" + hexStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\|" + hexStr)));
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\|") + hexStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES

            TheByteBuffer->buffer = barBuf;
            TheByteDecoder->SrcLoc = barLoc;
            
            return WLCharacter('\\');
        }
    }
    
    //
    // Well-formed
    //
    
    auto d5 = Utils::toDigit(hexStartBuf[0]);
    auto d4 = Utils::toDigit(hexStartBuf[1]);
    auto d3 = Utils::toDigit(hexStartBuf[2]);
    auto d2 = Utils::toDigit(hexStartBuf[3]);
    auto d1 = Utils::toDigit(hexStartBuf[4]);
    auto d0 = Utils::toDigit(hexStartBuf[5]);
    codepoint point = d5 << 20 | d4 << 16 | d3 << 12 | d2 << 8 | d1 << 4 | d0;
    
    if (point > 0x10ffff) {
        
        TheByteBuffer->buffer = barBuf;
        TheByteDecoder->SrcLoc = barLoc;
        
        return WLCharacter('\\');
    }
    
    //
    // Success!
    //
    
    switch (point) {
        case CODEPOINT_ACTUAL_DOUBLEQUOTE: {
            point = CODEPOINT_STRINGMETA_DOUBLEQUOTE;
            break;
        }
        case CODEPOINT_ACTUAL_BACKSLASH: {
            point = CODEPOINT_STRINGMETA_BACKSLASH;
            break;
        }
    }
    
#if !NISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_6HEX);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_6HEX);
        
        auto currentSourceCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(std::move(A));
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r)));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
        }
    };
#endif // !NISSUES
    
    return WLCharacter(point, ESCAPE_6HEX);
}


SourceCharacter CharacterDecoder::handleLineContinuation(Buffer tokenStartBuf, SourceLocation tokenStartLoc, SourceCharacter c, NextPolicy policy) {
    
    assert(c.to_point() == '\n' || c.to_point() == '\r' || c.to_point() == CODEPOINT_CRLF);
    
#if DIAGNOSTICS
    CharacterDecoder_LineContinuationCount++;
#endif // DIAGNOSTICS
    
    c = TheByteDecoder->currentSourceCharacter(policy);
    
    //
    // Even though strings preserve the whitespace after a line continuation, and
    // e.g., integers do NOT preserve the whitespace after a line continuation,
    // we do not need to worry about that here.
    //
    // There are no choices to be made here.
    // All whitespace after a line continuation can be ignored for the purposes of tokenization
    //
    while (c.isWhitespace()) {
        
        if (c.to_point() == '\t') {
            if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
                
                //
                // It is possible to have e.g.:
                //
                //"a\
                //<tab>b"
                //
                // where the embedded tab gets consumed by the whitespace loop after the line continuation.
                //
                // Must still count the embedded tab
                
                EmbeddedTabs.insert(tokenStartLoc);
            }
        }
        
        TheByteBuffer->buffer = TheByteDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
        
        c = TheByteDecoder->currentSourceCharacter(policy);
    }
    
    if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
        ComplexLineContinuations.insert(tokenStartLoc);
    } else {
        SimpleLineContinuations.insert(tokenStartLoc);
    }
    
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
    
    return c;
}


WLCharacter CharacterDecoder::handleBackslash(Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy) {
    
#if !NISSUES
    //
    // if inside a string, then test whether this \ is the result of the "feature" of
    // converting "\[Alpa]" into "\\[Alpa]", copying that, and then never giving any further warnings
    // when dealing with "\\[Alpa]"
    //
    if ((policy & ENABLE_UNLIKELY_ESCAPE_CHECKING) == ENABLE_UNLIKELY_ESCAPE_CHECKING) {
        
        auto resetBuf = TheByteBuffer->buffer;
        auto resetLoc = TheByteDecoder->SrcLoc;
        
        auto test = TheByteDecoder->currentSourceCharacter(policy);
        
        if (test.to_point() == '[') {
            
            auto tmpPolicy = policy;
            
            tmpPolicy &= ~ENABLE_CHARACTER_DECODING_ISSUES;
            
            handleLongName(escapedBuf, escapedLoc, resetBuf, resetLoc, tmpPolicy);
        }
        
        TheByteBuffer->buffer = resetBuf;
        TheByteDecoder->SrcLoc = resetLoc;
    }
#endif // !NISSUES
    
    return WLCharacter(CODEPOINT_STRINGMETA_BACKSLASH, ESCAPE_SINGLE);
}


WLCharacter CharacterDecoder::handleUnhandledEscape(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer unhandledBuf, SourceLocation unhandledLoc, SourceCharacter escapedChar, NextPolicy policy) {
    
    //
    // Anything else
    //
    // Something like  \A
    //
    
#if !NISSUES
    //
    // Make the warnings a little more relevant
    //
    
    if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
        
        auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        if (escapedChar.isUpper()) {
            
            //
            // Attempt to read \Alpha] and report a missing [
            //
            
            std::string alnumRun;
            
            alnumRun += escapedChar.to_point();
            
            auto curSource = TheByteDecoder->currentSourceCharacter(policy);
            
            auto wellFormed = false;
            
            while (true) {
                
                if (curSource.isAlphaOrDigit()) {
                    
                    alnumRun += curSource.to_point();
                    
                    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                    
                    curSource = TheByteDecoder->currentSourceCharacter(policy);
                    
                } else if (curSource == SourceCharacter(']')) {
                    
                    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
                    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
                    
                    wellFormed = true;
                    
                    break;
                    
                } else {
                    
                    //
                    // Unrecognized
                    //
                    // Something like \A!] which is not a long name
                    //
                    
                    break;
                }
            }
            
            auto wellFormedAndFound = false;
            if (wellFormed) {
                auto it = std::lower_bound(LongNameToCodePointMap_names.begin(), LongNameToCodePointMap_names.end(), alnumRun);
                wellFormedAndFound = (it != LongNameToCodePointMap_names.end() && *it == alnumRun);
            }
            
            if (wellFormedAndFound) {
                
                //
                // Something like \Alpha]
                //
                
                currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto curSourceGraphicalStr = alnumRun + "]";
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(CodeActionPtr(new InsertTextCodeAction("Insert ``[`` to form ``\\[" + alnumRun + "]``", Source(currentWLCharacterStartLoc.next()), "[")));
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
                
            } else {
                
                if (escapedChar.isHex()) {
                    
                    auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\[" + curSourceGraphicalStr + "XXX]``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + curSourceGraphicalStr + "XXX]")));
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\:" + curSourceGraphicalStr + "XXX``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\:" + curSourceGraphicalStr + "XXX")));
                    
                    auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\[" + curSourceGraphicalStr + "XXX]``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + curSourceGraphicalStr + "XXX]")));
                    
                    auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                    
                    TheParserSession->addIssue(std::move(I));
                }
            }
            
        } else if (escapedChar.isHex()) {
            
            auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
            
            CodeActionPtrVector Actions;
            
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\:" + curSourceGraphicalStr + "xxx``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\:" + curSourceGraphicalStr + "xxx")));
            
            auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
            
            TheParserSession->addIssue(std::move(I));
            
        } else if (escapedChar.isEndOfFile()) {
            
            //
            // Do not know what a good suggestion would be for \<EOF>
            //
            
        } else if (escapedChar.to_point() == CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE || escapedChar.to_point() == CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE || escapedChar.to_point() == CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE) {
            
            //
            // Do not know what a good suggestion would be for \<0xa9>
            //
            
        } else {
            
            //
            // Anything else
            //
            
            auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
            
            if (curSourceGraphicalStr.size() > 1) {
                
                //
                // Something like \<tab>
                //
                // curSourceGraphicalStr is now the 2 characters '\' 't'
                //
                // This can now be confusing when reporting the issue.
                // The correct number of backslashes is required.
                //
                // Do the simple thing: No actions, and report the character with all escaped backslashes now
                //
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, {}, {}));
                
                TheParserSession->addIssue(std::move(I));
                
            } else {
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with ``\\\\" + curSourceGraphicalStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\" + curSourceGraphicalStr)));
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions), {}));
                
                TheParserSession->addIssue(std::move(I));
            }
        }
    }
#endif // !NISSUES
    
    //
    // Keep these treated as 2 characters. This is how bad escapes are handled in WL strings.
    // And has the nice benefit of the single \ still giving an error at top-level
    //
    // Currently, past the bad character
    //
    // Must remember to reset to the bad character
    //
    // The tokenizer will use the bad character to decide what to do
    //
    
    TheByteBuffer->buffer = unhandledBuf;
    TheByteDecoder->SrcLoc = unhandledLoc;
    
    return WLCharacter('\\');
}

WLCharacter CharacterDecoder::handleUncommon(SourceCharacter curSource, Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy) {
    
    switch (curSource.to_point()) {
            
        case '.': {
            
            return handle2Hex(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
        }
        case '|': {
            
            return handle6Hex(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
        }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            return handleOctal(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
        }
    
            //
            // Simple escaped characters
            // \b \f \n \r \t
            //
        case 'b': {
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaBackspace++;
#endif // DIAGNOSTICS
            
            auto c = WLCharacter(CODEPOINT_STRINGMETA_BACKSPACE, ESCAPE_SINGLE);
                        
#if !NISSUES
            {
                auto graphicalStr = c.graphicalString();
                
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
                
                //
                // matched reduced severity of unexpected characters inside strings or comments
                //
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {}));
                
                TheParserSession->addIssue(std::move(I));
            }
        #endif // !NISSUES
                        
            return c;
        }

        case 'f': {
    
            //
            // \f is NOT a space character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaFormFeed++;
#endif // DIAGNOSTICS
            
            auto c = WLCharacter(CODEPOINT_STRINGMETA_FORMFEED, ESCAPE_SINGLE);
            
#if !NISSUES
            {
                auto graphicalStr = c.graphicalString();
                
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
                
                //
                // matched reduced severity of unexpected characters inside strings or comments
                //
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
            
            return c;
        }
            
        case 'n': {
            //
            // \n is NOT a newline character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaLineFeedCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
        }
            
        case 'r': {
            //
            // \r is NOT a newline character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaCarriageReturn++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
        }
            
        case 't': {
            //
            // \t is NOT a space character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaTab++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
        }
            
        //
        // \\ \" \< \>
        //
        // String meta characters
        // What are \< and \> ?
        // https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
        // https://stackoverflow.com/q/6065887
        //
        case '"': {
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaDoubleQuoteCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_STRINGMETA_DOUBLEQUOTE, ESCAPE_SINGLE);
        }
                
        case '\\': {
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaBackslashCount++;
#endif // DIAGNOSTICS
            
            return handleBackslash(escapedBuf, escapedLoc, policy);
        }
            
        case '<': {
            
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaOpenCount++;
#endif // DIAGNOSTICS
            
            auto c = WLCharacter(CODEPOINT_STRINGMETA_OPEN, ESCAPE_SINGLE);
            
#if !NISSUES
            {
                auto graphicalStr = c.graphicalString();
                
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
                
                //
                // matched reduced severity of unexpected characters inside strings or comments
                //
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected string meta character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {"The kernel parses ``\"" + graphicalStr + "\"`` as an empty string."}));
                
                TheParserSession->addIssue(std::move(I));
            }
        #endif // !NISSUES
            
            return c;
        }
            
        case '>': {
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaCloseCount++;
#endif // DIAGNOSTICS
            
            auto c = WLCharacter(CODEPOINT_STRINGMETA_CLOSE, ESCAPE_SINGLE);
            
#if !NISSUES
            {
                auto graphicalStr = c.graphicalString();
                
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
                
                //
                // matched reduced severity of unexpected characters inside strings or comments
                //
                
                auto I = IssuePtr(new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected string meta character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {"The kernel parses ``\"" + graphicalStr + "\"`` as an empty string."}));
                
                TheParserSession->addIssue(std::move(I));
            }
#endif // !NISSUES
            
            return c;
        }
        //
        // Linear syntax characters
        // \! \% \& \( \) \* \+ \/ \@ \^ \_ \` \<space>
        //
        case '!': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxBangCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_BANG, ESCAPE_SINGLE);
        }
        case '%': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxPercentCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_PERCENT, ESCAPE_SINGLE);
        }
        case '&': {
        
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxAmpCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_AMP, ESCAPE_SINGLE);
        }
        case '(': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxOpenParenCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_OPENPAREN, ESCAPE_SINGLE);
        }
        case ')': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxCloseParenCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_CLOSEPAREN, ESCAPE_SINGLE);
        }
        case '*': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxStarCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_STAR, ESCAPE_SINGLE);
        }
        case '+': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxPlusCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_PLUS, ESCAPE_SINGLE);
        }
        case '/': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxSlashCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_SLASH, ESCAPE_SINGLE);
        }
        case '@': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxAtCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_AT, ESCAPE_SINGLE);
        }
        case '^': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxCaretCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_CARET, ESCAPE_SINGLE);
        }
        case '_': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxUnderscoreCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_UNDER, ESCAPE_SINGLE);
        }
        case '`': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxBacktickCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_BACKTICK, ESCAPE_SINGLE);
        }
        case ' ': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxSpaceCount++;
#endif // DIAGNOSTICS
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_SPACE, ESCAPE_SINGLE);
        }
            //
            // Anything else
            //
            // Something like \A or \{
            //
        default: {
            
#if DIAGNOSTICS
            CharacterDecoder_UnhandledCount++;
#endif // DIAGNOSTICS
            
            return handleUnhandledEscape(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, curSource, policy);
        }
    }
}

std::set<SourceLocation>& CharacterDecoder::getSimpleLineContinuations() {
    return SimpleLineContinuations;
}

std::set<SourceLocation>& CharacterDecoder::getComplexLineContinuations() {
    return ComplexLineContinuations;
}

std::set<SourceLocation>& CharacterDecoder::getEmbeddedTabs() {
    return EmbeddedTabs;
}


#if !NISSUES
#if USE_EXPR_LIB
std::string CharacterDecoder::longNameSuggestion(std::string input) {
    
    auto InputExpr = Expr_UTF8BytesToStringExpr(reinterpret_cast<Buffer>(input.c_str()), input.size());
    
    auto e = Expr_LongNameSuggestion(InputExpr);
    
    Buffer buffer;
    size_t len;
    
    Expr_StringExprToUTF8Bytes(e, &buffer, reinterpret_cast<mint *>(&len));
    
    auto suggestion = std::string(reinterpret_cast<const char *>(buffer), len);
    
    Expr_Release(e);
    
    return suggestion;
}
#elif USE_MATHLINK
std::string CharacterDecoder::longNameSuggestion(std::string input) {
    
    MLINK link = TheParserSession->libData->getMathLink(TheParserSession->libData);
    
    if (!MLPutFunction(link, SYMBOL_EVALUATEPACKET.name(), 1)) {
        assert(false);
    }
    
    if (!MLPutFunction(link, SYMBOL_CODEPARSER_LIBRARY_LONGNAMESUGGESTION.name(), 1)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(link, reinterpret_cast<Buffer>(input.c_str()), static_cast<int>(input.size()))) {
        assert(false);
    }
    
    if (!TheParserSession->libData->processMathLink(link)) {
        assert(false);
    }
    
    auto pkt = MLNextPacket(link);
    if (pkt == RETURNPKT) {
        
        ScopedMLString str(link);
        if (!str.read()) {
            assert(false);
        }
        
        auto cstr = str.get();
        
        auto suggestion = std::string(cstr);
        
        return suggestion;
    }
    
    return "";
}
#else
std::string CharacterDecoder::longNameSuggestion(std::string input) {
    return "";
}
#endif // USE_EXPR_LIB
#endif // !NISSUES

CharacterDecoderPtr TheCharacterDecoder = nullptr;
