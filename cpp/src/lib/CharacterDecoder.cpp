
#include "CharacterDecoder.h"

#include "ByteDecoder.h"
#include "ByteBuffer.h"
#include "Utils.h"
#include "CharacterMaps.h"
#include "CodePoint.h"
#include "API.h"


CharacterDecoder::CharacterDecoder() : Issues(), libData(), lastBuf(), lastLoc() {}

void CharacterDecoder::init(WolframLibraryData libDataIn) {
    
    Issues.clear();
    
    libData = libDataIn;
}

void CharacterDecoder::deinit() {
    
    Issues.clear();
}

// Precondition: buffer is pointing to current WLCharacter
// Postcondition: buffer is pointing to next WLCharacter
//
// Example:
// memory: 1+\[Alpha]-2
//           ^
//           buffer
//
// after calling nextWLCharacter:
// memory: 1+\[Alpha]-2
//                   ^
//                   buffer
// return \[Alpha]
//
WLCharacter CharacterDecoder::nextWLCharacter0(NextCharacterPolicy policy) {
    
    auto currentWLCharacterStartBuf = TheByteBuffer->buffer;
    auto currentWLCharacterStartLoc = TheByteDecoder->SrcLoc;
    
    auto curSource = TheByteDecoder->nextSourceCharacter0(policy);
    
    //
    // Handle \
    //
    // handle escapes like line continuation and special characters
    //
    
    WLCharacter c;
    
    if (curSource.to_point() != '\\') {
        
        c = WLCharacter(curSource.to_point());
        
    } else {
        
        //
        // There was a \
        //
        
        auto escapedBuf = TheByteBuffer->buffer;
        auto escapedLoc = TheByteDecoder->SrcLoc;
        
        curSource = TheByteDecoder->nextSourceCharacter0(policy);
        
        switch (curSource.to_point()) {
            case '\n':
            case '\r':
            case CODEPOINT_CRLF:
                c = handleLineContinuation(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, curSource, policy);
                break;
            case '[':
                c = handleLongName(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
                break;
            case ':':
                c = handle4Hex(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
                break;
            case '.':
                c = handle2Hex(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
                break;
            case '|':
                c = handle6Hex(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
                break;
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
                c = handleOctal(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, policy);
                break;
                //
                // Simple escaped characters
                // \b \f \n \r \t
                //
            case 'b':
                c = WLCharacter(CODEPOINT_STRINGMETA_BACKSPACE, ESCAPE_SINGLE);
                break;
            case 'f':
                //
                // \f is NOT a space character (but inside of strings, it does have special meaning)
                //
                c = WLCharacter(CODEPOINT_STRINGMETA_FORMFEED, ESCAPE_SINGLE);
                break;
            case 'n':
                //
                // \n is NOT a newline character (but inside of strings, it does have special meaning)
                //
                c = WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
                break;
            case 'r':
                //
                // \r is NOT a newline character (but inside of strings, it does have special meaning)
                //
                c = WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
                break;
            case 't':
                //
                // \t is NOT a space character (but inside of strings, it does have special meaning)
                //
                c = WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
                break;
                //
                // \\ \" \< \>
                //
                // String meta characters
                // What are \< and \> ?
                // https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
                // https://stackoverflow.com/q/6065887
                //
            case '"':
                c = WLCharacter(CODEPOINT_STRINGMETA_DOUBLEQUOTE, ESCAPE_SINGLE);
                break;
            case '\\':
                c = handleBackSlash(escapedBuf, escapedLoc, policy);
                break;
            case '<':
                c = WLCharacter(CODEPOINT_STRINGMETA_OPEN, ESCAPE_SINGLE);
                break;
            case '>':
                c = WLCharacter(CODEPOINT_STRINGMETA_CLOSE, ESCAPE_SINGLE);
                break;
                //
                // Linear syntax characters
                // \! \% \& \( \) \* \+ \/ \@ \^ \_ \` \<space>
                //
            case '!':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_BANG, ESCAPE_SINGLE);
                break;
            case '%':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_PERCENT, ESCAPE_SINGLE);
                break;
            case '&':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_AMP, ESCAPE_SINGLE);
                break;
            case '(':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_OPENPAREN, ESCAPE_SINGLE);
                break;
            case ')':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_CLOSEPAREN, ESCAPE_SINGLE);
                break;
            case '*':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_STAR, ESCAPE_SINGLE);
                break;
            case '+':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_PLUS, ESCAPE_SINGLE);
                break;
            case '/':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_SLASH, ESCAPE_SINGLE);
                break;
            case '@':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_AT, ESCAPE_SINGLE);
                break;
            case '^':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_CARET, ESCAPE_SINGLE);
                break;
            case '_':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_UNDER, ESCAPE_SINGLE);
                break;
            case '`':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_BACKTICK, ESCAPE_SINGLE);
                break;
            case ' ':
                c = WLCharacter(CODEPOINT_LINEARSYNTAX_SPACE, ESCAPE_SINGLE);
                break;
                //
                // Anything else
                //
                // Something like \A or \{
                //
            default: {
                c = handleUnhandledEscape(currentWLCharacterStartBuf, currentWLCharacterStartLoc, escapedBuf, escapedLoc, curSource, policy);
                break;
            }
        }
    }
    
    //
    // Post-processing of WLCharacters
    //
    
#if !NISSUES
    if ((policy & ENABLE_STRANGE_CHARACTER_CHECKING) == ENABLE_STRANGE_CHARACTER_CHECKING) {
        
        if (c.isStrange() || c.isMBStrange()) {
            
            //
            // Just generally strange character is in the code
            //
            
            auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
            
            auto graphicalStr = c.graphicalString();
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", SYNTAXISSUESEVERITY_WARNING, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 0.95, {}));
            
            Issues.push_back(std::move(I));
        }
    }
#endif // !NISSUES
    
    return c;
}

//
//
//
WLCharacter CharacterDecoder::currentWLCharacter(NextCharacterPolicy policy) {
    
    auto resetBuf = TheByteBuffer->buffer;
    auto resetEOF = TheByteBuffer->wasEOF;
    
    auto resetLoc = TheByteDecoder->SrcLoc;
    
    auto c = nextWLCharacter0(policy);
    
    lastBuf = TheByteBuffer->buffer;
    lastLoc = TheByteDecoder->SrcLoc;
    
    TheByteBuffer->buffer = resetBuf;
    TheByteBuffer->wasEOF = resetEOF;
    TheByteDecoder->SrcLoc = resetLoc;
    
    return c;
}


//
//
//
WLCharacter CharacterDecoder::handleLongName(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer openSquareBuf, SourceLocation openSquareLoc, NextCharacterPolicy policy) {
    
    assert(*openSquareBuf == '[');
    
    //
    // Do not write leading \[ or trailing ] to LongName
    //
    auto longNameStartBuf = TheByteBuffer->buffer;
    
    auto curSource = TheByteDecoder->currentSourceCharacter(policy);
    
    auto wellFormed = false;
    
    auto atleast1DigitOrAlpha = false;
    
    //
    // Read at least 1 alnum before entering loop
    //
    if (curSource.isAlphaOrDigit()) {
        
        atleast1DigitOrAlpha = true;
        
        TheByteBuffer->buffer = TheByteDecoder->lastBuf;
        TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
        
        curSource = TheByteDecoder->currentSourceCharacter(policy);
        
        while (true) {
            
            //
            // No need to check isAbort() inside decoder loops
            //
            
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
            
            auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf, false);
            auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.buffer), longNameBufAndLen.length);
            
            if (atleast1DigitOrAlpha) {
                
                //
                // Something like \[A!]
                // Something like \[CenterDot\]
                //
                // Make the warning message a little more relevant
                //
                
                if (curSource.isEndOfFile()) {
                    
                    //
                    // Special case of \[A<EOF>
                    //
                    
                    auto suggestion = longNameSuggestion(longNameBufAndLen);
                    
                    std::vector<CodeActionPtr> Actions;
                    
                    if (!suggestion.empty()) {
                        Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\[" + suggestion + "]", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]")));
                    }
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\[") + longNameStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(curSource.to_point()).graphicalString();
                    
                    auto suggestion = longNameSuggestion(longNameBufAndLen);
                    
                    std::vector<CodeActionPtr> Actions;
                    
                    if (!suggestion.empty()) {
                        Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\[" + suggestion + "]", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]")));
                    }
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\[") + longNameStr + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
                
            } else {
                
                //
                // Malformed some other way
                //
                // Something like \[!
                // Something like \[*
                //
                
                if (curSource.isEndOfFile()) {
                    
                    //
                    // Special case of \[<EOF>
                    //
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\[" + longNameStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\[" + longNameStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\[") + longNameStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(curSource.to_point()).graphicalString();
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\[" + longNameStr + curSourceGraphicalStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\[" + longNameStr + curSourceGraphicalStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\[") + longNameStr + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
            }
        }
        //
        // Should we report "\\[]" as unlikely?
        //
//        else if (unlikelyEscapeChecking) {
//
//            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNLIKELYESCAPESEQUENCE, std::string("Unlikely escape sequence: ``\\\\[") + LongNameStr + "``", SYNTAXISSUESEVERITY_REMARK, Source(CharacterStart-1, Loc), 0.33, {}));
//
//            Issues.push_back(std::move(I));
//        }
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
    
    auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf, false);
    auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.buffer), longNameBufAndLen.length);
    
    auto it = LongNameToCodePointMap.find(longNameStr);
    auto found = (it != LongNameToCodePointMap.end());
    if (!found || ((policy & ENABLE_UNLIKELY_ESCAPE_CHECKING) == ENABLE_UNLIKELY_ESCAPE_CHECKING)) {
        
        //
        // Unrecognized name
        //
        // If found and unlikelyEscapeChecking, then still come in here.
        //
        
#if !NISSUES
        if (!found) {
            
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto longNameEndLoc = TheByteDecoder->SrcLoc;
                
                auto currentWLCharacterStartLoc = openSquareLoc - 1;
                
                auto currentWLCharacterEndLoc = longNameEndLoc;
                
                auto suggestion = longNameSuggestion(longNameBufAndLen);
                
                std::vector<CodeActionPtr> Actions;
                if (!suggestion.empty()) {
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\[" + suggestion + "]", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]")));
                }
                
                auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\[") + longNameStr + "]``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                
                Issues.push_back(std::move(I));
                
            } else if ((policy & ENABLE_UNLIKELY_ESCAPE_CHECKING) == ENABLE_UNLIKELY_ESCAPE_CHECKING) {
                
                auto currentWLCharacterStartLoc = openSquareLoc - 1;
                
                auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
                
                auto previousBackSlashLoc = currentWLCharacterStartLoc - 1;
                
                auto suggestion = longNameSuggestion(longNameBufAndLen);
                
                std::vector<CodeActionPtr> Actions;
                if (!suggestion.empty()) {
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\[" + suggestion + "]", Source(previousBackSlashLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]")));
                }
                
                auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNEXPECTEDESCAPESEQUENCE, std::string("Unexpected escape sequence: ``\\\\[") + longNameStr + "]``.", SYNTAXISSUESEVERITY_REMARK, Source(previousBackSlashLoc, currentWLCharacterEndLoc), 0.33, std::move(Actions)));
                
                Issues.push_back(std::move(I));
            }
        }
#endif // !NISSUES
        
        TheByteBuffer->buffer = openSquareBuf;
        TheByteDecoder->SrcLoc = openSquareLoc;
        
        return WLCharacter('\\');
    }
    
    //
    // Success!
    //
    
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheByteDecoder->lastLoc;
    
    auto point = it->second;
    
#if !NISSUES
    if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
        
        auto currentWLCharacterEndLoc = TheByteDecoder->SrcLoc;
        
        auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf, false);
        auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.buffer), longNameBufAndLen.length);
        
        //
        // The well-formed, recognized name could still be unsupported or undocumented
        //
        if (Utils::isUnsupportedLongName(longNameStr)) {
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNSUPPORTEDCHARACTER, std::string("Unsupported character: ``\\[") + longNameStr + "]``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 0.95, {}));
            
            Issues.push_back(std::move(I));
            
        } else if (Utils::isUndocumentedLongName(longNameStr)) {
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNDOCUMENTEDCHARACTER, std::string("Undocumented character: ``\\[") + longNameStr + "]``.", SYNTAXISSUESEVERITY_REMARK, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 0.95, {}));
            
            Issues.push_back(std::move(I));
        }
    }
#endif // !NISSUES
    
    if (isRaw(longNameStr)) {
        return WLCharacter(point, ESCAPE_RAW);
    } else {
        return WLCharacter(point, ESCAPE_LONGNAME);
    }
}

//
//
//
WLCharacter CharacterDecoder::handle4Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer colonBuf, SourceLocation colonLoc, NextCharacterPolicy policy) {
    
    assert(*colonBuf == ':');
    
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
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf, false);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length);
                
                if (curSource.isEndOfFile()) {
                    
                    //
                    // Special case of \:<EOF>
                    //
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\:" + hexStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\:" + hexStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\:") + hexStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(curSource.to_point()).graphicalString();
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\:" + hexStr + curSourceGraphicalStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\:" + hexStr + curSourceGraphicalStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\:") + hexStr + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
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
    
    auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
    
    auto hexEndBuf = currentWLCharacterEndBuf;
    
    auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf, false);
    auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length);
    
    auto it = ToSpecialMap.find(hexStr);
    if (it == ToSpecialMap.end()) {
        
        auto point = Utils::parseInteger(hexStr, 16);
        
        return WLCharacter(point, ESCAPE_4HEX);
    }
    
    auto point = it->second;
    
    return WLCharacter(point, ESCAPE_4HEX);
}

//
//
//
WLCharacter CharacterDecoder::handle2Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer dotBuf, SourceLocation dotLoc, NextCharacterPolicy policy) {
    
    assert(*dotBuf == '.');
    
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
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf, false);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length);
                
                if (curSource.isEndOfFile()) {
                    
                    //
                    // Special case of \.<EOF>
                    //
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\." + hexStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\." + hexStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, "Unrecognized character: ``\\." + hexStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(curSource.to_point()).graphicalString();
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\." + hexStr + curSourceGraphicalStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\." + hexStr + curSourceGraphicalStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, "Unrecognized character: ``\\." + hexStr + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
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
    
    auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
    
    auto hexEndBuf = currentWLCharacterEndBuf;
    
    auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf, false);
    auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length);
    
    auto it = ToSpecialMap.find(hexStr);
    if (it == ToSpecialMap.end()) {
        
        auto point = Utils::parseInteger(hexStr, 16);
        
        return WLCharacter(point, ESCAPE_2HEX);
    }
    
    auto point = it->second;
    
    return WLCharacter(point, ESCAPE_2HEX);
}

//
//
//
WLCharacter CharacterDecoder::handleOctal(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextCharacterPolicy policy) {
    
    assert(SourceCharacter(*firstOctalBuf).isOctal());
    
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
                
                auto octalBufAndLen = BufferAndLength(octalStartBuf, octalEndBuf - octalStartBuf, false);
                auto octalStr = std::string(reinterpret_cast<const char *>(octalBufAndLen.buffer), octalBufAndLen.length);
                
                if (curSource.isEndOfFile()) {
                    
                    //
                    // Special case of \0<EOF>
                    //
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\" + octalStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\" + octalStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\") + octalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(curSource.to_point()).graphicalString();
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\" + octalStr + curSourceGraphicalStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\" + octalStr + curSourceGraphicalStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\") + octalStr + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
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
    
    auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
    
    auto octalEndBuf = currentWLCharacterEndBuf;
    
    auto octalBufAndLen = BufferAndLength(octalStartBuf, octalEndBuf - octalStartBuf, false);
    auto octalStr = std::string(reinterpret_cast<const char *>(octalBufAndLen.buffer), octalBufAndLen.length);
    
    auto it = ToSpecialMap.find(octalStr);
    if (it == ToSpecialMap.end()) {
        
        auto point = Utils::parseInteger(octalStr, 8);
        
        return WLCharacter(point, ESCAPE_OCTAL);
    }
    
    auto point = it->second;
    
    return WLCharacter(point, ESCAPE_OCTAL);
}

//
//
//
WLCharacter CharacterDecoder::handle6Hex(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer barBuf, SourceLocation barLoc, NextCharacterPolicy policy) {
    
    assert(*barBuf == '|');
    
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
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf, false);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length);
                
                if (curSource.isEndOfFile()) {
                    
                    //
                    // Special case of \|<EOF>
                    //
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\|" + hexStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\|" + hexStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\|") + hexStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(curSource.to_point()).graphicalString();
                    
                    std::vector<CodeActionPtr> Actions;
                    Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\|" + hexStr + curSourceGraphicalStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\|" + hexStr + curSourceGraphicalStr)));
                    
                    auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character: ``\\|") + hexStr + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
                    
                    Issues.push_back(std::move(I));
                }
            }
#endif // !NISSUES

            TheByteBuffer->buffer = barBuf;
            TheByteDecoder->SrcLoc = barLoc;
            
            return WLCharacter('\\');
        }
    }
    
    //
    // Success!
    //
    
    auto currentWLCharacterEndBuf = TheByteBuffer->buffer;
    
    auto hexEndBuf = currentWLCharacterEndBuf;
    
    auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf, false);
    auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.buffer), hexBufAndLen.length);
    
    auto it = ToSpecialMap.find(hexStr);
    if (it == ToSpecialMap.end()) {
        
        auto point = Utils::parseInteger(hexStr, 16);
        
        return WLCharacter(point, ESCAPE_6HEX);
    }
    
    auto point = it->second;
    
    return WLCharacter(point, ESCAPE_6HEX);
}

//
// Handling line continuations belongs in some layer strictly above CharacterDecoder and below Tokenizer.
//
// Some middle layer that deals with "parts" of a token.
//
// But that layer doesn't exist (yet), so CharacterDecoder must handle line continuations.
//
// TODO: add this middle layer
//
// NOTE: this middle layer would need to warn about unneeded line continuations.
// e.g., with something like  { 123 \\\n }  then the line continuation is not needed
//
WLCharacter CharacterDecoder::handleLineContinuation(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer escapedBuf, SourceLocation escapedLoc, SourceCharacter firstChar, NextCharacterPolicy policy) {
    
    assert(*currentWLCharacterStartBuf == '\\');
    assert(firstChar.isNewline());
    
    if ((policy & LC_IS_MEANINGFUL) != LC_IS_MEANINGFUL) {

        //
        // Line continuation is NOT meaningful, so warn and return
        //
        // NOT meaningful, so do not worry about PRESERVE_WS_AFTER_LC
        //

#if !NISSUES
        //
        // Use ENABLE_CHARACTER_DECODING_ISSUES here to also talk about line continuations
        //
        // This disables unexpected line continuations inside comments
        //
        if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {

            //
            // Just remove the \, leave the \n
            //

            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new DeleteTextCodeAction("Delete \\", Source(currentWLCharacterStartLoc, escapedLoc))));

            auto I = IssuePtr(new FormatIssue(FORMATISSUETAG_UNEXPECTEDLINECONTINUATION, std::string("Unexpected line continuation."), FORMATISSUESEVERITY_FORMATTING, Source(currentWLCharacterStartLoc, escapedLoc), 0.0, std::move(Actions)));

            Issues.push_back(std::move(I));
        }
#endif // !NISSUES
        
        switch (firstChar.to_point()) {
            case '\n':
                return WLCharacter(CODEPOINT_LINECONTINUATION_LF, ESCAPE_SINGLE);
            case '\r':
                return WLCharacter(CODEPOINT_LINECONTINUATION_CR, ESCAPE_SINGLE);
            case CODEPOINT_CRLF:
                return WLCharacter(CODEPOINT_LINECONTINUATION_CRLF, ESCAPE_SINGLE);
            default:
                assert(false);
                return WLCharacter(CODEPOINT_UNKNOWN);
        }
    }
    
    auto c = currentWLCharacter(policy);
    
    if ((policy & PRESERVE_WS_AFTER_LC) != PRESERVE_WS_AFTER_LC) {
        
        while (c.isSpace()) {
            
            TheByteBuffer->buffer = lastBuf;
            
            c = currentWLCharacter(policy);
        }
    }
    
    //
    // Eat any more line continuations
    //
    // Yes, this is recursive, oh well
    //
    while (true) {
        
        if (!c.isMBLineContinuation()) {
            break;
        }
        
        TheByteBuffer->buffer = lastBuf;
        TheByteDecoder->SrcLoc = lastLoc;
        
        c = currentWLCharacter(policy);
        
        if ((policy & PRESERVE_WS_AFTER_LC) != PRESERVE_WS_AFTER_LC) {
            
            while (c.isSpace()) {
                
                TheByteBuffer->buffer = lastBuf;
                TheByteDecoder->SrcLoc = lastLoc;
                
                c = currentWLCharacter(policy);
            }
        }
    }
    
    TheByteBuffer->buffer = lastBuf;
    TheByteDecoder->SrcLoc = lastLoc;
    
    return c;
}

//
//
//
WLCharacter CharacterDecoder::handleBackSlash(Buffer escapedBuf, SourceLocation escapedLoc, NextCharacterPolicy policy) {
    
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
            
            tmpPolicy = tmpPolicy & ~ENABLE_CHARACTER_DECODING_ISSUES;
            
            handleLongName(escapedBuf, escapedLoc, resetBuf, resetLoc, tmpPolicy);
        }
        
        TheByteBuffer->buffer = resetBuf;
        TheByteDecoder->SrcLoc = resetLoc;
    }
#endif // !NISSUES
    
    return WLCharacter(CODEPOINT_STRINGMETA_BACKSLASH, ESCAPE_SINGLE);
}

//
//
//
WLCharacter CharacterDecoder::handleUnhandledEscape(Buffer currentWLCharacterStartBuf, SourceLocation currentWLCharacterStartLoc, Buffer unhandledBuf, SourceLocation unhandledLoc, SourceCharacter escapedChar, NextCharacterPolicy policy) {
    
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
        
        if (escapedChar.isUpper() && escapedChar.isHex()) {
            
            auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
            
            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\[" + curSourceGraphicalStr + "XXX]", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + curSourceGraphicalStr + "XXX]")));
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\:" + curSourceGraphicalStr + "XXX", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\:" + curSourceGraphicalStr + "XXX")));
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character ``\\") + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
            
            Issues.push_back(std::move(I));
            
        } else if (escapedChar.isUpper()) {
            
            auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
            
            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\[" + curSourceGraphicalStr + "XXX]", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + curSourceGraphicalStr + "XXX]")));
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character ``\\") + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
            
            Issues.push_back(std::move(I));
            
        } else if (escapedChar.isHex()) {
            
            auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
            
            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\:" + curSourceGraphicalStr + "xxx", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\:" + curSourceGraphicalStr + "xxx")));
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character ``\\") + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
            
            Issues.push_back(std::move(I));
            
        } else if (escapedChar.isEndOfFile()) {
            
            //
            // Do not know what a good suggestion would be for \<EOF>
            //
            
        } else {
            
            auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
            
            std::vector<CodeActionPtr> Actions;
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace with \\\\" + curSourceGraphicalStr, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\" + curSourceGraphicalStr)));
            
            auto I = IssuePtr(new SyntaxIssue(SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER, std::string("Unrecognized character ``\\") + curSourceGraphicalStr + "``.", SYNTAXISSUESEVERITY_ERROR, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, std::move(Actions)));
            
            Issues.push_back(std::move(I));
        }
    }
#endif // !NISSUES
    
    //
    // Keep these treated as 2 characters. This is how bad escapes are handled in WL strings.
    // And has the nice benefit of the single \ still giving an error at top-level
    //
    
    return WLCharacter('\\');
}


#if !NISSUES
std::vector<IssuePtr>& CharacterDecoder::getIssues() {
    return Issues;
}
#endif // !NISSUES


#if USE_MATHLINK
//
// example:
// input: Alpa
// return Alpha
//
// Return empty string if no suggestion.
//
std::string CharacterDecoder::longNameSuggestion(BufferAndLength input) {
    
    if (!libData) {
        return "";
    }
    
    MLINK link = libData->getMathLink(libData);
    if (!MLPutFunction(link, "EvaluatePacket", 1)) {
        assert(false);
    }
    if (!MLPutFunction(link, "AST`Library`LongNameSuggestion", 1)) {
        assert(false);
    }
    if (!MLPutUTF8String(link, input.buffer, static_cast<int>(input.length))) {
        assert(false);
    }
    if (!libData->processMathLink(link)) {
        assert(false);
    }
    auto pkt = MLNextPacket(link);
    if (pkt == RETURNPKT) {
        
        ScopedMLUTF8String str(link);
        if (!str.read()) {
            assert(false);
        }
        
        return reinterpret_cast<const char *>(str.get());
    }
    
    return "";
}

#else

std::string CharacterDecoder::longNameSuggestion(BufferAndLength input) {
    
    return "";
}

#endif // USE_MATHLINK

CharacterDecoderPtr TheCharacterDecoder = nullptr;

