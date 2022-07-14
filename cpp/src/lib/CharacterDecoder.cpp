
#include "CharacterDecoder.h"

#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "Utils.h" // for isMBStrange, etc.
#include "LongNamesRegistration.h" // for LongNameToCodePointMap, etc.
#include "LongNames.h"
#include "MyStringRegistration.h"
#include "SymbolRegistration.h"
#include "ParserSession.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#endif // USE_EXPR_LIB

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

#include <algorithm> // for lower_bound

#if USE_MUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // USE_MUSTTAIL


WLCharacter CharacterDecoder_handleLongName(ParserSessionPtr session, Buffer openSquareBuf, SourceLocation openSquareLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handle2Hex(ParserSessionPtr session, Buffer dotBuf, SourceLocation dotLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handle4Hex(ParserSessionPtr session, Buffer colonBuf, SourceLocation colonLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handle6Hex(ParserSessionPtr session, Buffer barBuf, SourceLocation barLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleOctal(ParserSessionPtr session, Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleUnhandledEscape(ParserSessionPtr session, Buffer unhandledBuf, SourceLocation unhandledLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleBackslash(ParserSessionPtr session, NextPolicy policy);

WLCharacter CharacterDecoder_handleUncommon(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleAssertFalse(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaDoubleQuote(ParserSessionPtr session, Buffer Ignored1, SourceLocation Ignored2, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaOpen(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaClose(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy);
WLCharacter CharacterDecoder_handleStringMetaBackslash(ParserSessionPtr session, Buffer Ignored1, SourceLocation Ignored2, NextPolicy policy);

std::string CharacterDecoder_longNameSuggestion(ParserSessionPtr session, std::string input);


typedef WLCharacter (*HandlerFunction)(ParserSessionPtr session, Buffer startBuf, SourceLocation startLoc, NextPolicy policy);

#define U CharacterDecoder_handleUncommon
#define A CharacterDecoder_handleAssertFalse

std::array<HandlerFunction, 128> CharacterDecoderHandlerTable = {
    A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A,
    U, U, CharacterDecoder_handleStringMetaDoubleQuote, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, CharacterDecoder_handleStringMetaOpen, U, CharacterDecoder_handleStringMetaClose, U,
    U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, CharacterDecoder_handleStringMetaBackslash, U, U, U,
    U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, A,
};

#undef U
#undef A


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
WLCharacter CharacterDecoder_nextWLCharacter(ParserSessionPtr session, NextPolicy policy) {
    
    auto curSource = ByteDecoder_nextSourceCharacter(session, policy);
    
    auto point = curSource.to_point();
    
    if (point != '\\') {
        
#if DIAGNOSTICS
        CharacterDecoder_UnescapedCount++;
#endif // DIAGNOSTICS
        
        return WLCharacter(point);
    }
    
    //
    // Handle \
    //
    // handle escapes like special characters
    //
        
    //
    // There was a \
    //
    
    auto escapedBuf = session->buffer;
    auto escapedLoc = session->SrcLoc;
    
    curSource = ByteDecoder_currentSourceCharacter(session, policy);
    
    point = curSource.to_point();
    
    if (!(0x20 <= point && point <= 0x7e)) {
        
//            MUSTTAIL
        return CharacterDecoder_handleUncommon(session, escapedBuf, escapedLoc, policy);
    }
    
    return CharacterDecoderHandlerTable[point](session, escapedBuf, escapedLoc, policy);
}

WLCharacter CharacterDecoder_currentWLCharacter(ParserSessionPtr session, NextPolicy policy) {
    
    auto resetBuf = session->buffer;
    auto resetEOF = session->wasEOF;
    auto resetLoc = session->SrcLoc;
    
    auto c = CharacterDecoder_nextWLCharacter(session, policy);
    
    session->buffer = resetBuf;
    session->wasEOF = resetEOF;
    session->SrcLoc = resetLoc;
    
    return c;
}

WLCharacter CharacterDecoder_handleStringMetaDoubleQuote(ParserSessionPtr session, Buffer Ignored1, SourceLocation Ignored2, NextPolicy policy) {
#if DIAGNOSTICS
    CharacterDecoder_StringMetaDoubleQuoteCount++;
#endif // DIAGNOSTICS
                
    ByteDecoder_nextSourceCharacter(session, policy);
    
    return WLCharacter(CODEPOINT_STRINGMETA_DOUBLEQUOTE, ESCAPE_SINGLE);
}

//
// \\ \" \< \>
//
// String meta characters
// What are \< and \> ?
// https://mathematica.stackexchange.com/questions/105018/what-are-and-delimiters-in-box-expressions
// https://stackoverflow.com/q/6065887
//
WLCharacter CharacterDecoder_handleStringMetaOpen(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy) {
    
#if DIAGNOSTICS
    CharacterDecoder_StringMetaOpenCount++;
#endif // DIAGNOSTICS
                
    ByteDecoder_nextSourceCharacter(session, policy);
    
    auto c = WLCharacter(CODEPOINT_STRINGMETA_OPEN, ESCAPE_SINGLE);
                
#if CHECK_ISSUES
    {
        auto graphicalStr = c.graphicalString();
        
        auto currentWLCharacterStartLoc = escapedLoc.previous();
        
        auto currentWLCharacterEndLoc = session->SrcLoc;
        
        auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
        
        //
        // matched reduced severity of unexpected characters inside strings or comments
        //
        
        auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected string meta character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {"The kernel parses ``\"" + graphicalStr + "\"`` as an empty string."});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
                
    return c;
}

WLCharacter CharacterDecoder_handleStringMetaClose(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy) {
    
#if DIAGNOSTICS
    CharacterDecoder_StringMetaCloseCount++;
#endif // DIAGNOSTICS
                
    ByteDecoder_nextSourceCharacter(session, policy);
    
    auto c = WLCharacter(CODEPOINT_STRINGMETA_CLOSE, ESCAPE_SINGLE);
    
#if CHECK_ISSUES
    {
        auto graphicalStr = c.graphicalString();
        
        auto currentWLCharacterStartLoc = escapedLoc.previous();
        
        auto currentWLCharacterEndLoc = session->SrcLoc;
        
        auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
        
        //
        // matched reduced severity of unexpected characters inside strings or comments
        //
        
        auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected string meta character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {"The kernel parses ``\"" + graphicalStr + "\"`` as an empty string."});
        
        session->addIssue(I);
    }
#endif // CHECK_ISSUES
                
    return c;
}

WLCharacter CharacterDecoder_handleStringMetaBackslash(ParserSessionPtr session, Buffer Ignored1, SourceLocation Ignored2, NextPolicy policy) {
#if DIAGNOSTICS
    CharacterDecoder_StringMetaBackslashCount++;
#endif // DIAGNOSTICS
                
    ByteDecoder_nextSourceCharacter(session, policy);
    
//    MUSTTAIL
    return CharacterDecoder_handleBackslash(session, policy);
}

WLCharacter CharacterDecoder_handleLongName(ParserSessionPtr session, Buffer openSquareBuf, SourceLocation openSquareLoc, NextPolicy policy) {
    
    assert(*openSquareBuf == '[');
    
#if DIAGNOSTICS
    CharacterDecoder_LongNameCount++;
#endif // DIAGNOSTICS
    
    //
    // Do not write leading \[ or trailing ] to LongName
    //
    auto longNameStartBuf = session->buffer;
    
    auto curSource = ByteDecoder_currentSourceCharacter(session, policy);
    
    auto wellFormed = false;
    
    auto atleast1DigitOrAlpha = false;
    
    //
    // Read at least 1 alnum before entering loop
    //
    // Must start with upper
    //
    if (curSource.isUpper()) {
        
        atleast1DigitOrAlpha = true;
        
        ByteDecoder_nextSourceCharacter(session, policy);
        
        curSource = ByteDecoder_currentSourceCharacter(session, policy);
        
        while (true) {
            
            if (curSource.isAlphaOrDigit()) {
                
                ByteDecoder_nextSourceCharacter(session, policy);
                
                curSource = ByteDecoder_currentSourceCharacter(session, policy);
                
            } else if (curSource.to_point() == ']') {
                
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
        
        ByteDecoder_nextSourceCharacter(session, policy);
        
        curSource = ByteDecoder_currentSourceCharacter(session, policy);
    }
    
    if (!wellFormed) {
        
        //
        // Not well-formed
        //
        
#if CHECK_ISSUES
        if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
            
            auto currentWLCharacterStartLoc = openSquareLoc.previous();
            
            auto currentWLCharacterEndBuf = session->buffer;
            auto currentWLCharacterEndLoc = session->SrcLoc;
            
            auto longNameEndBuf = currentWLCharacterEndBuf;
            
            auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
            auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.Buf), longNameBufAndLen.length());
            
            if (atleast1DigitOrAlpha) {
                
                //
                // Something like \[Alpha
                //
                // Make the warning message a little more relevant
                //
                
                auto suggestion = CharacterDecoder_longNameSuggestion(session, longNameStr);
                
                CodeActionPtrVector Actions;
                
                auto it = std::lower_bound(LongNameToCodePointMap_names.begin(), LongNameToCodePointMap_names.end(), longNameStr);
                auto found = (it != LongNameToCodePointMap_names.end() && *it == longNameStr);
                if (found) {
                    Actions.push_back(new InsertTextCodeAction("Insert ``]`` to form ``\\[" + suggestion + "]``", Source(currentWLCharacterEndLoc), "]"));
                }
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\[") + longNameStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
                
            } else {
                
                //
                // Malformed some other way
                //
                // Something like \[!
                // Something like \[*
                //
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\\\[" + longNameStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\[" + longNameStr));
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\[") + longNameStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
            }
        }
#endif // CHECK_ISSUES
        
        session->buffer = openSquareBuf;
        session->SrcLoc = openSquareLoc;
        
        return WLCharacter('\\');
    }
    
    //
    // Well-formed
    //
    
    //
    // if unlikelyEscapeChecking, then make sure to append all of the Source characters again
    //
    
    auto longNameEndBuf = session->buffer;
    
    auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
    
    auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.Buf), longNameBufAndLen.length());
    
    auto it = std::lower_bound(LongNameToCodePointMap_names.begin(), LongNameToCodePointMap_names.end(), longNameStr);
    
    auto found = (it != LongNameToCodePointMap_names.end() && *it == longNameStr);
    
    if (!found) {
        
        //
        // Unrecognized name
        //
        
#if CHECK_ISSUES
        if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
            
            auto longNameEndLoc = session->SrcLoc;
            
            auto currentWLCharacterStartLoc = openSquareLoc.previous();
            
            //
            // Accomodate the ] character
            //
            auto currentWLCharacterEndLoc = longNameEndLoc.next();
            
            auto suggestion = CharacterDecoder_longNameSuggestion(session, longNameStr);
            
            CodeActionPtrVector Actions;
            
            if (!suggestion.empty()) {
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\[" + suggestion + "]``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + suggestion + "]"));
            }
            
            //
            // More specifically: Unrecognized
            //
            auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\[") + longNameStr + "]``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {"``" + longNameStr + "`` is not a recognized long name."});
            
            session->addIssue(I);
            
        }
#endif // CHECK_ISSUES
        
        session->buffer = openSquareBuf;
        session->SrcLoc = openSquareLoc;
        
        return WLCharacter('\\');
    }
    
    //
    // Success!
    //
    
    ByteDecoder_nextSourceCharacter(session, policy);
    
    auto idx = it - LongNameToCodePointMap_names.begin();
    auto point = LongNameToCodePointMap_points[idx];
    
#if CHECK_ISSUES
    if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
        
        auto longNameBufAndLen = BufferAndLength(longNameStartBuf, longNameEndBuf - longNameStartBuf);
        auto longNameStr = std::string(reinterpret_cast<const char *>(longNameBufAndLen.Buf), longNameBufAndLen.length());
        
        if (Utils::isStrange(point)) {
            
            //
            // Just generally strange character is in the code
            //
            auto c = WLCharacter(point, LongNames::isRaw(longNameStr) ? ESCAPE_RAW : ESCAPE_LONGNAME);
            
            auto currentWLCharacterStartLoc = openSquareLoc.previous();
            
            auto currentSourceCharacterEndLoc = session->SrcLoc;
            
            auto graphicalStr = c.graphicalString();
            
            auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
            
            CodeActionPtrVector Actions;
            
            for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                Actions.push_back(A);
            }
            
            //
            // do not recommend replacing graphical character with literal version
            //
            
            //
            // any ASCII replacements
            //
            for (const auto& r : LongNames::asciiReplacements(point)) {
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
            }
            
            if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
                
                //
                // reduce severity of unexpected characters inside strings or comments
                //
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, Actions, {});
                
                session->addIssue(I);
                
            } else if (c.isStrangeWhitespace()) {
                
                ;
                
            } else {
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, Actions, {});
                
                session->addIssue(I);
            }
            
        } else if (Utils::isMBStrange(point)) {
            
            //
            // Just generally strange character is in the code
            //
            auto c = WLCharacter(point, LongNames::isRaw(longNameStr) ? ESCAPE_RAW : ESCAPE_LONGNAME);
            
            auto currentWLCharacterStartLoc = openSquareLoc.previous();
            
            auto currentSourceCharacterEndLoc = session->SrcLoc;
            
            auto graphicalStr = c.graphicalString();
            
            auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
            
            CodeActionPtrVector Actions;
            
            for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
                Actions.push_back(A);
            }
            
            //
            // do not recommend replacing graphical character with literal version
            //
            
            //
            // any ASCII replacements
            //
            for (const auto& r : LongNames::asciiReplacements(point)) {
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
            }
            
            if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
                
                //
                // reduce severity of unexpected characters inside strings or comments
                //
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, Actions, {});
                
                session->addIssue(I);
                
            } else if (c.isMBStrangeWhitespace()) {
                
                ;
                
            } else {
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, Actions, {});
                
                session->addIssue(I);
            }
        }
    }
#endif // CHECK_ISSUES
    
    if (LongNames::isRaw(longNameStr)) {
        return WLCharacter(point, ESCAPE_RAW);
    }
    
    return WLCharacter(point, ESCAPE_LONGNAME);
}


WLCharacter CharacterDecoder_handle4Hex(ParserSessionPtr session, Buffer colonBuf, SourceLocation colonLoc, NextPolicy policy) {
    
    assert(*colonBuf == ':');
    
#if DIAGNOSTICS
    CharacterDecoder_4HexCount++;
#endif // DIAGNOSTICS
    
    auto hexStartBuf = session->buffer;
    
    for (auto i = 0; i < 4; i++) {
        
        auto curSource = ByteDecoder_currentSourceCharacter(session, policy);
        
        if (curSource.isHex()) {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \:z
            //
            
#if CHECK_ISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterStartLoc = colonLoc.previous();
                
                auto currentWLCharacterEndBuf = session->buffer;
                auto currentWLCharacterEndLoc = session->SrcLoc;
                
                auto hexEndBuf = currentWLCharacterEndBuf;
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.Buf), hexBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\\\:" + hexStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\:" + hexStr));
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\:") + hexStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES
            
            session->buffer = colonBuf;
            session->SrcLoc = colonLoc;
            
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
    
#if CHECK_ISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_4HEX);
        
        auto currentWLCharacterStartLoc = colonLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, Actions, {});
            
            session->addIssue(I);
        }
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_4HEX);
        
        auto currentWLCharacterStartLoc = colonLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, Actions, {});
            
            session->addIssue(I);
        }
    }
#endif // CHECK_ISSUES
    
    return WLCharacter(point, ESCAPE_4HEX);
}


WLCharacter CharacterDecoder_handle2Hex(ParserSessionPtr session, Buffer dotBuf, SourceLocation dotLoc, NextPolicy policy) {
    
    assert(*dotBuf == '.');
    
#if DIAGNOSTICS
    CharacterDecoder_2HexCount++;
#endif // DIAGNOSTICS
    
    auto hexStartBuf = session->buffer;
    
    for (auto i = 0; i < 2; i++) {
        
        auto curSource = ByteDecoder_currentSourceCharacter(session, policy);
        
        if (curSource.isHex()) {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \.z
            //
            
#if CHECK_ISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterStartLoc = dotLoc.previous();
                
                auto currentWLCharacterEndBuf = session->buffer;
                auto currentWLCharacterEndLoc = session->SrcLoc;
                
                auto hexEndBuf = currentWLCharacterEndBuf;
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.Buf), hexBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\\\." + hexStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\." + hexStr));
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, "Unhandled character: ``\\." + hexStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES
            
            session->buffer = dotBuf;
            session->SrcLoc = dotLoc;
            
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
    
#if CHECK_ISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_2HEX);
        
        auto currentWLCharacterStartLoc = dotLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, Actions, {});
            
            session->addIssue(I);
        }
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_2HEX);
        
        auto currentWLCharacterStartLoc = dotLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, Actions, {});
            
            session->addIssue(I);
        }
    };
#endif // CHECK_ISSUES
    
    return WLCharacter(point, ESCAPE_2HEX);
}


WLCharacter CharacterDecoder_handleOctal(ParserSessionPtr session, Buffer firstOctalBuf, SourceLocation firstOctalLoc, NextPolicy policy) {
    
    assert(SourceCharacter(*firstOctalBuf).isOctal());
    
#if DIAGNOSTICS
    CharacterDecoder_OctalCount++;
#endif // DIAGNOSTICS
    
    auto octalStartBuf = firstOctalBuf;
    
    for (auto i = 0; i < 3-1; i++) {
        
        auto curSource = ByteDecoder_currentSourceCharacter(session, policy);
        
        if (curSource.isOctal()) {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \1z
            //
            
#if CHECK_ISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterStartLoc = firstOctalLoc.previous();
                
                auto currentWLCharacterEndBuf = session->buffer;
                auto currentWLCharacterEndLoc = session->SrcLoc;
                
                auto octalEndBuf = currentWLCharacterEndBuf;
                
                auto octalBufAndLen = BufferAndLength(octalStartBuf, octalEndBuf - octalStartBuf);
                auto octalStr = std::string(reinterpret_cast<const char *>(octalBufAndLen.Buf), octalBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\\\" + octalStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\" + octalStr));
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\") + octalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES

            session->buffer = firstOctalBuf;
            session->SrcLoc = firstOctalLoc;
            
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
    
#if CHECK_ISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_OCTAL);
        
        auto currentWLCharacterStartLoc = firstOctalLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, Actions, {});
            
            session->addIssue(I);
        }
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_OCTAL);
        
        auto currentWLCharacterStartLoc = firstOctalLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, Actions, {});
            
            session->addIssue(I);
        }
    };
#endif // CHECK_ISSUES
    
    return WLCharacter(point, ESCAPE_OCTAL);
}


WLCharacter CharacterDecoder_handle6Hex(ParserSessionPtr session, Buffer barBuf, SourceLocation barLoc, NextPolicy policy) {
    
    assert(*barBuf == '|');
    
#if DIAGNOSTICS
    CharacterDecoder_6HexCount++;
#endif // DIAGNOSTICS
    
    auto hexStartBuf = session->buffer;
    
    for (auto i = 0; i < 6; i++) {
        
        auto curSource = ByteDecoder_currentSourceCharacter(session, policy);
        
        if (curSource.isHex()) {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
        } else {
            
            //
            // Not well-formed
            //
            // Something like \|z
            //
            
#if CHECK_ISSUES
            if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
                
                auto currentWLCharacterStartLoc = barLoc.previous();
                
                auto currentWLCharacterEndBuf = session->buffer;
                auto currentWLCharacterEndLoc = session->SrcLoc;
                
                auto hexEndBuf = currentWLCharacterEndBuf;
                
                auto hexBufAndLen = BufferAndLength(hexStartBuf, hexEndBuf - hexStartBuf);
                auto hexStr = std::string(reinterpret_cast<const char *>(hexBufAndLen.Buf), hexBufAndLen.length());
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\\\|" + hexStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\|" + hexStr));
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character: ``\\|") + hexStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES

            session->buffer = barBuf;
            session->SrcLoc = barLoc;
            
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
        
        session->buffer = barBuf;
        session->SrcLoc = barLoc;
        
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
    
#if CHECK_ISSUES
    if (Utils::isStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_6HEX);
        
        auto currentWLCharacterStartLoc = barLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.95, Actions, {});
            
            session->addIssue(I);
        }
        
    } else if (Utils::isMBStrange(point)) {
        //
        // Just generally strange character is in the code
        //
        
        auto c = WLCharacter(point, ESCAPE_6HEX);
        
        auto currentWLCharacterStartLoc = barLoc.previous();
        
        auto currentSourceCharacterEndLoc = session->SrcLoc;
        
        auto graphicalStr = c.graphicalString();
        
        auto Src = Source(currentWLCharacterStartLoc, currentSourceCharacterEndLoc);
        
        CodeActionPtrVector Actions;
        
        for (auto& A : Utils::certainCharacterReplacementActions(c, Src)) {
            Actions.push_back(A);
        }
        
        //
        // do not recommend replacing graphical character with literal version
        //
        
        //
        // any ASCII replacements
        //
        for (const auto& r : LongNames::asciiReplacements(point)) {
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``" + LongNames::replacementGraphical(r) + "``", Src, r));
        }
        
        if ((policy & STRING_OR_COMMENT) == STRING_OR_COMMENT) {
            
            //
            // reduce severity of unexpected characters inside strings or comments
            //
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.85, Actions, {});
            
            session->addIssue(I);
            
        } else if (c.isMBStrangeWhitespace()) {
            
            ;
            
        } else {
            
            auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_WARNING, Src, 0.85, Actions, {});
            
            session->addIssue(I);
        }
    };
#endif // CHECK_ISSUES
    
    return WLCharacter(point, ESCAPE_6HEX);
}


WLCharacter CharacterDecoder_handleBackslash(ParserSessionPtr session, NextPolicy policy) {
    
#if CHECK_ISSUES
    //
    // test whether this \ is the result of the "feature" of
    // converting "\[Alpa]" into "\\[Alpa]", copying that, and then never giving any further warnings
    // when dealing with "\\[Alpa]"
    //
    {
        auto resetBuf = session->buffer;
        auto resetLoc = session->SrcLoc;
        
        //
        // will be resetting any way, so just use nextSourceCharacter here
        //
        auto c = ByteDecoder_nextSourceCharacter(session, policy);
        
        if (c.to_point() == '[') {
            
            //
            // Try to reconstruct \[XXX]
            //
            // If well-formed, then warn
            //
            
            auto longNameStartBuf = session->buffer;
            auto longNameStartLoc = session->SrcLoc;
            
            c = ByteDecoder_nextSourceCharacter(session, policy);
            
            auto wellFormed = false;
            
            if (c.isUpper()) {
                
                c = ByteDecoder_nextSourceCharacter(session, policy);
                
                while (true) {
                    
                    if (c.isAlphaOrDigit()) {
                        
                        c = ByteDecoder_nextSourceCharacter(session, policy);
                        
                        continue;
                    }
                    
                    if (c.to_point() == ']') {
                        wellFormed = true;
                    }
                    
                    break;
                }
            }
            
            if (wellFormed) {
                
                auto tmpPolicy = policy;
                
                tmpPolicy |= ENABLE_CHARACTER_DECODING_ISSUES;
                
                session->buffer = longNameStartBuf;
                session->SrcLoc = longNameStartLoc;
                
                CharacterDecoder_handleLongName(session, resetBuf, resetLoc, tmpPolicy);
            }
        }
        
        session->buffer = resetBuf;
        session->SrcLoc = resetLoc;
    }
#endif // CHECK_ISSUES
    
    return WLCharacter(CODEPOINT_STRINGMETA_BACKSLASH, ESCAPE_SINGLE);
}


WLCharacter CharacterDecoder_handleUnhandledEscape(ParserSessionPtr session, Buffer unhandledBuf, SourceLocation unhandledLoc, NextPolicy policy) {
    
    //
    // Anything else
    //
    // Something like  \A
    //
    
    auto escapedChar = ByteDecoder_currentSourceCharacter(session, policy);
    
    ByteDecoder_nextSourceCharacter(session, policy);
    
#if CHECK_ISSUES
    //
    // Make the warnings a little more relevant
    //
    
    if ((policy & ENABLE_CHARACTER_DECODING_ISSUES) == ENABLE_CHARACTER_DECODING_ISSUES) {
        
        auto currentWLCharacterStartLoc = unhandledLoc.previous();
        
        auto currentWLCharacterEndLoc = session->SrcLoc;
        
        if (escapedChar.isUpper()) {
            
            //
            // Attempt to read \Alpha] and report a missing [
            //
            
            std::string alnumRun;
            
            alnumRun += escapedChar.to_point();
            
            auto curSource = ByteDecoder_currentSourceCharacter(session, policy);
            
            auto wellFormed = false;
            
            while (true) {
                
                if (curSource.isAlphaOrDigit()) {
                    
                    alnumRun += curSource.to_point();
                    
                    ByteDecoder_nextSourceCharacter(session, policy);
                    
                    curSource = ByteDecoder_currentSourceCharacter(session, policy);
                    
                } else if (curSource.to_point() == ']') {
                    
                    ByteDecoder_nextSourceCharacter(session, policy);
                    
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
                
                currentWLCharacterEndLoc = session->SrcLoc;
                
                auto curSourceGraphicalStr = alnumRun + "]";
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new InsertTextCodeAction("Insert ``[`` to form ``\\[" + alnumRun + "]``", Source(currentWLCharacterStartLoc.next()), "["));
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
                
            } else {
                
                if (escapedChar.isHex()) {
                    
                    auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\[" + curSourceGraphicalStr + "XXX]``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + curSourceGraphicalStr + "XXX]"));
                    
                    Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\:" + curSourceGraphicalStr + "XXX``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\:" + curSourceGraphicalStr + "XXX"));
                    
                    auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                    
                    session->addIssue(I);
                    
                } else {
                    
                    auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
                    
                    CodeActionPtrVector Actions;
                    
                    Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\[" + curSourceGraphicalStr + "XXX]``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\[" + curSourceGraphicalStr + "XXX]"));
                    
                    auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                    
                    session->addIssue(I);
                }
            }
            
        } else if (escapedChar.isHex()) {
            
            auto curSourceGraphicalStr = WLCharacter(escapedChar.to_point()).graphicalString();
            
            CodeActionPtrVector Actions;
            
            Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\:" + curSourceGraphicalStr + "xxx``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\:" + curSourceGraphicalStr + "xxx"));
            
            auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
            
            session->addIssue(I);
            
        } else if (escapedChar.isEndOfFile()) {
            
            //
            // Do not know what a good suggestion would be for \<EOF>
            //
            
        } else if (escapedChar.isMBUnsafeUTF8Sequence()) {
            
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
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, {}, {});
                
                session->addIssue(I);
                
            } else {
                
                CodeActionPtrVector Actions;
                
                Actions.push_back(new ReplaceTextCodeAction("Replace with ``\\\\" + curSourceGraphicalStr + "``", Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), "\\\\" + curSourceGraphicalStr));
                
                auto I = new SyntaxIssue(STRING_UNHANDLEDCHARACTER, std::string("Unhandled character ``\\") + curSourceGraphicalStr + "``.", STRING_FATAL, Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc), 1.0, Actions, {});
                
                session->addIssue(I);
            }
        }
    }
#endif // CHECK_ISSUES
    
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
    
    session->buffer = unhandledBuf;
    session->SrcLoc = unhandledLoc;
    
    return WLCharacter('\\');
}

WLCharacter CharacterDecoder_handleAssertFalse(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy) {

    assert(false);
    
    return WLCharacter{0};
}

WLCharacter CharacterDecoder_handleUncommon(ParserSessionPtr session, Buffer escapedBuf, SourceLocation escapedLoc, NextPolicy policy) {
    
    auto curSource = ByteDecoder_currentSourceCharacter(session, policy);
    
    switch (curSource.to_point()) {
        case '\n': {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINECONTINUATION_LINEFEED, ESCAPE_SINGLE);
        }
        case '\r': {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINECONTINUATION_CARRIAGERETURN, ESCAPE_SINGLE);
        }
        case CODEPOINT_CRLF: {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINECONTINUATION_CRLF, ESCAPE_SINGLE);
        }
        case '[': {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
//            MUSTTAIL
            return CharacterDecoder_handleLongName(session, escapedBuf, escapedLoc, policy);
        }
        case ':': {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
//            MUSTTAIL
            return CharacterDecoder_handle4Hex(session, escapedBuf, escapedLoc, policy);
        }
        case '.': {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
//            MUSTTAIL
            return CharacterDecoder_handle2Hex(session, escapedBuf, escapedLoc, policy);
        }
        case '|': {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
//            MUSTTAIL
            return CharacterDecoder_handle6Hex(session, escapedBuf, escapedLoc, policy);
        }
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
//            MUSTTAIL
            return CharacterDecoder_handleOctal(session, escapedBuf, escapedLoc, policy);
        }
    
            //
            // Simple escaped characters
            // \b \f \n \r \t
            //
        case 'b': {
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaBackspaceCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            auto c = WLCharacter(CODEPOINT_STRINGMETA_BACKSPACE, ESCAPE_SINGLE);
                        
#if CHECK_ISSUES
            {
                auto graphicalStr = c.graphicalString();
                
                auto currentWLCharacterStartLoc = escapedLoc.previous();
                
                auto currentWLCharacterEndLoc = session->SrcLoc;
                
                auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
                
                //
                // matched reduced severity of unexpected characters inside strings or comments
                //
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES
                        
            return c;
        }

        case 'f': {
    
            //
            // \f is NOT a space character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaFormFeedCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            auto c = WLCharacter(CODEPOINT_STRINGMETA_FORMFEED, ESCAPE_SINGLE);
            
#if CHECK_ISSUES
            {
                auto graphicalStr = c.graphicalString();
                
                auto currentWLCharacterStartLoc = escapedLoc.previous();
                
                auto currentWLCharacterEndLoc = session->SrcLoc;
                
                auto Src = Source(currentWLCharacterStartLoc, currentWLCharacterEndLoc);
                
                //
                // matched reduced severity of unexpected characters inside strings or comments
                //
                
                auto I = new SyntaxIssue(STRING_UNEXPECTEDCHARACTER, "Unexpected character: ``" + graphicalStr + "``.", STRING_REMARK, Src, 0.95, {}, {});
                
                session->addIssue(I);
            }
#endif // CHECK_ISSUES
            
            return c;
        }
            
        case 'n': {
            //
            // \n is NOT a newline character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaLineFeedCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_STRINGMETA_LINEFEED, ESCAPE_SINGLE);
        }
            
        case 'r': {
            //
            // \r is NOT a newline character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaCarriageReturnCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_STRINGMETA_CARRIAGERETURN, ESCAPE_SINGLE);
        }
            
        case 't': {
            //
            // \t is NOT a space character (but inside of strings, it does have special meaning)
            //
            
#if DIAGNOSTICS
            CharacterDecoder_StringMetaTabCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_STRINGMETA_TAB, ESCAPE_SINGLE);
        }
        //
        // Linear syntax characters
        // \! \% \& \( \) \* \+ \/ \@ \^ \_ \` \<space>
        //
        case '!': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxBangCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_BANG, ESCAPE_SINGLE);
        }
        case '%': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxPercentCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_PERCENT, ESCAPE_SINGLE);
        }
        case '&': {
        
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxAmpCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_AMP, ESCAPE_SINGLE);
        }
        case '(': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxOpenParenCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_OPENPAREN, ESCAPE_SINGLE);
        }
        case ')': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxCloseParenCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_CLOSEPAREN, ESCAPE_SINGLE);
        }
        case '*': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxStarCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_STAR, ESCAPE_SINGLE);
        }
        case '+': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxPlusCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_PLUS, ESCAPE_SINGLE);
        }
        case '/': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxSlashCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_SLASH, ESCAPE_SINGLE);
        }
        case '@': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxAtCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_AT, ESCAPE_SINGLE);
        }
        case '^': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxCaretCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_CARET, ESCAPE_SINGLE);
        }
        case '_': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxUnderscoreCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_UNDER, ESCAPE_SINGLE);
        }
        case '`': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxBacktickCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_BACKTICK, ESCAPE_SINGLE);
        }
        case ' ': {
            
#if DIAGNOSTICS
            CharacterDecoder_LinearSyntaxSpaceCount++;
#endif // DIAGNOSTICS
            
            ByteDecoder_nextSourceCharacter(session, policy);
            
            return WLCharacter(CODEPOINT_LINEARSYNTAX_SPACE, ESCAPE_SINGLE);
        }
    } // switch
    
    //
    // Anything else
    //
    // Something like \A or \{
    //
#if DIAGNOSTICS
    CharacterDecoder_UnhandledCount++;
#endif // DIAGNOSTICS
    
//    MUSTTAIL
    return CharacterDecoder_handleUnhandledEscape(session, escapedBuf, escapedLoc, policy);
}


//
// example:
// input: Alpa
// return Alpha
//
// Return empty string if no suggestion.
//
#if USE_EXPR_LIB
std::string CharacterDecoder_longNameSuggestion(ParserSessionPtr session, std::string input) {
    
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
std::string CharacterDecoder_longNameSuggestion(ParserSessionPtr session, std::string input) {
    
    auto sessionLink = session->getSessionMathLink();
    
    if (!MLPutFunction(sessionLink, SYMBOL_EVALUATEPACKET.Name, 1)) {
        assert(false);
    }
    
    if (!MLPutFunction(sessionLink, SYMBOL_CODEPARSER_LIBRARY_LONGNAMESUGGESTION.Name, 1)) {
        assert(false);
    }
    
    if (!MLPutUTF8String(sessionLink, reinterpret_cast<Buffer>(input.c_str()), static_cast<int>(input.size()))) {
        assert(false);
    }
    
    if (!session->libData->processMathLink(sessionLink)) {
        assert(false);
    }
    
    auto pkt = MLNextPacket(sessionLink);
    
    if (pkt == RETURNPKT) {
        
        ScopedMLString str(sessionLink);
        
        if (!str.read()) {
            assert(false);
        }
        
        auto cstr = str.get();
        
        return std::string(cstr);
    }
    
    return "";
}
#else
std::string CharacterDecoder_longNameSuggestion(ParserSessionPtr session, std::string input) {
    return "";
}
#endif // USE_EXPR_LIB
