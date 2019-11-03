
#pragma once

#include "Source.h"
#include "WLCharacter.h"

#include "WolframLibrary.h"

#include <vector>
#include <memory> // for unique_ptr

enum NextWLCharacterPolicyBits : uint8_t {
    
    //
    // Preserve whitespace after line continuation
    //
    // ToExpression["0.\\\n  6"] evaluates to 0.6 (whitespace is NOT preserved)
    //
    // But ToExpression["\"0.\\\n  6\""] evaluates to "0.  6" (whitespace IS preserved)
    //
    PRESERVE_WS_AFTER_LC = 0x01,
    
    //
    // Disable \ escapes for characters
    //
    // ToExpression["a>>>c:\\b"] evaluates to PutAppend[a, "c:\\b"] (escapes are DISABLED)
    //
    // ToExpression["\"c\\b\""] evaluates to "c\010" (escapes are ENABLED)
    //
    DISABLE_ESCAPES = 0x02,
    
    //
    // Disable character decoding issues
    //
    // "\c" gives a CharacterDecoding error (issues are ENABLED)
    //
    // (*\c*) does NOT give a CharacterDecoding error (issues are DISABLED)
    //
    DISABLE_CHARACTERDECODINGISSUES = 0x04,
    
    //
    // This code:
    // { a, \
    //   b }
    //
    // would give a line continuation warning because the line continuation is not meaningful
    //
    // This code:
    // { a, "x\
    //   y", b }
    //
    // would NOT give a warning because the line continuation is meaningful
    //
    LC_IS_MEANINGFUL = 0x08,
    
    //
    // After a line continuation, is \r \n consumed as a single newline?
    //
    // TODO: add to kernel quirks mode
    //
    LC_UNDERSTANDS_CRLF = 0x10,
    
    //
    // Check for unlikely escape sequences?
    //
    UNLIKELY_ESCAPE_CHECKING = 0x20,
};

class NextWLCharacterPolicy {
    uint8_t val;
public:
    constexpr NextWLCharacterPolicy(uint8_t val) : val(val) {}
    
    bool operator==(const WLCharacter &o) const = delete;
    
    bool operator!=(const WLCharacter &o) const = delete;
    
    NextWLCharacterPolicyBits operator&(const NextWLCharacterPolicyBits bits) const {
        return static_cast<NextWLCharacterPolicyBits>(val & bits);
    }
    
    NextWLCharacterPolicyBits operator|(const NextWLCharacterPolicyBits bits) const {
        return static_cast<NextWLCharacterPolicyBits>(val | bits);
    }
};

const NextWLCharacterPolicy TOPLEVEL              = LC_UNDERSTANDS_CRLF;
const NextWLCharacterPolicy INSIDE_SYMBOL         = LC_IS_MEANINGFUL | LC_UNDERSTANDS_CRLF;
const NextWLCharacterPolicy INSIDE_NUMBER         = LC_IS_MEANINGFUL | LC_UNDERSTANDS_CRLF;
const NextWLCharacterPolicy INSIDE_STRING         = PRESERVE_WS_AFTER_LC | LC_IS_MEANINGFUL | UNLIKELY_ESCAPE_CHECKING;
const NextWLCharacterPolicy INSIDE_STRINGIFY_FILE = DISABLE_ESCAPES | LC_IS_MEANINGFUL;
const NextWLCharacterPolicy INSIDE_STRINGIFY_LINE = LC_IS_MEANINGFUL | LC_UNDERSTANDS_CRLF;
const NextWLCharacterPolicy INSIDE_OPERATOR       = LC_IS_MEANINGFUL | LC_UNDERSTANDS_CRLF;
const NextWLCharacterPolicy INSIDE_COMMENT        = DISABLE_CHARACTERDECODINGISSUES | LC_UNDERSTANDS_CRLF;

//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences of Source Characters such as \[Alpha] into a single WL character
//
class CharacterDecoder {
    
    WLCharacter _currentWLCharacter;
    
    std::vector<std::pair<SourceCharacter, SourceLocation>> sourceCharacterQueue;
    
    std::vector<std::unique_ptr<Issue>> Issues;
    
    WolframLibraryData libData;
    
    SourceCharacter nextSourceCharacter();
    
    WLCharacter handleLongName(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextWLCharacterPolicy policy, bool unlikelyEscapeChecking);
    WLCharacter handle2Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextWLCharacterPolicy policy);
    WLCharacter handle4Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextWLCharacterPolicy policy);
    WLCharacter handle6Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextWLCharacterPolicy policy);
    WLCharacter handleOctal(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextWLCharacterPolicy policy);
    
    WLCharacter handleLineContinuation(NextWLCharacterPolicy policy);
    
    void append(SourceCharacter, SourceLocation);
    
    std::string longNameSuggestion(std::string);
    
public:
    CharacterDecoder();
    
    void init(WolframLibraryData libData);
    
    void deinit();
    
    WLCharacter nextWLCharacter(NextWLCharacterPolicy policy = TOPLEVEL);
    
    WLCharacter currentWLCharacter() const;
    
    std::vector<std::unique_ptr<Issue>>& getIssues();
};

extern std::unique_ptr<CharacterDecoder> TheCharacterDecoder;

