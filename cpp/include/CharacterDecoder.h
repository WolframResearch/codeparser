
#pragma once

#include "ByteDecoder.h"
#include "Source.h"
#include "CodePoint.h"

#include <ostream>
#include <vector>
#include <cassert>
#include <unordered_map>

//
// https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
//
#if defined NDEBUG
# define X_ASSERT(CHECK) void(0)
#else
# define X_ASSERT(CHECK) \
( (CHECK) ? void(0) : []{assert(false && #CHECK);}() )
#endif

//
// Used to just be Escape, but this was observed:
// c:\users\brenton\dropbox\wolfram\ast\ast\cpp\include\CharacterDecoder.h(37): error C2061: syntax error: identifier 'Escape'
//
enum EscapeFormat {
    ESCAPE_NONE,
    ESCAPE_SINGLE,
    ESCAPE_LONGNAME,
    ESCAPE_4HEX,
    ESCAPE_2HEX,
    ESCAPE_OCTAL,
    ESCAPE_6HEX,
};

//
// A single WL character
//
// The text  \[Alpha]  would be 1 WLCharacter
//
class WLCharacter {
public:
    explicit constexpr WLCharacter(int val, EscapeFormat escape_ = ESCAPE_NONE) : value_(val), escape_(escape_) {}

    explicit operator int() const noexcept = delete;

    bool operator==(const WLCharacter &o) const;

    bool operator!=(const WLCharacter &o) const;

    //
    // for std::map
    //
    bool operator<(const WLCharacter &o) const;

    constexpr char to_char() const {
        //
        // https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
        //
        return X_ASSERT(0x00 <= value_ && value_ <= 0xff), value_;
    }

    constexpr int to_point() const {
        return value_;
    }
    
    constexpr EscapeFormat escape() const {
        return escape_;
    }

    std::string string() const;
    
    bool isEscaped() const;
    
    bool isDigit() const;

    bool isAlpha() const;

    bool isDollar() const;
    
    bool isLetterlike() const;
    
    bool isStrangeLetterlike() const;
    
    bool isPunctuation() const;
    
    bool isSpace() const;
    
    bool isNewline() const;
    
    bool isEndOfFile() const;
    
    bool isLinearSyntax() const;
    
    bool isUninterpretable() const;
    
    bool isControl() const;
    
    bool isLetterlikeCharacter() const;
    bool isStrangeLetterlikeCharacter() const;
    bool isPunctuationCharacter() const;
    bool isSpaceCharacter() const;
    bool isNewlineCharacter() const;
    bool isCommaCharacter() const;
    bool isUninterpretableCharacter() const;
    
private:
    int value_;
    EscapeFormat escape_;
};

std::ostream& operator<<(std::ostream& stream, WLCharacter);



constexpr WLCharacter WLCHARACTER_ENDOFFILE(CODEPOINT_ENDOFFILE);



enum NextCharacterPolicyBits {
    
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
    // would give a line continuation warning because the line continuation is not needed
    //
    // This code:
    // { a, "x\
    //   y", b }
    //
    // would NOT give a warning because the line continuation is inside of a token
    //
    CURRENTLY_INSIDE_TOKEN = 0x08,
    
    //
    // After a line continuation, is \r \n consumed as a single newline?
    //
    // TODO: add to kernel quirks mode
    //
    LC_UNDERSTANDS_CRLF = 0x10
};

typedef int NextCharacterPolicy;

const NextCharacterPolicy TOPLEVEL               = LC_UNDERSTANDS_CRLF;
const NextCharacterPolicy INSIDE_SYMBOL          = CURRENTLY_INSIDE_TOKEN | LC_UNDERSTANDS_CRLF;
const NextCharacterPolicy INSIDE_NUMBER          = CURRENTLY_INSIDE_TOKEN | LC_UNDERSTANDS_CRLF;
const NextCharacterPolicy INSIDE_STRING          = PRESERVE_WS_AFTER_LC | CURRENTLY_INSIDE_TOKEN;
const NextCharacterPolicy INSIDE_STRING_FILEIFY  = DISABLE_ESCAPES | CURRENTLY_INSIDE_TOKEN;
const NextCharacterPolicy INSIDE_OPERATOR        = CURRENTLY_INSIDE_TOKEN | LC_UNDERSTANDS_CRLF;
const NextCharacterPolicy INSIDE_COMMENT         = PRESERVE_WS_AFTER_LC | DISABLE_CHARACTERDECODINGISSUES | CURRENTLY_INSIDE_TOKEN | LC_UNDERSTANDS_CRLF;

//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences such as \[Alpha] into a single WL character
//
class CharacterDecoder {

    WLCharacter cur;
    
    std::vector<std::pair<SourceCharacter, SourceLocation>> characterQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    WLCharacter handleLongName(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle2Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle4Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle6Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handleOctal(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    
    void enqueue(SourceCharacter, SourceLocation);
    
    static std::string longNameSuggestion(std::string);

public:
    CharacterDecoder();

    void init();

    void deinit();

    WLCharacter nextWLCharacter(NextCharacterPolicy policy = TOPLEVEL);

    WLCharacter currentWLCharacter() const;

    std::vector<SyntaxIssue> getIssues() const;
};

extern CharacterDecoder *TheCharacterDecoder;
