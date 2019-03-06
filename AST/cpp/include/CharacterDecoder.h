
#pragma once

#include "ByteDecoder.h"
#include "SyntaxIssue.h"
#include "SourceManager.h"
#include "CodePoint.h"

#include <sstream>
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

enum Escape {
    ESCAPE_NONE,
    ESCAPE_SINGLE,
    ESCAPE_LONGNAME,
    ESCAPE_4HEX,
    ESCAPE_2HEX,
    ESCAPE_OCTAL,
    ESCAPE_6HEX,
};

class WLCharacter
{
public:
    explicit constexpr WLCharacter(int val, Escape escape_ = ESCAPE_NONE) : value_(val), escape_(escape_) {}

    explicit operator int() const noexcept = delete;

    bool operator==(const WLCharacter &o) const {
        return value_ == o.value_ && escape_ == o.escape_;
    }

    bool operator!=(const WLCharacter &o) const {
        return value_ != o.value_ || escape_ != o.escape_;
    }

    //
    // for std::map
    //
    bool operator<(const WLCharacter &o) const {
        return value_ < o.value_;
    }

    constexpr char to_char() const {
        //
        // https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
        //
        return X_ASSERT(0x00 <= value_ && value_ <= 0xff), value_;
    }

    constexpr int to_point() const {
        return value_;
    }
    
    std::string string() const;

    std::vector<SourceCharacter> source() const;

    bool isEscaped() const {
        return escape_ != ESCAPE_NONE;
    }
    
    bool isDigit() const;

    bool isAlpha() const;

    bool isDigitOrAlpha() const;

    bool isDigitOrAlphaOrDollar() const;

    bool isAlphaOrDollar() const;

    bool isHex() const;

    bool isOctal() const;
    
    bool isPunctuation() const;
    
    bool isLinearSyntax() const;
    
    bool isLetterlikeCharacter() const;
    bool isStrangeLetterlikeCharacter() const;
    bool isOperatorCharacter() const;
    bool isSpaceCharacter() const;
    bool isNewlineCharacter() const;
    bool isCommaCharacter() const;
    bool isUninterpretableCharacter() const;

    int toDigit() const;

    static int fromDigit(int d);

private:
    int value_;
    Escape escape_;
};

 namespace std {
     //
     // for std::unordered_set
     //
     template <> struct hash<WLCharacter> {
         size_t operator()(const WLCharacter &x) const {
             return hash<int>()(x.to_point());
         }
     };
 }

std::ostream& operator<<(std::ostream& stream, const WLCharacter&);



constexpr WLCharacter WLCHARACTER_EOF(CODEPOINT_EOF);



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
    DISABLE_ESCAPES = 0x02
};

typedef int NextCharacterPolicy;

const NextCharacterPolicy TOPLEVEL       = 0;
const NextCharacterPolicy INSIDE_NUMBER  = 0;
const NextCharacterPolicy INSIDE_STRING  = PRESERVE_WS_AFTER_LC;
const NextCharacterPolicy INSIDE_STRING_FILEIFY  = DISABLE_ESCAPES;
const NextCharacterPolicy INSIDE_COMMENT = PRESERVE_WS_AFTER_LC;


//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences such as \[Alpha] into a single WL character
//
class CharacterDecoder {

    WLCharacter cur;

    std::vector<std::pair<WLCharacter, SourceSpan>> characterQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    WLCharacter handleLongName(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle2Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle4Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle6Hex(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handleOctal(SourceCharacter curSourceIn, SourceLocation CharacterStart, NextCharacterPolicy policy);

    WLCharacter enqueue(std::vector<std::pair<WLCharacter, SourceSpan>> chars);
    
public:
    CharacterDecoder();

    void init();

    void deinit();

    WLCharacter nextWLCharacter(NextCharacterPolicy policy = TOPLEVEL);

    WLCharacter currentWLCharacter();

    std::vector<SyntaxIssue> getIssues();
};

extern CharacterDecoder *TheCharacterDecoder;
