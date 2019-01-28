
#pragma once

#include "ByteDecoder.h"
#include "SyntaxIssue.h"
#include "CodePoint.h"

#include <sstream>
#include <vector>
#include <cassert>

//
// https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
//
#if defined NDEBUG
# define X_ASSERT(CHECK) void(0)
#else
# define X_ASSERT(CHECK) \
( (CHECK) ? void(0) : []{assert(false && #CHECK);}() )
#endif

class WLCharacter
{
public:
    explicit constexpr WLCharacter(int val) : value_(val) {}

    explicit operator int() const noexcept = delete;

    bool operator==(const WLCharacter &o) const {
        return value_ == o.value_;
    }

    bool operator==(int o) const {
        return value_ == o;
    }

    bool operator!=(int o) const {
        return value_ != o;
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

    bool isDigit() const;

    bool isAlpha() const;

    bool isDigitOrAlpha() const;

    bool isDigitOrAlphaOrDollar() const;

    bool isAlphaOrDollar() const;

    bool isHex() const;

    bool isOctal() const;

    bool isSpace() const;

    bool isControl() const;

    bool isLinearSyntax() const;

    bool isLetterlikeCharacter() const;
    bool isStrangeLetterlikeCharacter() const;
    bool isOperatorCharacter() const;
    bool isSpaceCharacter() const;
    bool isNewlineCharacter() const;
    bool isCommaCharacter() const;

    int toBaseDigit() const;

private:
    int value_;
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
};

typedef int NextCharacterPolicy;

const NextCharacterPolicy TOPLEVEL       = 0;
const NextCharacterPolicy INSIDE_NUMBER  = 0;
const NextCharacterPolicy INSIDE_STRING  = PRESERVE_WS_AFTER_LC;
const NextCharacterPolicy INSIDE_COMMENT = PRESERVE_WS_AFTER_LC;


//
// CharacterDecoder is given a stream of integers that represent Unicode code points and decodes
// sequences such as \[Alpha] into a single WL character
//
class CharacterDecoder {

    WLCharacter cur;
    SourceCharacter curSource;

    std::vector<std::pair<WLCharacter, SourceSpan>> characterQueue;
    
    std::vector<SyntaxIssue> Issues;
    
    WLCharacter handleLongName(SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle2Hex(SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle4Hex(SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handle6Hex(SourceLocation CharacterStart, NextCharacterPolicy policy);
    WLCharacter handleOctal(SourceLocation CharacterStart, NextCharacterPolicy policy);

    WLCharacter leaveAlone(std::vector<std::pair<WLCharacter, SourceSpan>> chars);

    int replaceRawCodePoint(int point);
    
public:
    CharacterDecoder();

    WLCharacter nextWLCharacter(NextCharacterPolicy policy = TOPLEVEL);

    WLCharacter currentWLCharacter();

    std::vector<SyntaxIssue> getIssues();
};

extern CharacterDecoder *TheCharacterDecoder;
