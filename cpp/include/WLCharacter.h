
#pragma once

#include <cassert>
#include <string>
#include <iterator>

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
// Version 1 of WLCharacter encoding
//
// 32 bits:
//
// vutsrqponmlkjihgfedcba9876543210
//            ^~~~~~~~~~~~~~~~~~~~~
//            Character bits (21 bits)
//           ^
//           Sign bit
//        ^~~
//        EscapeFormat bits (3 bits)
// ^~~~~~~
// Currently unused (7 bits)
//

//
// A single WL character
//
// The text  \[Alpha]  would be 1 WLCharacter
//
struct WLCharacter {
    
    uint32_t valBits : 21;
    uint8_t signBit : 1;
    uint8_t escapeBits : 3;
    
    explicit constexpr WLCharacter(int val, EscapeFormat escape = ESCAPE_NONE) : valBits(val), signBit(val < 0), escapeBits(escape) {}
    
    explicit operator int() const noexcept = delete;
    
    constexpr bool operator==(const WLCharacter &o) const {
        return valBits == o.valBits && signBit == o.signBit && escapeBits == o.escapeBits;
    }
    
    constexpr bool operator!=(const WLCharacter &o) const {
        return valBits != o.valBits || signBit != o.signBit || escapeBits != o.escapeBits;
    }
    
    //
    // for std::map
    //
    constexpr bool operator<(const WLCharacter &o) const {
        return to_point() < o.to_point();
    }
    
    constexpr int to_point() const {
        //
        // Sign extend the value
        //
        return signBit ? (valBits | -0x200000) : valBits;
    }
    
    constexpr EscapeFormat escape() const {
        return static_cast<EscapeFormat>(escapeBits);
    }
    
    std::string graphicalString() const;
    
    bool isEscaped() const;
    
    bool isAlpha() const;
    
    bool isDigit() const;
    
    bool isAlphaOrDigit() const;
    
    bool isHex() const;
    
    bool isOctal() const;
    
    bool isLetterlike() const;
    
    bool isStrangeLetterlike() const;
    
    bool isVeryStrangeLetterlike() const;
    
    bool isPunctuation() const;
    
    bool isSpace() const;
    
    bool isStrangeSpace() const;
    
    bool isNewline() const;
    
    bool isLinearSyntax() const;
    
    bool isStringMeta() const;
    
    bool isUninterpretable() const;
    
    bool isControl() const;
    
    bool isLetterlikeCharacter() const;
    bool isStrangeLetterlikeCharacter() const;
    bool isVeryStrangeLetterlikeCharacter() const;
    bool isPunctuationCharacter() const;
    bool isSpaceCharacter() const;
    bool isStrangeSpaceCharacter() const;
    bool isNewlineCharacter() const;
    bool isStrangeNewlineCharacter() const;
    bool isUninterpretableCharacter() const;
    bool isControlCharacter() const;
    bool isLineContinuation() const;
    
    bool isStrange() const;
};

std::ostream& operator<<(std::ostream& stream, WLCharacter);

//
// For googletest
//
void PrintTo(const WLCharacter&, std::ostream*);
