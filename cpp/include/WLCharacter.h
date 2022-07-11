
#pragma once

#include <string>
#include <cstdint> // for uint32_t
#include <ostream>

using codepoint = int32_t;


//
// The 8 styles of character escapes
//
// None: just regular characters: a, b, c, etc.
// Raw: Using the \[Raw] style: \[RawWedge], \[RawAt], etc.
// Single: A single backslash: \n, \t, \r, etc.
// 2Hex: \.xx style
// 4Hex: \:xxxx style
// 6Hex: \|xxxxxx style
// Octal: \xxx style
// LongName: Using \[XX] style: \[Alpha], \[Beta], etc.
//
// Used to just be Escape, but this was observed:
// c:\users\brenton\dropbox\wolfram\ast\ast\cpp\include\CharacterDecoder.h(37): error C2061: syntax error: identifier 'Escape'
//
enum EscapeStyle {
    ESCAPE_NONE,
    ESCAPE_RAW,
    ESCAPE_SINGLE,
    ESCAPE_2HEX,
    ESCAPE_4HEX,
    ESCAPE_6HEX,
    ESCAPE_OCTAL,
    ESCAPE_LONGNAME,
};

//
// A single WL character
//
// The text  \[Alpha]  would be 1 WLCharacter
//
struct WLCharacter {
    
    uint32_t valBits : 21;
    uint8_t signBit : 1;
    uint8_t escapeBits : 3;
    
    explicit constexpr WLCharacter(codepoint val, EscapeStyle escape = ESCAPE_NONE) : valBits(val), signBit(val < 0), escapeBits(escape) {}
    
    explicit operator int() const noexcept = delete;
    
    constexpr codepoint to_point() const {
        //
        // Sign extend the value
        //
        return signBit ? (valBits | -0x200000) : valBits;
    }
    
    constexpr EscapeStyle escape() const {
        return static_cast<EscapeStyle>(escapeBits);
    }
    
    std::string graphicalString() const;
    
    std::string safeAndGraphicalString() const;
    
    
    bool isUpper() const;
    
    bool isDigit() const;
    
    bool isAlphaOrDigit() const;
    
    bool isHex() const;
    
    bool isOctal() const;
    
    bool isLetterlike() const;
    
    bool isStrangeLetterlike() const;
    
    bool isStrangeWhitespace() const;
    
    bool isControl() const;
    
    bool isSign() const;
    
    bool isMBLinearSyntax() const;
    bool isMBStringMeta() const;
    bool isMBLetterlike() const;
    bool isMBStrangeLetterlike() const;
    bool isMBPunctuation() const;
    bool isMBWhitespace() const;
    bool isMBStrangeWhitespace() const;
    bool isMBNewline() const;
    bool isMBStrangeNewline() const;
    bool isMBUninterpretable() const;
};

//
// Sizes of structs with bit-fields are implementation-dependent
//
#ifdef __clang__
static_assert(sizeof(WLCharacter) == 4, "Check your assumptions");
#endif // __clang__

std::ostream& operator<<(std::ostream& s, WLCharacter c);

#if BUILD_TESTS
//
// For googletest
//
void PrintTo(const WLCharacter& c, std::ostream *s);
#endif // BUILD_TESTS
