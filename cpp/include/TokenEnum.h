
#pragma once

#include <cstdint> // for uint16_t


//
// All group closers
//
enum Closer : uint8_t {
    CLOSER_BARGREATER,
    CLOSER_CLOSECURLY,
    CLOSER_CLOSEPAREN,
    CLOSER_CLOSESQUARE,
    CLOSER_LONGNAME_CLOSECURLYDOUBLEQUOTE,
    CLOSER_LONGNAME_CLOSECURLYQUOTE,
    CLOSER_LONGNAME_RIGHTANGLEBRACKET,
    CLOSER_LONGNAME_RIGHTASSOCIATION,
    CLOSER_LONGNAME_RIGHTBRACKETINGBAR,
    CLOSER_LONGNAME_RIGHTCEILING,
    CLOSER_LONGNAME_RIGHTDOUBLEBRACKET,
    CLOSER_LONGNAME_RIGHTDOUBLEBRACKETINGBAR,
    CLOSER_LONGNAME_RIGHTFLOOR,
    // UNUSED
    CLOSER_ASSERTFALSE,
};

//
// Representing a token enum, with various properties exposed
//
struct TokenEnum {

    uint16_t valBits : 9;
    uint16_t group1Bits : 2;
    uint16_t group2Bits : 2;

    TokenEnum() : valBits(), group1Bits(), group2Bits() {}

    constexpr TokenEnum(uint16_t val, uint16_t group1, uint16_t group2) : valBits(val), group1Bits(group1), group2Bits(group2) {}
    
    explicit operator int() const noexcept = delete;
    
    constexpr uint16_t value() const {
        return valBits;
    }
    
    constexpr bool isTrivia() const {
        return static_cast<bool>((valBits & 0x1f8) == 0x08);
    }
    
    //
    // Only valid if already checked isError
    //
    constexpr bool isUnterminated() const {
        return static_cast<bool>((valBits & 0x1c) == 0x1c);
    }
    
    constexpr bool isPossibleBeginning() const {
        return static_cast<bool>(group1Bits == 0x1);
    }
    
    constexpr bool isCloser() const {
        return static_cast<bool>(group1Bits == 0x2);
    }
    
    constexpr bool isError() const {
        return static_cast<bool>(group1Bits == 0x3);
    }
    
    constexpr bool isEmpty() const {
        return static_cast<bool>(group2Bits == 0x1);
    }
    
    constexpr bool isTriviaButNotToplevelNewline() const {
        return isTrivia() && valBits != 0xc;
    }
};

bool operator==(TokenEnum a, TokenEnum b);

bool operator!=(TokenEnum a, TokenEnum b);

//
// Sizes of structs with bit-fields are implementation-dependent
//
#ifdef __clang__
static_assert(sizeof(TokenEnum) == 2, "Check your assumptions");
#endif // __clang__

Closer GroupOpenerToCloser(TokenEnum T);
Closer TokenToCloser(TokenEnum T);
