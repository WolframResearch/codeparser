
#pragma once

#include "WLCharacter.h" // for WLCharacter
#include "Source.h" // for Source
#include "Issue.h"

#include <ostream>
#include <cstdint> // for uint8_t
#include <cstddef> // for size_t

using Buffer = const unsigned char *;

std::ostream& set_graphical(std::ostream& s);

std::ostream& clear_graphical(std::ostream& s);

int get_graphical_i();

//
//
//
class Utils {
public:
    
    static bool isStraySurrogate(codepoint point);
    
    static bool isStrange(codepoint point);
    
    static bool isMBStrange(codepoint point);
    
    //
    // Convert val to the digit that it represents
    //
    static uint8_t toDigit(unsigned char val);
    
    //
    // if c is an ASCII WLCharacter, then compare to test
    //
    static bool ifASCIIWLCharacter(unsigned char c, char test);
    
    //
    //
    //
    static CodeActionPtrVector certainCharacterReplacementActions(WLCharacter c, Source src);
    
    static bool containsOnlyASCII(Buffer Buf, size_t Len);

    static bool containsTab(Buffer Buf, size_t Len);
};
