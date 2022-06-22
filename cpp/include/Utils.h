
#pragma once

#include "WLCharacter.h" // for WLCharacter
#include "Source.h" // for Source

#include <string>
#include <unordered_set> // for unordered_set
#include <ostream>
#include <cstdint> // for uint8_t


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
};
