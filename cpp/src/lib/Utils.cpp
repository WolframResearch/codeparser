
#include "Utils.h"

#include "Tokenizer.h" // for Tokenizer
#include "LongNamesRegistration.h" // for CodePointToLongNameMap

#include <cassert>
#include <cctype> // for isalnum, isxdigit, isupper, isdigit, isalpha, ispunct, iscntrl with GCC and MSVC


bool Utils::isStrange(codepoint point) {

    switch (point) {
            //
            // C0 control characters
            //
            // Skipping LF, CR, TAB, and ESC
            //
        case '\x00': case '\x01': case '\x02': case '\x03': case '\x04': case '\x05': case '\x06': case '\x07':
        case '\x08': /*    \x09*/ /*    \x0a*/ case '\x0b': case '\x0c': /*    \x0d*/ case '\x0e': case '\x0f':
        case '\x10': case '\x11': case '\x12': case '\x13': case '\x14': case '\x15': case '\x16': case '\x17':
        case '\x18': case '\x19': case '\x1a': /*    \x1b*/ case '\x1c': case '\x1d': case '\x1e': case '\x1f': {
            //
            // Make sure to include DEL
            //
        case '\x7f':
            return true;
        }
    }
    
    return false;
}

bool Utils::isMBStrange(codepoint point) {

    //
    // Reject if ASCII, should use isStrange()
    //
    if ((0x00 <= point && point <= 0x7f)) {
        return false;
    }

    //
    // Individual characters
    //
    switch (point) {
        case CODEPOINT_ZEROWIDTHSPACE: {
            return true;
        }
            //
            // ZERO WIDTH NON-JOINER
            //
        case 0x200c: {
            return true;
        }
            //
            // ZERO WIDTH JOINER
            //
        case 0x200d: {
            return true;
        }
//            //
//            // LINE SEPARATOR
//            //
//        case 0x2028:
//            return true;
//            //
//            // WORD JOINER
//            //
//            // This is the character that is recommended to use for ZERO WIDTH NON-BREAKING SPACE
//            // https://unicode.org/faq/utf_bom.html#bom6
//            //
//        case 0x2060:
//            return true;
        //
        // Various curly quotes
        //
        case CODEPOINT_LONGNAME_OPENCURLYQUOTE:
        case CODEPOINT_LONGNAME_CLOSECURLYQUOTE:
        case CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE:
        case CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE: {
            return true;
        }
        //
        // U+2061
        //
        case CODEPOINT_FUNCTIONAPPLICATION: {
            return true;
        }
        //
        // U+2063
        //
        case CODEPOINT_INVISIBLESEPARATOR: {
            return true;
        }
        //
        // U+2064
        //
        case CODEPOINT_INVISIBLEPLUS: {
            return true;
        }
        //
        // U+2192
        //
        case CODEPOINT_LONGNAME_RIGHTARROW: {
            return true;
        }
        //
        // U+279D
        //
        case CODEPOINT_TRIANGLEHEADEDRIGHTWARDSARROW: {
            return true;
        }
        //
        // U+29F4
        //
        case CODEPOINT_RULEDELAYED: {
            return true;
        }
        case CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK: {
            return true;
        }
        //
        // Yes, we suggest \:2061 -> \[InvisibleApplication], but that is not saying \[InvisibleApplication] is not also strange!
        //
        case CODEPOINT_LONGNAME_INVISIBLEAPPLICATION: {
            return true;
        }
    }

    //
    // C1
    //
    if (0x0080 <= point && point <= 0x009f) {
        return true;
    }
    
    //
    // TODO: implement isBMPPUAUnassigned
    //
//    if (Utils::isBMPPUAUnassigned(point)) {
//        return true;
//    }
    
    //
    // TODO: implement isBMPNoncharacters
    //
//    if (Utils::isBMPNoncharacters(point)) {
//        return true;
//    }
    
    if (point <= 0xffff) {
        return false;
    }
    
    //
    // Non-BMP
    //
    
    //
    // TODO: implement isNonBMPNoncharacters
    //
//    if (Utils::isNonBMPNoncharacters(point)) {
//        return true;
//    }
    
    //
    // Plane 15 PUA
    //
    if (0x0f0000 <= point && point <= 0x0ffffd) {
        return true;
    }

    //
    // Plane 16 PUA
    //
    if (0x100000 <= point && point <= 0x10fffd) {
        return true;
    }
    
    return false;
}

bool Utils::isStraySurrogate(codepoint point) {
    
    if (0xd800 <= point && point <= 0xdfff) {
        return true;
    }
    
    return false;
}


int get_graphical_i() {
    
    static int i = std::ios_base::xalloc();
    
    return i;
}

std::ostream& set_graphical(std::ostream& s) {
    
    s.iword(get_graphical_i()) = 1;
    
    return s;
}

std::ostream& clear_graphical(std::ostream& s) {
    
    s.iword(get_graphical_i()) = 0;
    
    return s;
}


uint8_t digitLookup[] = {
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 99, 99, 99, 99, 99, 99,
    99, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99,
    99, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99};

uint8_t Utils::toDigit(unsigned char val) {
    return digitLookup[val];
}


bool Utils::ifASCIIWLCharacter(unsigned char c, char test) {
    
    if (c > 0x7f) {
        return true;
    }
    //
    // What is the last possible byte of an escaped WLCharacter?
    //
    if (std::isalnum(c) || c == ']') {
        return true;
    }
    
    //
    // there may be a line continuation and so testing against  '^'  may actually involve the bytes  '\' '\n' '^'
    //
    if (c == '\\') {
        return true;
    }
    
    return c == test;
}


//
// Give suggestions for replacing certain characters with other characters:
//
// \[COMPATIBILITYNoBreak] -> \[NoBreak]
// \:2061 -> \[InvisibleApplication]
// \:2063 -> \[InvisibleComma]
// \:2064 -> \[ImplicitPlus]
// \[RightArrow] -> \[Rule]
// \:279D -> \[Rule]
// \:29F4 -> \[RuleDelayed]
// \:200B -> \[InvisibleSpace]
//
CodeActionPtrVector Utils::certainCharacterReplacementActions(WLCharacter c, Source src) {
    
    CodeActionPtrVector Actions;
    
    switch (c.to_point()) {
        case CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK: {
            
            auto safeAndGraphicalStr1 = c.safeAndGraphicalString();
            auto safeAndGraphicalStr2 = WLCharacter(CODEPOINT_LONGNAME_NOBREAK).safeAndGraphicalString();
            
            //
            // UTF-8 bytes for U+2060 (\[NoBreak])
            //
            Actions.push_back(new ReplaceTextCodeAction("Replace ``" + safeAndGraphicalStr1 + "`` with ``" + safeAndGraphicalStr2 + "``", src, (c.escape() == ESCAPE_NONE) ? "\xe2\x81\xa0" : "\\[NoBreak]"));
            
            break;
        }
        case CODEPOINT_LONGNAME_RIGHTARROW:
            //
            // U+279D Triangle-Headed Rightwards Arrow being used in place of \[Rule] is seen here:
            // http://mail-archive.wolfram.com/archive/t-paclets/2022/Mar00/0004.html
            //
        case CODEPOINT_TRIANGLEHEADEDRIGHTWARDSARROW: {
            
            auto safeAndGraphicalStr1 = c.safeAndGraphicalString();
            auto safeAndGraphicalStr2 = WLCharacter(CODEPOINT_LONGNAME_RULE).safeAndGraphicalString();
            
            //
            // UTF-8 bytes for U+F522 (\[Rule])
            //
            Actions.push_back(new ReplaceTextCodeAction("Replace ``" + safeAndGraphicalStr1 + "`` with ``" + safeAndGraphicalStr2 + "``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x94\xa2" : "\\[Rule]"));
            
            break;
        }
        case CODEPOINT_RULEDELAYED: {
            
            auto safeAndGraphicalStr1 = c.safeAndGraphicalString();
            auto safeAndGraphicalStr2 = WLCharacter(CODEPOINT_LONGNAME_RULEDELAYED).safeAndGraphicalString();
            
            //
            // UTF-8 bytes for U+F51F (\[RuleDelayed])
            //
            Actions.push_back(new ReplaceTextCodeAction("Replace ``" + safeAndGraphicalStr1 + "`` with ``" + safeAndGraphicalStr2 + "``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x94\x9f" : "\\[RuleDelayed]"));
            
            break;
        }
        case CODEPOINT_FUNCTIONAPPLICATION: {
            
            auto safeAndGraphicalStr1 = c.safeAndGraphicalString();
            auto safeAndGraphicalStr2 = WLCharacter(CODEPOINT_LONGNAME_INVISIBLEAPPLICATION).safeAndGraphicalString();
            
            //
            // UTF-8 bytes for U+F76D (\[InvisibleApplication])
            //
            Actions.push_back(new ReplaceTextCodeAction("Replace ``" + safeAndGraphicalStr1 + "`` with ``" + safeAndGraphicalStr2 + "``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x9d\xad" : "\\[InvisibleApplication]"));
            
            Actions.push_back(new DeleteTextCodeAction("Delete ``" + safeAndGraphicalStr1 + "``", src));
            
            break;
        }
        case CODEPOINT_INVISIBLESEPARATOR: {
            
            auto safeAndGraphicalStr1 = c.safeAndGraphicalString();
            auto safeAndGraphicalStr2 = WLCharacter(CODEPOINT_LONGNAME_INVISIBLECOMMA).safeAndGraphicalString();
            
            //
            // UTF-8 bytes for U+F765 (\[InvisibleComma])
            //
            Actions.push_back(new ReplaceTextCodeAction("Replace ``" + safeAndGraphicalStr1 + "`` with ``" + safeAndGraphicalStr2 + "``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x9d\xa5" : "\\[InvisibleComma]"));
            
            break;
        }
        case CODEPOINT_INVISIBLEPLUS: {
            
            auto safeAndGraphicalStr1 = c.safeAndGraphicalString();
            auto safeAndGraphicalStr2 = WLCharacter(CODEPOINT_LONGNAME_IMPLICITPLUS).safeAndGraphicalString();
            
            //
            // UTF-8 bytes for U+F39E (\[ImplicitPlus])
            //
            Actions.push_back(new ReplaceTextCodeAction("Replace ``" + safeAndGraphicalStr1 + "`` with ``" + safeAndGraphicalStr2 + "``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x8e\x9e" : "\\[ImplicitPlus]"));
            
            break;
        }
        case CODEPOINT_ZEROWIDTHSPACE: {
            
            auto safeAndGraphicalStr1 = c.safeAndGraphicalString();
            auto safeAndGraphicalStr2 = WLCharacter(CODEPOINT_LONGNAME_INVISIBLESPACE).safeAndGraphicalString();
            
            //
            // UTF-8 bytes for U+F360 (\[InvisibleSpace])
            //
            Actions.push_back(new ReplaceTextCodeAction("Replace ``" + safeAndGraphicalStr1 + "`` with ``" + safeAndGraphicalStr2 + "``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x8d\xa0" : "\\[InvisibleSpace]"));
            
            break;
        }
    }
    
    return Actions;
}
