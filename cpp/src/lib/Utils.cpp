
#include "Utils.h"

#include "Tokenizer.h" // for Tokenizer
#include "LongNames.h" // for CodePointToLongNameMap

#include <cassert>
#include <cctype> // for isalnum, isxdigit, isupper, isdigit, isalpha, ispunct, iscntrl with GCC and MSVC


#if !NISSUES
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
        case '\x18': case '\x19': case '\x1a': /*    \x1b*/ case '\x1c': case '\x1d': case '\x1e': case '\x1f':
            //
            // Make sure to include DEL
            //
        case '\x7f':
            return true;
        default:
            return false;
    }
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
        case CODEPOINT_ZEROWIDTHSPACE:
            return true;
            //
            // ZERO WIDTH NON-JOINER
            //
        case 0x200c:
            return true;
            //
            // ZERO WIDTH JOINER
            //
        case 0x200d:
            return true;
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
        // U+2061
        //
        case CODEPOINT_FUNCTIONAPPLICATION:
            return true;
        //
        // U+2063
        //
        case CODEPOINT_INVISIBLESEPARATOR:
            return true;
        //
        // U+2064
        //
        case CODEPOINT_INVISIBLEPLUS:
            return true;
        //
        // U+2192
        //
        case CODEPOINT_LONGNAME_RIGHTARROW:
            return true;
        //
        // U+29F4
        //
        case CODEPOINT_RULEDELAYED:
            return true;
            //
            // ZERO WIDTH NO-BREAK SPACE
            //
            // But most likely BOM
            //
        case CODEPOINT_ACTUAL_BOM:
            //
            // Do not assert(false)
            //
            // \:feff is completely fine
            // The problem with 0xfeff is when it is a source character
            //
            return true;
            //
            // also BOM
            //
        case CODEPOINT_VIRTUAL_BOM:
            return true;
        case CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK:
            return true;
        //
        // Yes, we suggest \:2061 -> \[InvisibleApplication], but that is not saying \[InvisibleApplication] is not also strange!
        //
        case CODEPOINT_LONGNAME_INVISIBLEAPPLICATION:
            return true;
            //
            // REPLACEMENT CHARACTER
            //
            // This can be the result of badly encoded UTF-8
            //
        case CODEPOINT_REPLACEMENT_CHARACTER:
            return true;
    }

    //
    // C1
    //
    if (0x0080 <= point && point <= 0x009f) {
        return true;
    }

    //
    // High and low surrogates
    //
    if (0xd800 <= point && point <= 0xdfff) {
        return true;
    }
    
    //
    // TODO: implement isBMPPUAUnassigned
    //
//    if (Utils::isBMPPUAUnassigned(point)) {
//        return true;
//    }
    
    if (Utils::isBMPNonCharacter(point)) {
        return true;
    }
    
    if (point <= 0xffff) {
        return false;
    }
    
    //
    // Non-BMP
    //
    
    //
    // Plane 15 PUA
    //
    if (0xf0000 <= point && point <= 0xffffd) {
        return true;
    }

    //
    // Plane 16 PUA
    //
    if (0x100000 <= point && point <= 0x10fffd) {
        return true;
    }
    
    if (Utils::isNonBMPNonCharacter(point)) {
        return true;
    }
    
    return false;
}
#endif // !NISSUES

//
// https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
//
bool Utils::isBMPNonCharacter(codepoint point) {

    switch (point) {
        case 0xfdd0: case 0xfdd1: case 0xfdd2: case 0xfdd3: case 0xfdd4: case 0xfdd5: case 0xfdd6: case 0xfdd7:
        case 0xfdd8: case 0xfdd9: case 0xfdda: case 0xfddb: case 0xfddc: case 0xfddd: case 0xfdde: case 0xfddf:
        case 0xfde0: case 0xfde1: case 0xfde2: case 0xfde3: case 0xfde4: case 0xfde5: case 0xfde6: case 0xfde7:
        case 0xfde8: case 0xfde9: case 0xfdea: case 0xfdeb: case 0xfdec: case 0xfded: case 0xfdee: case 0xfdef:
            return true;
        case 0xfffe: case 0xffff:
            return true;
        default:
            return false;
    }
}

//
// https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Non-characters
//
bool Utils::isNonBMPNonCharacter(codepoint point) {

    switch (point) {
        case 0x1fffe: case 0x1ffff:
        case 0x2fffe: case 0x2ffff:
        case 0x3fffe: case 0x3ffff:
        case 0x4fffe: case 0x4ffff:
        case 0x5fffe: case 0x5ffff:
        case 0x6fffe: case 0x6ffff:
        case 0x7fffe: case 0x7ffff:
        case 0x8fffe: case 0x8ffff:
        case 0x9fffe: case 0x9ffff:
        case 0xafffe: case 0xaffff:
        case 0xbfffe: case 0xbffff:
        case 0xcfffe: case 0xcffff:
        case 0xdfffe: case 0xdffff:
        case 0xefffe: case 0xeffff:
        case 0xffffe: case 0xfffff:
        case 0x10fffe: case 0x10ffff:
            return true;
        default:
            return false;
    }
}


int get_graphical_i() {
    static int i = std::ios_base::xalloc();
    return i;
}

int get_safe_i() {
    static int i = std::ios_base::xalloc();
    return i;
}

std::ostream& set_graphical(std::ostream& stream) {
    stream.iword(get_graphical_i()) = 1;
    return stream;
}

std::ostream& clear_graphical(std::ostream& stream) {
    stream.iword(get_graphical_i()) = 0;
    return stream;
}

std::ostream& set_safe(std::ostream& stream) {
    stream.iword(get_safe_i()) = 1;
    return stream;
}

std::ostream& clear_safe(std::ostream& stream) {
    stream.iword(get_safe_i()) = 0;
    return stream;
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

SourceConvention Utils::parseSourceConvention(std::string s) {
    if (s == "LineColumn") {
        return SOURCECONVENTION_LINECOLUMN;
    } else if (s == "SourceCharacterIndex") {
        return SOURCECONVENTION_SOURCECHARACTERINDEX;
    } else {
        return SOURCECONVENTION_UNKNOWN;
    }
}


bool Utils::ifASCIIWLCharacter(unsigned char c, char test) {
    
    if (c >= 128) {
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
// \:29F4 -> \[RuleDelayed]
// \:200B -> \[InvisibleSpace]
//
CodeActionPtrVector Utils::certainCharacterReplacementActions(WLCharacter c, Source src) {
    
    auto graphicalStr = c.graphicalString();
    
    CodeActionPtrVector Actions;
    
    switch (c.to_point()) {
        case CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK:
            //
            // UTF-8 bytes for U+2060 (\[NoBreak])
            //
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``" + graphicalStr + "`` with ``\\[NoBreak]``", src, (c.escape() == ESCAPE_NONE) ? "\xe2\x81\xa0" : "\\[NoBreak]")));
            break;
        case CODEPOINT_LONGNAME_RIGHTARROW:
            //
            // UTF-8 bytes for U+F522 (\[Rule])
            //
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``" + graphicalStr + "`` with ``\\[Rule]``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x94\xa2" : "\\[Rule]")));
            break;
        case CODEPOINT_RULEDELAYED:
            //
            // UTF-8 bytes for U+F51F (\[RuleDelayed])
            //
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``" + graphicalStr + "`` with ``\\[RuleDelayed]``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x94\x9f" : "\\[RuleDelayed]")));
            break;
        case CODEPOINT_FUNCTIONAPPLICATION:
            //
            // UTF-8 bytes for U+F76D (\[InvisibleApplication])
            //
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``" + graphicalStr + "`` with ``\\[InvisibleApplication]``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x9d\xad" : "\\[InvisibleApplication]")));
            
            Actions.push_back(CodeActionPtr(new DeleteTextCodeAction("Delete ``" + graphicalStr + "``", src)));
            break;
        case CODEPOINT_INVISIBLESEPARATOR:
            //
            // UTF-8 bytes for U+F765 (\[InvisibleComma])
            //
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``" + graphicalStr + "`` with ``\\[InvisibleComma]``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x9d\xa5" : "\\[InvisibleComma]")));
            break;
        case CODEPOINT_INVISIBLEPLUS:
            //
            // UTF-8 bytes for U+F39E (\[ImplicitPlus])
            //
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``" + graphicalStr + "`` with ``\\[ImplicitPlus]``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x8e\x9e" : "\\[ImplicitPlus]")));
            break;
        case CODEPOINT_ZEROWIDTHSPACE:
            //
            // UTF-8 bytes for U+F360 (\[InvisibleSpace])
            //
            Actions.push_back(CodeActionPtr(new ReplaceTextCodeAction("Replace ``" + graphicalStr + "`` with ``\\[InvisibleSpace]``", src, (c.escape() == ESCAPE_NONE) ? "\xef\x8d\xa0" : "\\[InvisibleSpace]")));
            break;
    }
    
    return Actions;
}
