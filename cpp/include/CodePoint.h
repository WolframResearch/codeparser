
#pragma once

#include "TokenEnum.h"

#include <cstdio> // for EOF

constexpr int CODEPOINT_BEL(0x07);
constexpr int CODEPOINT_ESC(0x1b);
constexpr int CODEPOINT_DEL(0x7f);

//
// These are the actual WL code points for linear syntax characters
//
constexpr int CODEPOINT_LINEARSYNTAX_CLOSEPAREN(0xf7c0);
constexpr int CODEPOINT_LINEARSYNTAX_BANG(0xf7c1);
constexpr int CODEPOINT_LINEARSYNTAX_AT(0xf7c2);
//UNUSED: constexpr int CODEPOINT_LINEARSYNTAX_HASH(0xf7c3);
//UNUSED: constexpr int CODEPOINT_LINEARSYNTAX_DOLLAR(0xf7c4);
constexpr int CODEPOINT_LINEARSYNTAX_PERCENT(0xf7c5);
constexpr int CODEPOINT_LINEARSYNTAX_CARET(0xf7c6);
constexpr int CODEPOINT_LINEARSYNTAX_AMP(0xf7c7);
constexpr int CODEPOINT_LINEARSYNTAX_STAR(0xf7c8);
constexpr int CODEPOINT_LINEARSYNTAX_OPENPAREN(0xf7c9);
constexpr int CODEPOINT_LINEARSYNTAX_UNDER(0xf7ca);
constexpr int CODEPOINT_LINEARSYNTAX_PLUS(0xf7cb);
constexpr int CODEPOINT_LINEARSYNTAX_SLASH(0xf7cc);
constexpr int CODEPOINT_LINEARSYNTAX_BACKTICK(0xf7cd);

//
// Nice to have
//
constexpr int CODEPOINT_REPLACEMENT_CHARACTER(0xfffd);

//
// Need a special code point because MathLink does not transmit BOM when it is first character
//
// Related bugs: 366106
//
constexpr int CODEPOINT_ACTUAL_BOM(0xfeff);
constexpr int CODEPOINT_VIRTUAL_BOM(0xe001);

//
// Do the simple thing and have ENDOFFILE be EOF
//
constexpr int CODEPOINT_ENDOFFILE(EOF);

//
//
//
constexpr int CODEPOINT_UNKNOWN(-2);

//
// There is an inconsistency in WL, such that LINEARSYNTAX_SPACE does not have a dedicated code point
// So invent one here.
//
constexpr int CODEPOINT_LINEARSYNTAX_SPACE(-3);

//
// The string meta characters \< and \> will have code points here, but they are not actual characters and do not have real code points
// The string meta characters \" and \\ will have code points here, but they are not actual characters and do not have real code points
// Assign codepoints to \b \f \n \r \t
// These WLCharacters may only appear in strings
// 
constexpr int CODEPOINT_STRINGMETA_OPEN(-4);
constexpr int CODEPOINT_STRINGMETA_CLOSE(-5);
constexpr int CODEPOINT_STRINGMETA_DOUBLEQUOTE(-6);
constexpr int CODEPOINT_STRINGMETA_BACKSLASH(-7);
constexpr int CODEPOINT_STRINGMETA_BACKSPACE(-8);
constexpr int CODEPOINT_STRINGMETA_FORMFEED(-9);
constexpr int CODEPOINT_STRINGMETA_LINEFEED(-10);
constexpr int CODEPOINT_STRINGMETA_CARRIAGERETURN(-11);
constexpr int CODEPOINT_STRINGMETA_TAB(-12);

//
// \r\n is a single SourceCharacter
//
// There is a mnemonic here: \r is 13 and CODEPOINT_CRLF is -13
//
constexpr int CODEPOINT_CRLF(-13);

//
// Create a line continuation character for CharacterDecoder to communicate to Tokenizer
//
constexpr int CODEPOINT_LINECONTINUATION_LF(-14);
constexpr int CODEPOINT_LINECONTINUATION_CR(-15);
constexpr int CODEPOINT_LINECONTINUATION_CRLF(-16);


TokenEnum LongNameCodePointToOperator(int c);
int LongNameOperatorToCodePoint(TokenEnum t);
