
#pragma once

#include "TokenEnum.h"

#include <cstdio> // for EOF
#include <cstdint> // for int32_t

using codepoint = int32_t;

//
// ASCII codepoints
//
constexpr codepoint CODEPOINT_BEL(0x07);
constexpr codepoint CODEPOINT_ESC(0x1b);
constexpr codepoint CODEPOINT_ACTUAL_DOUBLEQUOTE(0x22);
constexpr codepoint CODEPOINT_ACTUAL_BACKSLASH(0x5c);
constexpr codepoint CODEPOINT_DEL(0x7f);

//
// These are the actual WL code points for linear syntax characters
//
constexpr codepoint CODEPOINT_LINEARSYNTAX_CLOSEPAREN(0xf7c0);
constexpr codepoint CODEPOINT_LINEARSYNTAX_BANG(0xf7c1);
constexpr codepoint CODEPOINT_LINEARSYNTAX_AT(0xf7c2);
//UNUSED: constexpr codepoint CODEPOINT_LINEARSYNTAX_HASH(0xf7c3);
//UNUSED: constexpr codepoint CODEPOINT_LINEARSYNTAX_DOLLAR(0xf7c4);
constexpr codepoint CODEPOINT_LINEARSYNTAX_PERCENT(0xf7c5);
constexpr codepoint CODEPOINT_LINEARSYNTAX_CARET(0xf7c6);
constexpr codepoint CODEPOINT_LINEARSYNTAX_AMP(0xf7c7);
constexpr codepoint CODEPOINT_LINEARSYNTAX_STAR(0xf7c8);
constexpr codepoint CODEPOINT_LINEARSYNTAX_OPENPAREN(0xf7c9);
constexpr codepoint CODEPOINT_LINEARSYNTAX_UNDER(0xf7ca);
constexpr codepoint CODEPOINT_LINEARSYNTAX_PLUS(0xf7cb);
constexpr codepoint CODEPOINT_LINEARSYNTAX_SLASH(0xf7cc);
constexpr codepoint CODEPOINT_LINEARSYNTAX_BACKTICK(0xf7cd);

//
// Nice to have
//
constexpr codepoint CODEPOINT_REPLACEMENT_CHARACTER(0xfffd);

//
// Need a special code point because MathLink does not transmit BOM when it is first character
//
// Related bugs: 366106
//
constexpr codepoint CODEPOINT_ACTUAL_BOM(0xfeff);
constexpr codepoint CODEPOINT_VIRTUAL_BOM(0xe001);

//
// Do the simple thing and have ENDOFFILE be EOF
//
constexpr codepoint CODEPOINT_ENDOFFILE(EOF);

//
// Should never be used
//
constexpr codepoint CODEPOINT_ASSERTFALSE(-2);

//
// There is an inconsistency in WL, such that LINEARSYNTAX_SPACE does not have a dedicated code point
// So invent one here.
//
constexpr codepoint CODEPOINT_LINEARSYNTAX_SPACE(-3);

//
// The string meta characters \< and \> will have code points here, but they are not actual characters and do not have real code points
// The string meta characters \" and \\ will have code points here, but they are not actual characters and do not have real code points
// Assign codepoints to \b \f \n \r \t
// These WLCharacters may only appear in strings
// 
constexpr codepoint CODEPOINT_STRINGMETA_OPEN(-4);
constexpr codepoint CODEPOINT_STRINGMETA_CLOSE(-5);
constexpr codepoint CODEPOINT_STRINGMETA_DOUBLEQUOTE(-6);
constexpr codepoint CODEPOINT_STRINGMETA_BACKSLASH(-7);
constexpr codepoint CODEPOINT_STRINGMETA_BACKSPACE(-8);
constexpr codepoint CODEPOINT_STRINGMETA_FORMFEED(-9);
constexpr codepoint CODEPOINT_STRINGMETA_LINEFEED(-10);
constexpr codepoint CODEPOINT_STRINGMETA_CARRIAGERETURN(-11);
constexpr codepoint CODEPOINT_STRINGMETA_TAB(-12);

//
// \r\n is a single SourceCharacter
//
// There is a mnemonic here: \r is 13 and CODEPOINT_CRLF is -13
//
constexpr codepoint CODEPOINT_CRLF(-13);

TokenEnum LongNameCodePointToOperator(codepoint c);
codepoint LongNameOperatorToCodePoint(TokenEnum t);
