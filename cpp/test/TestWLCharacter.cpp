
#include "WLCharacter.h"

#include "LongNames.h"

#include "gtest/gtest.h"

#include <sstream>

class WLCharacterTest : public ::testing::Test {
protected:

};

TEST_F(WLCharacterTest, Bug1) {

    auto C = WLCharacter('\t');

    EXPECT_EQ(C.graphicalString(), "\\t");
}

TEST_F(WLCharacterTest, Bug2) {
    
    auto C = WLCharacter(' ');
    
    EXPECT_EQ(C.graphicalString(), " ");
}

TEST_F(WLCharacterTest, RawGraphical) {
    
    auto c = WLCharacter(CODEPOINT_LONGNAME_RAWTAB, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "\\t");
    
    c = WLCharacter(CODEPOINT_LONGNAME_NEWLINE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "\\n");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWRETURN, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "\\r");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWESCAPE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "\\[RawEscape]");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWSPACE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), " ");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWEXCLAMATION, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "!");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWDOUBLEQUOTE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "\\\"");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWNUMBERSIGN, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "#");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWDOLLAR, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "$");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWPERCENT, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "%");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWAMPERSAND, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "&");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWQUOTE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "'");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWLEFTPARENTHESIS, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "(");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWRIGHTPARENTHESIS, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), ")");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWSTAR, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "*");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWPLUS, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "+");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWCOMMA, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), ",");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWDASH, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "-");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWDOT, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), ".");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWSLASH, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "/");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWCOLON, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), ":");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWSEMICOLON, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), ";");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWLESS, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "<");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWEQUAL, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "=");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWGREATER, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), ">");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWQUESTION, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "?");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWAT, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "@");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWLEFTBRACKET, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "[");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWBACKSLASH, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "\\\\");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWRIGHTBRACKET, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "]");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWWEDGE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "^");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWUNDERSCORE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "_");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWBACKQUOTE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "`");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWLEFTBRACE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "{");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWVERTICALBAR, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "|");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWRIGHTBRACE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "}");
    
    c = WLCharacter(CODEPOINT_LONGNAME_RAWTILDE, ESCAPE_RAW);
    
    EXPECT_EQ(c.graphicalString(), "~");
    
}
