
#include "Source.h"

#include "gtest/gtest.h"


class SourceCharacterTest : public ::testing::Test {
protected:
    
};

TEST_F(SourceCharacterTest, Graphical1) {
    
    EXPECT_EQ(SourceCharacter('\t').graphicalString(), "\\t");
    
    EXPECT_EQ(SourceCharacter(0x1b).graphicalString(), "\\[RawEscape]");
    
    EXPECT_EQ(SourceCharacter(0xb0).graphicalString(), "\\[Degree]");
    
    EXPECT_EQ(SourceCharacter(0x80).graphicalString(), "\\.80");
    
    EXPECT_EQ(SourceCharacter(0xabcd).graphicalString(), "\\:abcd");
}
