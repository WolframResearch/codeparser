
#include "Source.h"

#include "gtest/gtest.h"

#include <sstream>

class SourceCharacterTest : public ::testing::Test {
protected:
    
};

TEST_F(SourceCharacterTest, Graphical1) {
    
    auto C = SourceCharacter('\t');
    
    EXPECT_EQ(C.graphicalString(), "\\t");
    
    C = SourceCharacter(0x1b);
    
    EXPECT_EQ(C.graphicalString(), "\\[RawEscape]");
}
