
#include "WLCharacter.h"

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

