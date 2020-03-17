
#include "TokenEnum.h"

#include "gtest/gtest.h"

#include <sstream>

//TEST(TokenEnumTest, Infix) {
//    
//    EXPECT_TRUE(TOKEN_LESS.isInfixOperator());
//}

TEST(TokenEnumTest, DifferentialD) {
    
    EXPECT_TRUE(TOKEN_LONGNAME_DIFFERENTIALD.isDifferentialD());
    
    EXPECT_TRUE(TOKEN_LONGNAME_CAPITALDIFFERENTIALD.isDifferentialD());
}

