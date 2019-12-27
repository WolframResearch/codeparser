
#include "TokenEnum.h"

#include "gtest/gtest.h"

#include <sstream>

TEST(TokenEnumTest, Inequality) {
    
    EXPECT_TRUE(TOKEN_LESS.isInequalityOperator());
}

TEST(TokenEnumTest, DifferentialD) {
    
    EXPECT_TRUE(TOKEN_LONGNAME_DIFFERENTIALD.isDifferentialD());
    
    EXPECT_TRUE(TOKEN_LONGNAME_CAPITALDIFFERENTIALD.isDifferentialD());
}

