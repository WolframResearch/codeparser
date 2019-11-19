
#include "TokenEnum.h"

#include "gtest/gtest.h"

#include <sstream>

TEST(TokenEnumTest, Inequality) {
    
    EXPECT_TRUE(TOKEN_LESS.isInequalityOperator());
    
}

