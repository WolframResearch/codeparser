
#include "TokenEnumRegistration.h"

#include "gtest/gtest.h"


TEST(TokenEnumTest, Trivia) {
    
    EXPECT_TRUE(TOKEN_WHITESPACE.isTrivia());
}

TEST(TokenEnumTest, PossibleBeginning) {
    
    EXPECT_TRUE(TOKEN_SYMBOL.isPossibleBeginning());
    
    EXPECT_TRUE(TOKEN_SEMISEMI.isPossibleBeginning());
}

TEST(TokenEnumTest, Closer) {
    
    EXPECT_TRUE(TOKEN_CLOSESQUARE.isCloser());
    
    EXPECT_TRUE(TOKEN_LONGNAME_RIGHTCEILING.isCloser());
}

TEST(TokenEnumTest, Error) {
    
    EXPECT_TRUE(TOKEN_ERROR_EXPECTEDTAG.isError());
    
    EXPECT_TRUE(TOKEN_ERROR_UNSUPPORTEDTOKEN.isError());
}

TEST(TokenEnumTest, Unterminated) {
    
    EXPECT_TRUE(TOKEN_ERROR_UNTERMINATEDSTRING.isUnterminated());
    
    EXPECT_TRUE(TOKEN_ERROR_UNTERMINATEDCOMMENT.isUnterminated());
}

TEST(TokenEnumTest, Empty) {
    
    EXPECT_TRUE(TOKEN_ENDOFFILE.isEmpty());
    
    EXPECT_TRUE(TOKEN_ERROR_EXPECTEDOPERAND.isEmpty());
}
