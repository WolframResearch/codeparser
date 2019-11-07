
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>


class APITest : public ::testing::Test {
protected:
    static void SetUpTestSuite() {
        
        TheParserSession = std::unique_ptr<ParserSession>(new ParserSession);
    }
    
    static void TearDownTestSuite() {
        
        TheParserSession.reset(nullptr);
    }
    
    void SetUp() override {
        
    }
    
    void TearDown() override {
        
    }
};

//
// this used to assert
//
TEST_F(APITest, Bug1) {
    
    auto strIn = std::string("abc[]");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    ParseLeaf(nullptr, str, strIn.size(), "LineCol", "False", "False");
    
    SUCCEED();
}
