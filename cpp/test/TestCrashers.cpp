
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>


class CrashTest : public ::testing::Test {
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

TEST_F(CrashTest, Crash1) {
    
    const unsigned char arr[] = {'1', ':', ':', '*', '\\', '\r', '\n'};
    
    auto bufAndLen = BufferAndLength(arr, 7, false);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE);
    
    TheParserSession->parseExpressions();
    
    TheParserSession->deinit();
    
    SUCCEED();
}
