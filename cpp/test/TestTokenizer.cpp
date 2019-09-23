
#include "API.h"
//#include "Tokenizer.h"

//#include "ByteDecoder.h"
//#include "SourceManager.h"
//#include "CharacterDecoder.h"
//#include "Symbol.h"

#include "gtest/gtest.h"

#include <sstream>

static std::unique_ptr<MLSession> mlSession;

class TokenizerTest : public ::testing::Test {
protected:
    
    static void SetUpTestSuite() {
        
        mlSession = std::unique_ptr<MLSession>(new MLSession);
        
        TheParserSession = std::unique_ptr<ParserSession>(new ParserSession);
    }
    
    static void TearDownTestSuite() {
        
        TheParserSession.reset(nullptr);
        
        mlSession.reset(nullptr);
    }
    
    void SetUp() override {
        
    }
    
    void TearDown() override {
        TheParserSession->deinit();
    }
    
};

//
// This was asserting
//
TEST_F(TokenizerTest, Bug1) {
    
    auto iss = std::stringstream("\\.GG");
    
    TheParserSession->init(nullptr, iss, false);
    
    SUCCEED();
}
