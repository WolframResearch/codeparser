
#include "API.h"
#include "Tokenizer.h"

//#include "ByteDecoder.h"
//#include "SourceManager.h"
//#include "CharacterDecoder.h"
//#include "Symbol.h"

#include "gtest/gtest.h"

#include <sstream>

//static std::unique_ptr<MLSession> mlSession;

class TokenizerTest : public ::testing::Test {
protected:
    
    static void SetUpTestSuite() {
        
//        mlSession = std::unique_ptr<MLSession>(new MLSession);
        
        TheParserSession = std::unique_ptr<ParserSession>(new ParserSession);
    }
    
    static void TearDownTestSuite() {
        
        TheParserSession.reset(nullptr);
        
//        mlSession.reset(nullptr);
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
    
    auto strIn = std::string("\\.GG");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    TheParserSession->init(nullptr, str, strIn.size(), SOURCESTYLE_LINECOL, 0);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug2) {
    
    auto strIn = std::string("<<<");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    TheParserSession->init(nullptr, str, strIn.size(), SOURCESTYLE_LINECOL, 0);
    
    TheTokenizer->nextToken();
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug3) {
    
    auto strIn = std::string("\\\r");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    TheParserSession->init(nullptr, str, strIn.size(), SOURCESTYLE_LINECOL, 0);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug4) {
    
    auto strIn = std::string("\\[");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    TheParserSession->init(nullptr, str, strIn.size(), SOURCESTYLE_LINECOL, 0);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug5) {
    
    auto strIn = std::string("\"a\\\\\r\nb\"");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    TheParserSession->init(nullptr, str, strIn.size(), SOURCESTYLE_LINECOL, 0);
    
    SUCCEED();
}

