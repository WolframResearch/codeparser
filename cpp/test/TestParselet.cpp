
#include "Parser.h"
#include "API.h"

#include "gtest/gtest.h"

#include <sstream>

static std::unique_ptr<MLSession> mlSession;

class ParseletTest : public ::testing::Test {
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

TEST_F(ParseletTest, Bug1) {
    
    auto str = std::string("a /: b := c");
    
    TheParserSession->init(nullptr, str.c_str(), str.size(), SOURCESTYLE_LINECOL, false, false, false);
    
    ParserContext Ctxt;
    
    auto NP = TheParser->parse(Ctxt);
    
    auto N = NP.get();
    
    EXPECT_TRUE(dynamic_cast<TernaryNode*>(N));
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug2) {
    
    auto str = std::string("a<b ");
    
    TheParserSession->init(nullptr, str.c_str(), str.size(), SOURCESTYLE_LINECOL, false, false, false);
    
    ParserContext Ctxt;
    
    TheParser->parse(Ctxt);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug3) {
    
    auto str = std::string("a\\[Integral]b\\[Integral]c ");
    
    TheParserSession->init(nullptr, str.c_str(), str.size(), SOURCESTYLE_LINECOL, false, false, false);
    
    ParserContext Ctxt;
    
    TheParser->parse(Ctxt);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug4) {
    
    auto str = std::string("\\[RawLeftBrace]*\\[RawRightBrace]");
    
    TheParserSession->init(nullptr, str.c_str(), str.size(), SOURCESTYLE_LINECOL, false, false, false);
    
    ParserContext Ctxt;
    
    TheParser->parse(Ctxt);
    
    SUCCEED();
}
