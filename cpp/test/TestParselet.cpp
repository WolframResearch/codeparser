
#include "Parser.h"
#include "Tokenizer.h"
#include "API.h"
#include "ParseletRegistration.h"
#include "Parselet.h" // for Parselet impls

#include "gtest/gtest.h"

#include <sstream>



class ParseletTest : public ::testing::Test {
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
        TheParserSession->deinit();
    }

};

TEST_F(ParseletTest, Bug1) {
    
    auto strIn = std::string("a /: b := c");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    auto NP = prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    auto N = NP.get();
    
    EXPECT_TRUE(dynamic_cast<TernaryNode*>(N));
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug2) {
    
    auto strIn = std::string("a<b ");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug3) {
    
    auto strIn = std::string("a\\[Integral]b\\[Integral]c ");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug4) {
    
    auto strIn = std::string("\\[RawLeftBrace]*\\[RawRightBrace]");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    prefixParselets[Tok.Tok.value()]->parse(Tok, Ctxt);
    
    SUCCEED();
}
