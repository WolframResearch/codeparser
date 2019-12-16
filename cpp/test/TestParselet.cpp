
#include "Parser.h"
#include "Tokenizer.h"
#include "API.h"

#include "gtest/gtest.h"

#include <sstream>


//const NextCharacterPolicy TOPLEVEL = ENABLE_BYTE_DECODING_ISSUES | ENABLE_ESCAPES | ENABLE_CHARACTER_DECODING_ISSUES | LC_UNDERSTANDS_CRLF | ENABLE_STRANGE_CHARACTER_CHECKING;
//const NextCharacterPolicy TOPLEVEL = ENABLE_BYTE_DECODING_ISSUES | ENABLE_CHARACTER_DECODING_ISSUES | ENABLE_STRANGE_CHARACTER_CHECKING;


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
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    auto NP = TheParser->parse(Tok, Ctxt);
    
    auto N = NP.get();
    
    EXPECT_TRUE(dynamic_cast<TernaryNode*>(N));
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug2) {
    
    auto strIn = std::string("a<b ");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    TheParser->parse(Tok, Ctxt);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug3) {
    
    auto strIn = std::string("a\\[Integral]b\\[Integral]c ");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    TheParser->parse(Tok, Ctxt);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug4) {
    
    auto strIn = std::string("\\[RawLeftBrace]*\\[RawRightBrace]");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    ParserContext Ctxt;
    
    TheParser->parse(Tok, Ctxt);
    
    SUCCEED();
}
