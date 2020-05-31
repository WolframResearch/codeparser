
#include "API.h"
#include "Tokenizer.h"
#include "ByteDecoder.h"
#include "ByteBuffer.h"

#include "gtest/gtest.h"

#include <sstream>



class TokenizerTest : public ::testing::Test {
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

//
// This was asserting
//
TEST_F(TokenizerTest, Bug1) {
    
    auto strIn = std::string("\\.GG");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug2) {
    
    auto strIn = std::string("<<<");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    TheTokenizer->nextToken(Tok);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug3) {
    
    auto strIn = std::string("\\\r");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug4) {
    
    auto strIn = std::string("\\[");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug5) {
    
    auto strIn = std::string("\"a\\\\\r\nb\"");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    SUCCEED();
}

TEST_F(TokenizerTest, IntegerRealMixup) {
    
    auto strIn = std::string("0..");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok1 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok1, Token(TOKEN_INTEGER, BufferAndLength(str, 1), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    TheTokenizer->nextToken(Tok1);
    
    auto Tok2 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok2, Token(TOKEN_DOTDOT, BufferAndLength(str + 1, 2), Source(SourceLocation(1, 2), SourceLocation(1, 4))));
    
    TheTokenizer->nextToken(Tok2);
    
    auto Tok3 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok3, Token(TOKEN_ENDOFFILE, BufferAndLength(str + 3, 0), Source(SourceLocation(1, 4), SourceLocation(1, 4))));
}

TEST_F(TokenizerTest, Basic2) {
    
    auto strIn = std::string("\\[Alpha]bc+1");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok1 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok1, Token(TOKEN_SYMBOL, BufferAndLength(str + 0, 10), Source(SourceLocation(1, 1), SourceLocation(1, 11))));
    
    TheTokenizer->nextToken(Tok1);
    
    auto Tok2 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok2, Token(TOKEN_PLUS, BufferAndLength(str + 10, 1), Source(SourceLocation(1, 11), SourceLocation(1, 12))));
    
    TheTokenizer->nextToken(Tok2);
    
    auto Tok3 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok3, Token(TOKEN_INTEGER, BufferAndLength(str + 11, 1), Source(SourceLocation(1, 12), SourceLocation(1, 13))));
    
    TheTokenizer->nextToken(Tok3);
    
    auto Tok4 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok4, Token(TOKEN_ENDOFFILE, BufferAndLength(str + 12, 0), Source(SourceLocation(1, 13), SourceLocation(1, 13))));
}

TEST_F(TokenizerTest, OldAssert1) {
    
    auto strIn = std::string("8*");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_INTEGER, BufferAndLength(str, 1), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
}

TEST_F(TokenizerTest, Basic3) {
    
    auto strIn = std::string("{\n}");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_OPENCURLY, BufferAndLength(str, 1), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    TheTokenizer->nextToken(Tok);
    
    //
    // Clear 0x100 because we are inside a group now
    //
    Tok = TheTokenizer->currentToken(TOPLEVEL & ~(RETURN_TOPLEVELNEWLINE));
    
    EXPECT_EQ(Tok, Token(TOKEN_INTERNALNEWLINE, BufferAndLength(str + 1, 1), Source(SourceLocation(1, 2), SourceLocation(2, 1))));
    
    TheTokenizer->nextToken(Tok);
    
    Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_CLOSECURLY, BufferAndLength(str + 2, 1), Source(SourceLocation(2, 1), SourceLocation(2, 2))));
    
    TheTokenizer->nextToken(Tok);
}

TEST_F(TokenizerTest, Basic4) {
    
    const unsigned char arr[] = { 0xff };
    
    TheParserSession->init(BufferAndLength(arr, 1), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    EXPECT_EQ(TheByteDecoder->SrcLoc, SourceLocation(1, 1));
    
    EXPECT_EQ(TheByteBuffer->wasEOF, false);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_SYMBOL, BufferAndLength(arr, 1, UTF8STATUS_INVALID), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    EXPECT_EQ(TheByteDecoder->SrcLoc, SourceLocation(1, 1));
    
    EXPECT_EQ(TheByteBuffer->wasEOF, false);
    
    TheTokenizer->nextToken(Tok);
    
    Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, BufferAndLength(arr + 1, 0), Source(SourceLocation(1, 2), SourceLocation(1, 2))));
    
    TheTokenizer->nextToken(Tok);
    
    EXPECT_EQ(TheByteDecoder->SrcLoc, SourceLocation(1, 2));
    
    EXPECT_EQ(TheByteBuffer->wasEOF, true);
}

TEST_F(TokenizerTest, Crash1) {
    
    const unsigned char arr[] = { '6', '`', '5', '.', '.' };
    
    TheParserSession->init(BufferAndLength(arr, 5), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    TheTokenizer->currentToken(TOPLEVEL);
    
    SUCCEED();
}


