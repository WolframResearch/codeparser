
#include "API.h"
#include "Tokenizer.h"

#include "gtest/gtest.h"

#include <sstream>


//const NextCharacterPolicy TOPLEVEL = ENABLE_BYTE_DECODING_ISSUES | ENABLE_ESCAPES | ENABLE_CHARACTER_DECODING_ISSUES | LC_UNDERSTANDS_CRLF | ENABLE_STRANGE_CHARACTER_CHECKING;
//const NextCharacterPolicy TOPLEVEL = ENABLE_BYTE_DECODING_ISSUES | ENABLE_CHARACTER_DECODING_ISSUES | ENABLE_STRANGE_CHARACTER_CHECKING;


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
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug2) {
    
    auto strIn = std::string("<<<");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    TheTokenizer->currentToken(TOPLEVEL);
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug3) {
    
    auto strIn = std::string("\\\r");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug4) {
    
    auto strIn = std::string("\\[");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug5) {
    
    auto strIn = std::string("\"a\\\\\r\nb\"");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    SUCCEED();
}

TEST_F(TokenizerTest, IntegerRealMixup) {
    
    auto strIn = std::string("0..");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok1 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok1, Token(TOKEN_INTEGER, BufferAndLength(Buffer(str), 1, false), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    auto Tok2 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok2, Token(TOKEN_DOTDOT, BufferAndLength(Buffer(str + 1), 2, false), Source(SourceLocation(1, 2), SourceLocation(1, 4))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    auto Tok3 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok3, Token(TOKEN_ENDOFFILE, BufferAndLength(Buffer(str + 3), 0, false), Source(SourceLocation(1, 4), SourceLocation(1, 4))));
}

TEST_F(TokenizerTest, Basic2) {
    
    auto strIn = std::string("\\[Alpha]bc+1");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok1 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok1, Token(TOKEN_SYMBOL, BufferAndLength(Buffer(str + 0), 10, false), Source(SourceLocation(1, 1), SourceLocation(1, 11))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    auto Tok2 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok2, Token(TOKEN_PLUS, BufferAndLength(Buffer(str + 10), 1, false), Source(SourceLocation(1, 11), SourceLocation(1, 12))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    auto Tok3 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok3, Token(TOKEN_INTEGER, BufferAndLength(Buffer(str + 11), 1, false), Source(SourceLocation(1, 12), SourceLocation(1, 13))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    auto Tok4 = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok4, Token(TOKEN_ENDOFFILE, BufferAndLength(Buffer(str + 12), 0, false), Source(SourceLocation(1, 13), SourceLocation(1, 13))));
}

TEST_F(TokenizerTest, OldAssert1) {
    
    auto strIn = std::string("8*");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_INTEGER, BufferAndLength(Buffer(str), 1, false), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
}

TEST_F(TokenizerTest, Basic3) {
    
    auto strIn = std::string("{\n}");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_OPENCURLY, BufferAndLength(Buffer(str), 1, false), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_NEWLINE, BufferAndLength(Buffer(str + 1), 1, false), Source(SourceLocation(2, 0), SourceLocation(2, 1))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_CLOSECURLY, BufferAndLength(Buffer(str + 2), 1, false), Source(SourceLocation(2, 1), SourceLocation(2, 2))));
    
    TheTokenizer->nextToken(TOPLEVEL);
}

TEST_F(TokenizerTest, Basic4) {
    
    const unsigned char arr[] = { 0xff };
    
    TheParserSession->init(BufferAndLength(arr, 1, true), nullptr, INCLUDE_SOURCE);
    
    EXPECT_EQ(TheByteDecoder->SrcLoc, SourceLocation(1, 1));
    
    EXPECT_EQ(TheByteBuffer->wasEOF, false);
    
    auto Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_SYMBOL, BufferAndLength(Buffer(arr), 1, true), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    EXPECT_EQ(TheByteDecoder->SrcLoc, SourceLocation(1, 1));
    
    EXPECT_EQ(TheByteBuffer->wasEOF, false);
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    Tok = TheTokenizer->currentToken(TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, BufferAndLength(Buffer(arr + 1), 0, false), Source(SourceLocation(1, 2), SourceLocation(1, 2))));
    
    TheTokenizer->nextToken(TOPLEVEL);
    
    EXPECT_EQ(TheByteDecoder->SrcLoc, SourceLocation(1, 2));
    
    EXPECT_EQ(TheByteBuffer->wasEOF, true);
}




