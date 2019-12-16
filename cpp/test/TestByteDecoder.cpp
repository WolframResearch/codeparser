
#include "ByteDecoder.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>


//const NextCharacterPolicy TOPLEVEL = ENABLE_BYTE_DECODING_ISSUES | ENABLE_ESCAPES | ENABLE_CHARACTER_DECODING_ISSUES | LC_UNDERSTANDS_CRLF | ENABLE_STRANGE_CHARACTER_CHECKING;
//const NextCharacterPolicy TOPLEVEL = ENABLE_BYTE_DECODING_ISSUES | ENABLE_CHARACTER_DECODING_ISSUES | ENABLE_STRANGE_CHARACTER_CHECKING;


class ByteDecoderTest : public ::testing::Test {
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

TEST_F(ByteDecoderTest, Basic1) {
    
    auto strIn = std::string("1+2");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    TheParserSession->init(BufferAndLength(str, strIn.size(), false), nullptr, INCLUDE_SOURCE);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('2'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 0u);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
TEST_F(ByteDecoderTest, Basic2) {
    
    const unsigned char arr[] = {'1', '+', 206, 177};
    
    TheParserSession->init(BufferAndLength(arr, 4, false), nullptr, INCLUDE_SOURCE);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(0x03b1));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 0u);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
TEST_F(ByteDecoderTest, Basic3) {
    
    const unsigned char arr[] = {'1', '+', 0xE2, 0x9A, 0xA1};
    
    TheParserSession->init(BufferAndLength(arr, 5, false), nullptr, INCLUDE_SOURCE);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(0x26A1));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 0u);
}

TEST_F(ByteDecoderTest, Invalid1) {
    
    const unsigned char arr[] = {'1', '+', 0xf8};
    
    TheParserSession->init(BufferAndLength(arr, 3, false), nullptr, INCLUDE_SOURCE);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 1u);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
// So test with only first byte
//
TEST_F(ByteDecoderTest, Invalid2) {
    
    const unsigned char arr[] = {'1', '+', 206};
    
    TheParserSession->init(BufferAndLength(arr, 3, false), nullptr, INCLUDE_SOURCE);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 1u);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
TEST_F(ByteDecoderTest, Invalid3) {
    
    const unsigned char arr[] = {'1', '+', 0xE2};
    
    TheParserSession->init(BufferAndLength(arr, 3, false), nullptr, INCLUDE_SOURCE);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    // from 0xE2 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 1u);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
TEST_F(ByteDecoderTest, Invalid4) {
    
    const unsigned char arr[] = {'1', '+', 0xE2, 0x9A};
    
    TheParserSession->init(BufferAndLength(arr, 4, false), nullptr, INCLUDE_SOURCE);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    // from 0xE2 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    // from 0x9A byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_REPLACEMENT_CHARACTER));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 2u);
}

