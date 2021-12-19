
#include "ByteDecoder.h"
#include "ByteBuffer.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>


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
    
    TheParserSession->init(BufferAndLength(str, strIn.size()), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
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
    
    TheParserSession->init(BufferAndLength(arr, 4), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
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
    
    //
    // Issue: Non-ASCII character: ``"α" (\[Alpha])``
    //
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 1u);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
TEST_F(ByteDecoderTest, Basic3) {
    
    const unsigned char arr[] = {'1', '+', 0xE2, 0x9A, 0xA1};
    
    TheParserSession->init(BufferAndLength(arr, 5), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
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
    
    //
    // Issue: Non-ASCII character: ``"⚡" (\:26a1)``
    //
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 1u);
}

TEST_F(ByteDecoderTest, Invalid1) {
    
    const unsigned char arr[] = {'1', '+', 0xf8};
    
    TheParserSession->init(BufferAndLength(arr, 3), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_SEQUENCE));
    
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
    
    TheParserSession->init(BufferAndLength(arr, 3), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
//    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_SEQUENCE));
    
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
    
    TheParserSession->init(BufferAndLength(arr, 3), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
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
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_SEQUENCE));
    
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
    
    TheParserSession->init(BufferAndLength(arr, 4), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
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
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_2_BYTE_SEQUENCE));
    
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
// Surrogates
//
TEST_F(ByteDecoderTest, Surrogate1) {
    
    const unsigned char arr[] = {'1', '+', 0xed, 0xa0, 0x80};
    
    TheParserSession->init(BufferAndLength(arr, 5), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 0);
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 1);
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    // from 0xED byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_SEQUENCE));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 2);
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    // from 0xA0 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 5);
    
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 1u);
}

//
// Surrogates
//
TEST_F(ByteDecoderTest, Surrogate2) {
    
    const unsigned char arr[] = {'1', '+', 0xed, 0xb0, 0x80};
    
    TheParserSession->init(BufferAndLength(arr, 5), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('1'));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 0);
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    EXPECT_EQ(c, SourceCharacter('+'));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 1);
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    // from 0xED byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_SEQUENCE));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 2);
    
    //    TheByteDecoder->nextSourceCharacter(TOPLEVEL);
    TheByteBuffer->buffer = TheByteDecoder->lastBuf;
    
    c = TheByteDecoder->currentSourceCharacter(TOPLEVEL);
    
    // from 0xB0 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(TheByteBuffer->buffer, arr + 5);
    
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    EXPECT_EQ(TheByteDecoder->getIssues().size(), 1u);
}

