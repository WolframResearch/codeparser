
#include "ParserSession.h"
#include "ByteDecoder.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

class ParserSession;

using ParserSessionPtr = ParserSession *;


class ByteDecoderTest : public ::testing::Test {
protected:
    
    static ParserSessionPtr session;
    
    static void SetUpTestSuite() {
        
        session = new ParserSession();
    }
    
    static void TearDownTestSuite() {
        
        delete session;
    }
    
    void SetUp() override {
        
    }
    
    void TearDown() override {
        
        session->deinit();
    }
};

ParserSessionPtr ByteDecoderTest::session;


TEST_F(ByteDecoderTest, Basic1) {
    
    auto strIn = std::string("1+2");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('2'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
TEST_F(ByteDecoderTest, Basic2) {
    
    const unsigned char arr[] = {'1', '+', 206, 177};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 4, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(0x03b1));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    //
    // Issue: Non-ASCII character: ``"α" (\[Alpha])``
    //
    EXPECT_EQ(session->nonFatalIssues.size(), 1u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
TEST_F(ByteDecoderTest, Basic3) {
    
    const unsigned char arr[] = {'1', '+', 0xE2, 0x9A, 0xA1};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 5, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(0x26A1));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    //
    // Issue: Non-ASCII character: ``"⚡" (\:26a1)``
    //
    EXPECT_EQ(session->nonFatalIssues.size(), 1u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(ByteDecoderTest, Invalid1) {
    
    const unsigned char arr[] = {'1', '+', 0xf8};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 3, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

//
// UTF-8 bytes for \[Alpha] are {206, 177}
//
// So test with only first byte
//
TEST_F(ByteDecoderTest, Invalid2) {
    
    const unsigned char arr[] = {'1', '+', 206};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 3, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
TEST_F(ByteDecoderTest, Invalid3) {
    
    const unsigned char arr[] = {'1', '+', 0xE2};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 3, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    // from 0xE2 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_1_BYTE_UTF8_SEQUENCE));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

//
// UTF-8 bytes for HIGH VOLTAGE SIGN (U+26A1) are { 0xE2, 0x9A, 0xA1 }
//
TEST_F(ByteDecoderTest, Invalid4) {
    
    const unsigned char arr[] = {'1', '+', 0xE2, 0x9A};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 4, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);

    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    // from 0xE2 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_2_BYTE_UTF8_SEQUENCE));

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

//
// Surrogates
//
TEST_F(ByteDecoderTest, Surrogate1) {
    
    const unsigned char arr[] = {'1', '+', 0xed, 0xa0, 0x80};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 5, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    EXPECT_EQ(session->buffer, session->start + 0);

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    EXPECT_EQ(session->buffer, session->start + 1);

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    // from 0xED byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE));

    EXPECT_EQ(session->buffer, session->start + 2);

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    // from 0xA0 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    EXPECT_EQ(session->buffer, session->start + 5);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

//
// Surrogates
//
TEST_F(ByteDecoderTest, Surrogate2) {
    
    const unsigned char arr[] = {'1', '+', 0xed, 0xb0, 0x80};
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 5, nullptr, opts);

    auto c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('1'));

    EXPECT_EQ(session->buffer, session->start + 0);

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, SourceCharacter('+'));

    EXPECT_EQ(session->buffer, session->start + 1);

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    // from 0xED byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_UNSAFE_3_BYTE_UTF8_SEQUENCE));

    EXPECT_EQ(session->buffer, session->start + 2);

    ByteDecoder_nextSourceCharacter(session, TOPLEVEL);
    
    c = ByteDecoder_currentSourceCharacter(session, TOPLEVEL);

    // from 0xB0 byte
    EXPECT_EQ(c, SourceCharacter(CODEPOINT_ENDOFFILE));

    EXPECT_EQ(session->buffer, session->start + 5);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    //
    // Issue: Invalid UTF-8 sequence: Stray surrogate
    //
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}
