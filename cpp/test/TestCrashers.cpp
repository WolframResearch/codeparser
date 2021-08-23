
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "ByteBuffer.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>


class CrashTest : public ::testing::Test {
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
        
    }
};

TEST_F(CrashTest, Crash0_characters) {
    
    const unsigned char arr[] = {'1', '\\', '\n'};
    
    auto bufAndLen = BufferAndLength(arr, 3);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto policy = TOPLEVEL;
    
    auto c = TheCharacterDecoder->currentWLCharacter(TheByteBuffer->buffer, TheByteDecoder->SrcLoc, policy);
    
    EXPECT_EQ(c.to_point(), '1');
    
    EXPECT_EQ(TheCharacterDecoder->lastBuf, arr + 1);
    EXPECT_EQ(TheCharacterDecoder->lastLoc, SourceLocation(1, 2));
    
    
    TheByteBuffer->buffer = TheCharacterDecoder->lastBuf;
    TheByteDecoder->SrcLoc = TheCharacterDecoder->lastLoc;
    
    c = TheCharacterDecoder->currentWLCharacter(TheByteBuffer->buffer, TheByteDecoder->SrcLoc, policy);
    
    EXPECT_EQ(c.to_point(), CODEPOINT_ENDOFFILE);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

TEST_F(CrashTest, Crash0_tokens) {
    
    const unsigned char arr[] = {'1', '\\', '\n'};
    
    auto bufAndLen = BufferAndLength(arr, 3);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto policy = TOPLEVEL;
    
    auto Tok = TheTokenizer->currentToken(policy);
    
    EXPECT_EQ(Tok.Tok.value(), TOKEN_INTEGER);
    EXPECT_EQ(Tok.Src, Source(SourceLocation(1, 1), SourceLocation(1, 2)));
    
    TheTokenizer->nextToken(Tok);
    
    Tok = TheTokenizer->currentToken(policy);
    
    EXPECT_EQ(Tok.Tok.value(), TOKEN_ENDOFFILE);
    EXPECT_EQ(Tok.Src, Source(SourceLocation(1, 2), SourceLocation(2, 1)));
    
    TheParserSession->deinit();
    
    SUCCEED();
}

TEST_F(CrashTest, Crash1) {
    
    const unsigned char arr[] = {'1', ':', ':', '*', '\\', '\r', '\n'};
    
    auto bufAndLen = BufferAndLength(arr, 7);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

#if 0
TEST_F(CrashTest, StackOverflow1) {
    
    unsigned char arr[1600];
    for (auto i = 0; i < 1600 ; i++){
        arr[i] = '(';
    }
    
    auto bufAndLen = BufferAndLength(arr, 1600);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}
#endif // #if 0

TEST_F(CrashTest, Crash2) {
    
    const unsigned char arr[] = {'\\', ':', 'f', 'e', 'f', 'f'};
    
    auto bufAndLen = BufferAndLength(arr, 6);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

TEST_F(CrashTest, Crash3) {
    
    const unsigned char arr[] = {'a', ':', 'b', '~', '1', ':', '2'};
    
    auto bufAndLen = BufferAndLength(arr, 7);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

TEST_F(CrashTest, Crash4) {
    
    const unsigned char arr[] = {'\\', '[', 'I', 'n', 't', 'e', 'g', 'r', 'a', 'l', ']', '\\', '[', 'S', 'u', 'm', ']'};
    
    auto bufAndLen = BufferAndLength(arr, 17);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

TEST_F(CrashTest, Crash5) {
    
    const unsigned char arr[] = {'{', '\t', '1', '\\', '\n', '^'};
    
    auto bufAndLen = BufferAndLength(arr, 6);
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_SOURCECHARACTERINDEX, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

