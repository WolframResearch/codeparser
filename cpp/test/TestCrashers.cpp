
#include "Tokenizer.h"
#include "ParserSession.h"
#include "TokenEnumRegistration.h"

#include "gtest/gtest.h"

class ParserSession;

using ParserSessionPtr = ParserSession *;


class CrashTest : public ::testing::Test {
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
        
    }
};

ParserSessionPtr CrashTest::session;


TEST_F(CrashTest, Crash0_tokens) {
    
    const unsigned char arr[] = {'1', '\\', '\n'};

    auto bufAndLen = BufferAndLength(arr, 3);
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(bufAndLen, nullptr, opts);

    auto policy = TOPLEVEL;

    auto Tok = Tokenizer_currentToken(session, policy);

    EXPECT_EQ(Tok, Token(TOKEN_INTEGER, BufferAndLength(session->start + 0, 1), Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    Tokenizer_nextToken(session, policy);
    
    Tok = Tokenizer_currentToken(session, policy);

    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, BufferAndLength(session->start + 1, 3), Source(SourceLocation(1, 2), SourceLocation(2, 1))));

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    session->deinit();

    SUCCEED();
}

TEST_F(CrashTest, Crash1) {
    
    const unsigned char arr[] = {'1', ':', ':', '*', '\\', '\r', '\n'};
    
    auto bufAndLen = BufferAndLength(arr, 7);
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(bufAndLen, nullptr, opts);
    
    auto N = session->parseExpressions();
    
    session->releaseNodeContainer(N);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    session->deinit();
    
    SUCCEED();
}

#if 0
TEST_F(CrashTest, StackOverflow1) {
    
    unsigned char arr[1600];
    for (auto i = 0; i < 1600 ; i++){
        arr[i] = '(';
    }
    
    auto bufAndLen = BufferAndLength(arr, 1600);
    
    TheParserSession->init(bufAndLen, nullptr, SOURCECONVENTION_LINECOLUMN);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    TheParserSession->deinit();
    
    SUCCEED();
}
#endif // #if 0

TEST_F(CrashTest, Crash2) {
    
    const unsigned char arr[] = {'\\', ':', 'f', 'e', 'f', 'f'};

    auto bufAndLen = BufferAndLength(arr, 6);
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(bufAndLen, nullptr, opts);

    auto N = session->parseExpressions();

    session->releaseNodeContainer(N);

    EXPECT_EQ(session->nonFatalIssues.size(), 1u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    session->deinit();

    SUCCEED();
}

TEST_F(CrashTest, Crash3) {
    
    const unsigned char arr[] = {'a', ':', 'b', '~', '1', ':', '2'};

    auto bufAndLen = BufferAndLength(arr, 7);
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(bufAndLen, nullptr, opts);

    auto N = session->parseExpressions();

    session->releaseNodeContainer(N);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    session->deinit();

    SUCCEED();
}

TEST_F(CrashTest, Crash4) {
    
    const unsigned char arr[] = {'\\', '[', 'I', 'n', 't', 'e', 'g', 'r', 'a', 'l', ']', '\\', '[', 'S', 'u', 'm', ']'};

    auto bufAndLen = BufferAndLength(arr, 17);
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(bufAndLen, nullptr, opts);

    auto N = session->parseExpressions();

    session->releaseNodeContainer(N);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    session->deinit();

    SUCCEED();
}

TEST_F(CrashTest, Crash5) {
    
    const unsigned char arr[] = {'{', '\t', '1', '\\', '\n', '^'};

    auto bufAndLen = BufferAndLength(arr, 6);
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_SOURCECHARACTERINDEX;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(bufAndLen, nullptr, opts);

    auto N = session->parseExpressions();

    session->releaseNodeContainer(N);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    session->deinit();

    SUCCEED();
}

