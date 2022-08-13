
#include "ParserSession.h"

#include "gtest/gtest.h"

#include <sstream>

class ParserSession;

using ParserSessionPtr = ParserSession *;


class APITest : public ::testing::Test {
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

ParserSessionPtr APITest::session;


//
// this used to assert
//
TEST_F(APITest, Bug1) {
    
    auto strIn = std::string("abc[]");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(bufAndLen, nullptr, opts);

    auto N = session->concreteParseLeaf(STRINGIFYMODE_NORMAL);

    session->releaseNodeContainer(N);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    session->deinit();

    SUCCEED();
}

//
// this used to hang
//
TEST_F(APITest, Hang1) {
    
    auto strIn = std::string("<<rr[R");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    auto bufAndLen = BufferAndLength(str, strIn.size());
    
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

//
// this used to crash
//
TEST_F(APITest, Crash1) {
    
    auto strIn = std::string("0^^");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    auto bufAndLen = BufferAndLength(str, strIn.size());
    
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

//
// this used to crash
//
TEST_F(APITest, Crash2) {
    
    auto strIn = std::string(".2^^0");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
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

//
// this used to crash
//
TEST_F(APITest, Crash3) {
    
    auto strIn = std::string("12^^a.a");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
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

//
// this used to crash
//
TEST_F(APITest, Crash4) {
    
    auto strIn = std::string("12..");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    auto bufAndLen = BufferAndLength(str, strIn.size());
    
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

//
// this used to crash
//
TEST_F(APITest, Crash5) {
    
    auto strIn = std::string("123\\\n.45");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    auto bufAndLen = BufferAndLength(str, strIn.size());
    
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

//
// this used to crash
//
TEST_F(APITest, Crash6) {
    
    auto strIn = std::string("\\0560");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
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

//
// this used to crash
//
TEST_F(APITest, Crash7) {
    
    auto strIn = std::string("1+1");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    //
    // this was originally using SOURCECONVENTION_UNKNOWN, which was 0
    // but 0 is now SOURCECONVENTION_LINECOLUMN
    // so make up a bogus SourceConvention of 2
    //
    
    ParserSessionOptions opts;
    opts.srcConvention = static_cast<SourceConvention>(2);
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    auto res = session->init(bufAndLen, nullptr, opts);
    
    EXPECT_EQ(res, PARSERSESSIONINIT_ERROR);

    SUCCEED();
}

//
// this used to crash
//
// CODETOOLS-62
//
TEST_F(APITest, Crash8) {
    
    const unsigned char arr[] = {'(', '*', '\r', '\n', '*', ')'};

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

