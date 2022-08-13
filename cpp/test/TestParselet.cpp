
#include "Tokenizer.h"
#include "ParseletRegistration.h"
#include "Parselet.h"
#include "ParserSession.h"

#include "gtest/gtest.h"

class ParserSession;

using ParserSessionPtr = ParserSession *;


class ParseletTest : public ::testing::Test {
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

ParserSessionPtr ParseletTest::session;


TEST_F(ParseletTest, Bug1) {
    
    auto strIn = std::string("a /: b := c");

    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(BufferAndLength(str, strIn.size()), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    (prefixParselets[Tok.Tok.value()]->parsePrefix())(session, prefixParselets[Tok.Tok.value()], Tok);

    auto& N = Parser_topNode(session);

    EXPECT_TRUE(std::holds_alternative<NodePtr>(N));
    
    auto& P = std::get<NodePtr>(N);
    
    EXPECT_TRUE(dynamic_cast<TernaryNode *>(P));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug2) {
//
    auto strIn = std::string("a<b ");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(BufferAndLength(str, strIn.size()), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    (prefixParselets[Tok.Tok.value()]->parsePrefix())(session, prefixParselets[Tok.Tok.value()], Tok);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug3) {
    
    auto strIn = std::string("a\\[Integral]b\\[Integral]c ");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(BufferAndLength(str, strIn.size()), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    (prefixParselets[Tok.Tok.value()]->parsePrefix())(session, prefixParselets[Tok.Tok.value()], Tok);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(ParseletTest, Bug4) {
    
    auto strIn = std::string("\\[RawLeftBrace]*\\[RawRightBrace]");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(BufferAndLength(str, strIn.size()), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    (prefixParselets[Tok.Tok.value()]->parsePrefix())(session, prefixParselets[Tok.Tok.value()], Tok);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}
