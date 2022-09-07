
#include "Tokenizer.h"
#include "ParserSession.h"
#include "TokenEnumRegistration.h"

#include "gtest/gtest.h"

class ParserSession;

using ParserSessionPtr = ParserSession *;


class TokenizerTest : public ::testing::Test {
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

ParserSessionPtr TokenizerTest::session;


//
// This was asserting
//
TEST_F(TokenizerTest, Bug1) {
    
    auto strIn = std::string("\\.GG");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug2) {
    
    auto strIn = std::string("<<<");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);
    
    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    Tok.skip(session);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

//
// This used to assert
//
TEST_F(TokenizerTest, Bug3) {
    
    auto strIn = std::string("\\\r");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug4) {
    
    auto strIn = std::string("\\[");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

TEST_F(TokenizerTest, Bug5) {
    
    auto strIn = std::string("\"a\\\\\r\nb\"");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

TEST_F(TokenizerTest, IntegerRealMixup) {
    
    auto strIn = std::string("0..");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok1 = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok1, Token(TOKEN_INTEGER, session->start + 0, 1, Source(SourceLocation(1, 1), SourceLocation(1, 2))));

    Tok1.skip(session);

    auto Tok2 = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok2, Token(TOKEN_DOTDOT, session->start + 1, 2, Source(SourceLocation(1, 2), SourceLocation(1, 4))));

    Tok2.skip(session);

    auto Tok3 = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok3, Token(TOKEN_ENDOFFILE, session->start + 3, 1, Source(SourceLocation(1, 4), SourceLocation(1, 4))));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 1u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(TokenizerTest, Basic2) {
    
    auto strIn = std::string("\\[Alpha]bc+1");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok1 = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok1, Token(TOKEN_SYMBOL, session->start + 0, 10, Source(SourceLocation(1, 1), SourceLocation(1, 11))));

    Tok1.skip(session);

    auto Tok2 = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok2, Token(TOKEN_PLUS, session->start + 10, 1, Source(SourceLocation(1, 11), SourceLocation(1, 12))));

    Tok2.skip(session);

    auto Tok3 = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok3, Token(TOKEN_INTEGER, session->start + 11, 1, Source(SourceLocation(1, 12), SourceLocation(1, 13))));

    Tok3.skip(session);

    auto Tok4 = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok4, Token(TOKEN_ENDOFFILE, session->start + 12, 1, Source(SourceLocation(1, 13), SourceLocation(1, 13))));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(TokenizerTest, OldAssert1) {
    
    auto strIn = std::string("8*");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_INTEGER, session->start, 1, Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(TokenizerTest, Basic3) {
    
    auto strIn = std::string("{\n}");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_OPENCURLY, session->start, 1, Source(SourceLocation(1, 1), SourceLocation(1, 2))));

    Tok.skip(session);

    //
    // Clear 0x100 because we are inside a group now
    //
    Tok = Tokenizer_currentToken(session, TOPLEVEL & ~(RETURN_TOPLEVELNEWLINE));

    EXPECT_EQ(Tok, Token(TOKEN_INTERNALNEWLINE, session->start + 1, 1, Source(SourceLocation(1, 2), SourceLocation(2, 1))));

    Tok.skip(session);

    Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_CLOSECURLY, session->start + 2, 1, Source(SourceLocation(2, 1), SourceLocation(2, 2))));

    Tok.skip(session);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(TokenizerTest, Basic4) {
    
    const unsigned char arr[] = { 0xff };
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 1, nullptr, opts);
    
    EXPECT_EQ(session->SrcLoc, SourceLocation(1, 1));
    
    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_ERROR_UNSAFECHARACTERENCODING, session->start + 0, 1, Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    EXPECT_EQ(session->SrcLoc, SourceLocation(1, 1));
    
    Tok.skip(session);
    
    Tok = Tokenizer_currentToken(session, TOPLEVEL);
    
    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, session->start + 1, 1, Source(SourceLocation(1, 2), SourceLocation(1, 2))));
    
    Tok.skip(session);
    
    EXPECT_EQ(session->SrcLoc, SourceLocation(1, 2));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

TEST_F(TokenizerTest, Crash1) {
    
    const unsigned char arr[] = { '6', '`', '5', '.', '.' };
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(arr, 5, nullptr, opts);
    
    Tokenizer_currentToken(session, TOPLEVEL);
    
    EXPECT_EQ(session->nonFatalIssues.size(), 1u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    
    SUCCEED();
}

TEST_F(TokenizerTest, LineContinuation1) {

    auto strIn = std::string("ab\\\ncd");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_SYMBOL, session->start + 0, 6, Source(SourceLocation(1, 1), SourceLocation(2, 3))));
    
    Tokenizer_nextToken(session, TOPLEVEL);
    
    Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, session->start + 6, 1, Source(SourceLocation(2, 3), SourceLocation(2, 3))));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(TokenizerTest, LineContinuation2) {
    
    auto strIn = std::string("ab\\\r\ncd");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_SYMBOL, session->start + 0, 7, Source(SourceLocation(1, 1), SourceLocation(2, 3))));
    
    Tokenizer_nextToken(session, TOPLEVEL);
    
    Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, session->start + 7, 1, Source(SourceLocation(2, 3), SourceLocation(2, 3))));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(TokenizerTest, LineContinuation3) {
    
    auto strIn = std::string("ab\\\rcd");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_SYMBOL, session->start + 0, 6, Source(SourceLocation(1, 1), SourceLocation(2, 3))));

    Tokenizer_nextToken(session, TOPLEVEL);
    
    Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, session->start + 6, 1, Source(SourceLocation(2, 3), SourceLocation(2, 3))));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 1u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(TokenizerTest, LineContinuation4) {
    
    auto strIn = std::string("1\\\n");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session->init(str, strIn.size(), nullptr, opts);

    auto Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_INTEGER, session->start + 0, 1, Source(SourceLocation(1, 1), SourceLocation(1, 2))));
    
    Tokenizer_nextToken(session, TOPLEVEL);
    
    Tok = Tokenizer_currentToken(session, TOPLEVEL);

    EXPECT_EQ(Tok, Token(TOKEN_ENDOFFILE, session->start + 1, 3, Source(SourceLocation(1, 2), SourceLocation(2, 1))));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}
