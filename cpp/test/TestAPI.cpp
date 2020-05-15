
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>


class APITest : public ::testing::Test {
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

//
// this used to assert
//
TEST_F(APITest, Bug1) {
    
    auto strIn = std::string("abc[]");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->concreteParseLeaf(STRINGIFYMODE_NORMAL);
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to hang
//
TEST_F(APITest, Hang1) {
    
    auto strIn = std::string("<<rr[R");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to crash
//
TEST_F(APITest, Crash1) {
    
    auto strIn = std::string("0^^");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to crash
//
TEST_F(APITest, Crash2) {
    
    auto strIn = std::string(".2^^0");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to crash
//
TEST_F(APITest, Crash3) {
    
    auto strIn = std::string("12^^a.a");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to crash
//
TEST_F(APITest, Crash4) {
    
    auto strIn = std::string("12..");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to crash
//
TEST_F(APITest, Crash5) {
    
    auto strIn = std::string("123\\\n.45");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to crash
//
TEST_F(APITest, Crash6) {
    
    auto strIn = std::string("\\0560");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

//
// this used to crash
//
TEST_F(APITest, Crash7) {
    
    auto strIn = std::string("1+1");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    auto bufAndLen = BufferAndLength(str, strIn.size());
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_UNKNOWN, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
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
    
    TheParserSession->init(bufAndLen, nullptr, INCLUDE_SOURCE, SOURCECONVENTION_SOURCECHARACTERINDEX, DEFAULT_TAB_WIDTH);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    SUCCEED();
}

