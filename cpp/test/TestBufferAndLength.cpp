
#include "Source.h"
#include "API.h"

#include "gtest/gtest.h"

#include <sstream>


class BufferAndLengthTest : public ::testing::Test {
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

TEST_F(BufferAndLengthTest, Hang1) {
    
    const unsigned char arr[] = {0xf3, '=', '\\', '\n', ' '};

    //
    // The actual test for the hang is with this buffer created to have length < 5.
    // But I have added an assert against that possibility, so this is no longer a direct test.
    //
    TheParserSession->init(BufferAndLength(arr, 5), nullptr, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto bufLen = BufferAndLength(arr, 5, UTF8STATUS_INVALID);

    std::ostringstream newStrStream;

    bufLen.printUTF8String(newStrStream);
    
    auto str = newStrStream.str();
    
    EXPECT_EQ(str, "\xef\xbf\xbd=\\\n ");
}
