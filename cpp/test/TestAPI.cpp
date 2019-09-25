
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>

static std::unique_ptr<MLSession> mlSession;

class APITest : public ::testing::Test {
protected:
    static void SetUpTestSuite() {
        
        mlSession = std::unique_ptr<MLSession>(new MLSession);
        
        TheParserSession = std::unique_ptr<ParserSession>(new ParserSession);
    }
    
    static void TearDownTestSuite() {
        
        TheParserSession.reset(nullptr);
        
        mlSession.reset(nullptr);
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
    
    auto link = mlSession->getMLINK();
    
    MLPutFunction(link, "List", 2);
    
    MLPutString(link, "abc[]");
    
    MLPutString(link, "LineCol");
    
    ParseLeaf_LibraryLink(nullptr, link);
    
    SUCCEED();
}
