
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>

static std::unique_ptr<MLSession> mlSession;

class CharacterDecoderTest : public ::testing::Test {
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
        TheParserSession->deinit();
    }
};

TEST_F(CharacterDecoderTest, Basic) {
    
    auto strIn = std::string("1+2");
    
    auto str = reinterpret_cast<const unsigned char *>(strIn.c_str());
    
    TheParserSession->init(nullptr, str, strIn.size(), SOURCESTYLE_LINECOL, false, false, false);
    
    auto T = TheTokenizer->currentToken();
    EXPECT_EQ(T, Token(TOKEN_INTEGER, "1", Source(SourceLocation(LineCol(1, 1)))));
    
    auto C = TheCharacterDecoder->currentWLCharacter();
    EXPECT_EQ(C, WLCharacter('+'));
    
    C = TheCharacterDecoder->nextWLCharacter();
    EXPECT_EQ(C, WLCharacter('2'));
    
    C = TheCharacterDecoder->nextWLCharacter();
    EXPECT_EQ(C, WLCharacter(CODEPOINT_ENDOFFILE));
}
