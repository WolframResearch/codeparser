
#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "SourceManager.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>

class CharacterDecoderTest : public ::testing::Test {
protected:
    
    static void SetUpTestSuite() {
        TheByteDecoder = new ByteDecoder();
        TheSourceManager = new SourceManager();
        TheCharacterDecoder = new CharacterDecoder();
    }
    
    static void TearDownTestSuite() {
        delete TheSourceManager;
        delete TheCharacterDecoder;
        delete TheByteDecoder;
    }
    
    void SetUp() override {
        TheByteDecoder->init();
        TheCharacterDecoder->init();
    }
    
    void TearDown() override {
        TheCharacterDecoder->deinit();
        TheByteDecoder->deinit();
        TheSourceManager->deinit();
    }
};

TEST_F(CharacterDecoderTest, Basic) {
    
    auto iss = std::stringstream("1+2");
    
    TheSourceManager->init(iss, nullptr);
    
    auto C = TheCharacterDecoder->nextWLCharacter();
    EXPECT_EQ(C, WLCharacter('1'));
    
    C = TheCharacterDecoder->nextWLCharacter();
    EXPECT_EQ(C, WLCharacter('+'));
    
    C = TheCharacterDecoder->nextWLCharacter();
    EXPECT_EQ(C, WLCharacter('2'));
    
    C = TheCharacterDecoder->nextWLCharacter();
    EXPECT_EQ(C, WLCharacter(CODEPOINT_ENDOFFILE));
}
