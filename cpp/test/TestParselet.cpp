
#include "Parselet.h"

#include "ByteDecoder.h"
#include "SourceManager.h"
#include "CharacterDecoder.h"
#include "Tokenizer.h"
#include "Symbol.h"

#include "gtest/gtest.h"

#include <sstream>

MLENV ep;
MLINK mlp;

class ParseletTest : public ::testing::Test {
protected:
    
    static void SetUpTestSuite() {
        
        allocSymbols();
        
        TheByteDecoder = new ByteDecoder();
        TheSourceManager = new SourceManager();
        TheCharacterDecoder = new CharacterDecoder();
        TheTokenizer = new Tokenizer();
        TheParser = new Parser();
        
        MLEnvironmentParameter p;
        int err;
        
        p = MLNewParameters(MLREVISION, MLAPIREVISION);
#ifdef WINDOWS_MATHLINK
        
#else
        //
        // Needed because MathLink intercepts all signals
        //
        MLDoNotHandleSignalParameter(p, SIGINT);
#endif
        ep = MLInitialize(p);
        if (ep == (MLENV)0) {
            return;
        }
        
        WolframLibrary_initialize(nullptr);
        
        mlp = MLLoopbackOpen(ep, &err);
    }
    
    static void TearDownTestSuite() {
        
        if (mlp != nullptr) {
            MLClose(mlp);
        }
        if (ep != 0) {
            MLDeinitialize(ep);
        }
        
        delete TheParser;
        delete TheTokenizer;
        delete TheSourceManager;
        delete TheCharacterDecoder;
        delete TheByteDecoder;
        
        freeSymbols();
    }
    
    void SetUp() override {
        TheByteDecoder->init();
        TheCharacterDecoder->init();
        TheParser->init( []() {
            return false;
        }, { } );
    }
    
    void TearDown() override {
        TheParser->deinit();
        TheTokenizer->deinit();
        TheCharacterDecoder->deinit();
        TheByteDecoder->deinit();
        TheSourceManager->deinit();
    }

};

TEST_F(ParseletTest, Bug1) {
    
    auto iss = std::stringstream("a /: b := c");
    
    TheSourceManager->init(iss, nullptr);
    TheTokenizer->init(false);
    
    ParserContext Ctxt;
    
    TheParser->parse(Ctxt);
}
