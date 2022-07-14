
#include "CharacterDecoder.h"
#include "ParserSession.h"

#include "gtest/gtest.h"

class ParserSession;

using ParserSessionPtr = ParserSession *;


class CharacterDecoderTest : public ::testing::Test {
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

ParserSessionPtr CharacterDecoderTest::session;


TEST_F(CharacterDecoderTest, Basic1) {
    
    auto strIn = std::string("1+2");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('2'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(CharacterDecoderTest, LongName) {
    
    auto strIn = std::string("1+\\[Alpha]");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(0x03b1, ESCAPE_LONGNAME));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(CharacterDecoderTest, 4Hex) {
    
    auto strIn = std::string("1+\\:03b1");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(0x03b1, ESCAPE_4HEX));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(CharacterDecoderTest, 2Hex) {
    
    auto strIn = std::string("1+\\.f2");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(242, ESCAPE_2HEX));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(CharacterDecoderTest, Octal) {
    
    auto strIn = std::string("1+\\333");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(219, ESCAPE_OCTAL));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(CharacterDecoderTest, 6Hex) {
    
    auto strIn = std::string("1+\\|0000f2");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(242, ESCAPE_6HEX));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(CharacterDecoderTest, Raw) {
    
    auto strIn = std::string("1+\\[RawWedge]");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('^', ESCAPE_RAW));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 0u);
}

TEST_F(CharacterDecoderTest, LongNameError1) {
    
    auto strIn = std::string("1+\\[Alpha+2");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('\\'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('['));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('A'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('l'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('p'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('h'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('a'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('2'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

TEST_F(CharacterDecoderTest, LongNameError2) {
    
    auto strIn = std::string("1+\\[Alpa]+2");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('\\'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('['));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('A'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('l'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('p'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('a'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(']'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('2'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

TEST_F(CharacterDecoderTest, 4HexError1) {
    
    auto strIn = std::string("1+\\:03b+1");

    auto str = reinterpret_cast<Buffer>(strIn.c_str());

    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('\\'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(':'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('0'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('3'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('b'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('+'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter('1'));

    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);

    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);

    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
    EXPECT_EQ(session->fatalIssues.size(), 1u);
}

//
// There was a bug where UnexpectedEscapeSequence issues were being added by mistake
//
TEST_F(CharacterDecoderTest, UnexpectedEscapeSequence) {
    
    auto strIn = std::string("\"\\[Alpha]\"");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    session->init(BufferAndLength(str, strIn.size()), nullptr, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);
    
    EXPECT_EQ(c, WLCharacter('"'));
    
    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);
    
    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);
    
    EXPECT_EQ(c, WLCharacter(0x03b1, ESCAPE_LONGNAME));
    
    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);
    
    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);
    
    EXPECT_EQ(c, WLCharacter('"'));
    
    CharacterDecoder_nextWLCharacter(session, TOPLEVEL);
    
    c = CharacterDecoder_currentWLCharacter(session, TOPLEVEL);
    
    EXPECT_EQ(c, WLCharacter(CODEPOINT_ENDOFFILE));
    
    EXPECT_EQ(session->fatalIssues.size(), 0u);
    EXPECT_EQ(session->nonFatalIssues.size(), 0u);
}
