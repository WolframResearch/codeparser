
#include "Node.h"
#include "ParserSession.h"
#include "TokenEnumRegistration.h"
#include "SymbolRegistration.h"

#include "gtest/gtest.h"

class ParserSession;

using ParserSessionPtr = ParserSession *;


class NodeTest : public ::testing::Test {
protected:

};

TEST_F(NodeTest, Bug1) {

    NodeSeq Args;

    std::string input = "a_.";
    
    ParserSession session;
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    session.init(BufferAndLength(Buffer(input.c_str() + 0), 3), nullptr, opts);
    
    auto T1 = Token(TOKEN_SYMBOL, BufferAndLength(Buffer(input.c_str() + 0), 1), Source(SourceLocation(1, 1), SourceLocation(1, 2)));
    Args.push(T1);
    
    auto T2 = Token(TOKEN_UNDERDOT, BufferAndLength(Buffer(input.c_str() + 1), 2), Source(SourceLocation(1, 2), SourceLocation(1, 4)));
    Args.push(T2);

    auto N = std::unique_ptr<Node>(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));

    auto NSource = N->getSource();

    EXPECT_EQ(NSource.Start, SourceLocation(1, 1));
    EXPECT_EQ(NSource.End, SourceLocation(1, 4));
    
    EXPECT_EQ(session.nonFatalIssues.size(), 0u);
    EXPECT_EQ(session.fatalIssues.size(), 0u);
    
    session.deinit();
}
