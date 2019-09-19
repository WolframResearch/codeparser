
#include "Node.h"

#include "gtest/gtest.h"

#include <sstream>

class NodeTest : public ::testing::Test {
protected:

};

TEST_F(NodeTest, Bug1) {

    auto Args = std::unique_ptr<NodeSeq>(new NodeSeq);

    auto T1 = Token(TOKEN_SYMBOL, "a", Source(SourceLocation(1, 1), SourceLocation(1, 1)));
    Args->append(std::unique_ptr<Node>(new LeafNode(T1)));

    auto T2 = Token(TOKEN_UNDERDOT, "_.", Source(SourceLocation(1, 2), SourceLocation(1, 3)));
    Args->append(std::unique_ptr<Node>(new LeafNode(T2)));

    auto N = std::unique_ptr<Node>(new OptionalDefaultPatternNode(std::move(Args)));

    auto NSource = N->getSourceSpan();

    EXPECT_EQ(NSource.lines.start, SourceLocation(1, 1));
    EXPECT_EQ(NSource.lines.end, SourceLocation(1, 3));
}
