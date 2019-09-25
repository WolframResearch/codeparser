
#include "Node.h"

#include "gtest/gtest.h"

#include <sstream>

class NodeTest : public ::testing::Test {
protected:

};

TEST_F(NodeTest, Bug1) {

    auto Args = std::unique_ptr<NodeSeq>(new NodeSeq);

    auto T1 = Token(TOKEN_SYMBOL, "a", Source(SourceLocation(LineCol(1, 1))));
    Args->append(std::unique_ptr<Node>(new LeafNode(T1)));

    auto T2 = Token(TOKEN_UNDERDOT, "_.", Source(SourceLocation(LineCol(1, 2)), SourceLocation(LineCol(1, 3))));
    Args->append(std::unique_ptr<Node>(new LeafNode(T2)));

    auto N = std::unique_ptr<Node>(new OptionalDefaultPatternNode(std::move(Args)));

    auto NSource = N->getSource();

    EXPECT_EQ(NSource.lineCol.start, LineCol(1, 1));
    EXPECT_EQ(NSource.lineCol.end, LineCol(1, 3));
}
