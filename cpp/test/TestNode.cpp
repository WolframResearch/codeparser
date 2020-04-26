
#include "Node.h"

#include "ByteDecoder.h"
#include "ByteBuffer.h"

#include "gtest/gtest.h"

#include <sstream>

class NodeTest : public ::testing::Test {
protected:

};

TEST_F(NodeTest, Bug1) {

    NodeSeq Args;

    std::string input = "a_.";
    
    TheByteBuffer = std::unique_ptr<ByteBuffer>(new ByteBuffer);
    TheByteDecoder = std::unique_ptr<ByteDecoder>(new ByteDecoder);
    
    TheByteBuffer->init(BufferAndLength(Buffer(input.c_str() + 0), 3));
    TheByteDecoder->init(SOURCECONVENTION_LINECOLUMN);
    
    auto T1 = Token(TOKEN_SYMBOL, BufferAndLength(Buffer(input.c_str() + 0), 1), Source(SourceLocation(1, 1), SourceLocation(1, 2)));
    Args.append(std::unique_ptr<Node>(new LeafNode(T1)));
    
    auto T2 = Token(TOKEN_UNDERDOT, BufferAndLength(Buffer(input.c_str() + 1), 2), Source(SourceLocation(1, 2), SourceLocation(1, 4)));
    Args.append(std::unique_ptr<Node>(new LeafNode(T2)));

    auto N = std::unique_ptr<Node>(new CompoundNode(SYMBOL_CODEPARSER_PATTERNOPTIONALDEFAULT, std::move(Args)));

    auto NSource = N->getSource();

    EXPECT_EQ(NSource.Start, SourceLocation(1, 1));
    EXPECT_EQ(NSource.End, SourceLocation(1, 4));
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}
