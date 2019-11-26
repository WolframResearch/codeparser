
#include "ByteDecoder.h"
#include "API.h"
#include "CodePoint.h"

#include "gtest/gtest.h"

#include <sstream>

// Example: given the list { 'a', 'b', '\n', 'c', 'd', 'e', '\n', 'f'}
// the result would be < 0, 2, 6 >
//
// Example: given the list { 'a', 'b', '\r', '\n', 'c', 'd', 'e', '\r', '\n', 'f'}
// the result would be < 0, 3, 8 >
//
// Example: given the list { '\n', 'c', 'd', 'e', '\n', 'f'}
// the result would be < 0, 0, 4 >
//
// Example: given the list { '\r', '\n', 'c', 'd', 'e', '\r', '\n', 'f'}
// the result would be < 0, 1, 6 >

TEST(OffsetLineMap, Setup1) {
    
    auto strIn = std::string("ab\ncde\nf");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
//    auto endMap = TheByteDecoder->getEndOffsetLineMap();
    
    EXPECT_EQ(map.size(), 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 2);
    EXPECT_EQ(map[2], str + 6);
    EXPECT_EQ(map[3], str + 8);
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Setup2) {
    
    auto strIn = std::string("ab\r\ncde\r\nf");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
//    auto endMap = TheByteDecoder->getEndOffsetLineMap();
    
    EXPECT_EQ(TheByteDecoder->actualOffsetLineMapSize, 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 3);
    EXPECT_EQ(map[2], str + 8);
    EXPECT_EQ(map[3], str + 10);
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Setup3) {
    
    auto strIn = std::string("\ncde\nf");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
//    auto endMap = TheByteDecoder->getEndOffsetLineMap();
    
    
    EXPECT_EQ(TheByteDecoder->actualOffsetLineMapSize, 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 0);
    EXPECT_EQ(map[2], str + 4);
    EXPECT_EQ(map[3], str + 6);
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Setup4) {
    
    auto strIn = std::string("\r\ncde\r\nf");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
//    auto endMap = TheByteDecoder->getEndOffsetLineMap();
    
    EXPECT_EQ(TheByteDecoder->actualOffsetLineMapSize, 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 1);
    EXPECT_EQ(map[2], str + 6);
    EXPECT_EQ(map[3], str + 8);
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Setup5) {
    
    auto strIn = std::string("\r\n\r\nA");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
//    auto endMap = TheByteDecoder->getEndOffsetLineMap();
    
    
    EXPECT_EQ(TheByteDecoder->actualOffsetLineMapSize, 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 1);
    EXPECT_EQ(map[2], str + 3);
    EXPECT_EQ(map[3], str + 5);
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Setup6) {
    
    auto strIn = std::string("ab\rcde\rf");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
//    auto endMap = TheByteDecoder->getEndOffsetLineMap();
    
    
    EXPECT_EQ(TheByteDecoder->actualOffsetLineMapSize, 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 2);
    EXPECT_EQ(map[2], str + 6);
    EXPECT_EQ(map[3], str + 8);
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Setup7) {
    
    auto strIn = std::string("\rcde\rf");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
//    auto endMap = TheByteDecoder->getEndOffsetLineMap();
    
    
    EXPECT_EQ(TheByteDecoder->actualOffsetLineMapSize, 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 0);
    EXPECT_EQ(map[2], str + 4);
    EXPECT_EQ(map[3], str + 6);
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Setup8) {
    
    auto strIn = std::string("\r\rA");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto map = TheByteDecoder->getOffsetLineMap();
    
    EXPECT_EQ(TheByteDecoder->actualOffsetLineMapSize, 4u);
    
    EXPECT_EQ(map[0], str + 0);
    EXPECT_EQ(map[1], str + 0);
    EXPECT_EQ(map[2], str + 1);
    EXPECT_EQ(map[3], str + 3);
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}









TEST(OffsetLineMap, Convert1) {
    
    auto strIn = std::string("1+2");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto buf = str;
    
    auto loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 2));
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 3));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 3));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 4));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 4));
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Convert2) {
    
    auto strIn = std::string("\n1+1\n");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto buf = str;
    
    auto loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 3));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 3));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 4));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(3, 1));
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Convert3) {
    
    auto strIn = std::string("{\n}");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto buf = str;
    
    auto loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 2));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Convert4) {
    
    auto strIn = std::string("\"abc\\\r\ndef\"");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto buf = str;
    
    auto loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 2));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 3));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 3));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 4));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 4));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 5));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 5));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 6));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 6));
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 3));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 3));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 4));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 4));
    
    
    
    
    
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}


TEST(OffsetLineMap, Convert5) {
    
    auto strIn = std::string("\r\n\r\nA");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto buf = str;
    
    auto loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(3, 1));
    
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(3, 2));
    
    
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Convert6) {
    
    auto strIn = std::string("\r\rA");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto buf = str;
    
    auto loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(3, 1));
    
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(3, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(3, 2));
    
    
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

TEST(OffsetLineMap, Convert7) {
    
    auto strIn = std::string("{a;\n}");
    
    auto str = reinterpret_cast<Buffer>(strIn.c_str());
    
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    
    
    TheByteBuffer->init(BufferAndLength(str, strIn.size(), false));
    
    TheByteDecoder->init();
    
    
    auto buf = str;
    
    auto loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 1));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 2));
    
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(1, 3));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 3));
    
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 0));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(1, 4));
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 1));
    
    
    
    
    buf = buf + 1;
    
    loc = TheByteDecoder->convertBufferToStart(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    loc = TheByteDecoder->convertBufferToEnd(buf);
    EXPECT_EQ(loc, SourceLocation(2, 2));
    
    
    
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

