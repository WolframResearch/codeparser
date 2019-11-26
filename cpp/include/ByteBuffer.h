
#pragma once

#include "API.h"

#include <cstddef>
#include <vector>
#include <istream>
#include <memory> // for unique_ptr
#include <ostream>

class ByteBuffer;
using ByteBufferPtr = std::unique_ptr<ByteBuffer>;


class ByteBuffer {
    
    BufferAndLength origBufAndLen;
    
    WolframLibraryData libData;
    
    
public:
    
    Buffer buffer;
    
    Buffer end;
    
    bool wasEOF;
    
    
    ByteBuffer();
    
    void init(BufferAndLength bufAndLen, WolframLibraryData libData = nullptr);
    
    void deinit();
    
    unsigned char currentByte();
    
    void nextByte();
    
    unsigned char nextByte0();
    
    bool eof() const;
};

extern ByteBufferPtr TheByteBuffer;

