
#pragma once

//#include "API.h"
#include "Source.h" // for BufferAndLength

#include "WolframLibrary.h"
#undef True
#undef False

//#include <cstddef>
//#include <vector>
//#include <istream>
#include <memory> // for unique_ptr
//#include <ostream>

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
};

extern ByteBufferPtr TheByteBuffer;

