
#pragma once

#include "Source.h" // for BufferAndLength

#include "WolframLibrary.h"
#undef True
#undef False

#include <memory> // for unique_ptr

class ByteBuffer;
using ByteBufferPtr = std::unique_ptr<ByteBuffer>;

//
//
//
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
    
    //
    // Precondition: buffer is pointing to current byte
    // Postcondition: buffer is pointing to 1 byte past current byte
    //
    // Return current byte
    //
    unsigned char nextByte0();
};

extern ByteBufferPtr TheByteBuffer;

