
#pragma once

#include "Source.h" // for BufferAndLength

#include <memory> // for unique_ptr

class ByteBuffer;

using ByteBufferPtr = std::unique_ptr<ByteBuffer>;


//
// A byte buffer that can return the current byte and advance to the next byte.
//
class ByteBuffer {
    
public:
    
    Buffer buffer;
    
    Buffer end;
    
    bool wasEOF;
    
    
    ByteBuffer();
    
    void init();
    
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

