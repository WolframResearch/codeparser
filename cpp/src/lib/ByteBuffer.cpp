
#include "ByteBuffer.h"

#include "CodePoint.h"

ByteBuffer::ByteBuffer() : origBufAndLen(), libData(), buffer(), end(), wasEOF() {}

void ByteBuffer::init(BufferAndLength bufAndLenIn, WolframLibraryData libDataIn) {
  
    origBufAndLen = bufAndLenIn;
    
    buffer = bufAndLenIn.buffer;
    
    libData = libDataIn;
    
    end = origBufAndLen.end();
    
    wasEOF = false;
}

void ByteBuffer::deinit() {}

//
// Precondition: buffer is pointing to current byte
// Postcondition: buffer is pointing to 1 byte past current byte
//
// Return current byte
//
unsigned char ByteBuffer::nextByte0() {
    
    assert((origBufAndLen.buffer <= buffer && buffer <= end) && "Fix at call site");
    
#ifndef NDEBUG
    size_t oldProgress;
    if (origBufAndLen.length != 0) {
        oldProgress = (100 * (buffer - origBufAndLen.buffer) / origBufAndLen.length);
    }
#endif // NDEBUG
    
    if (buffer == end) {
        
        wasEOF = true;
        
        return 0xff;
    }
    
    auto b = *buffer;
    ++buffer;
    
#ifndef NDEBUG
#if USE_MATHLINK
    if (origBufAndLen.length != 0) {
        
        size_t progress = (100 * (buffer - origBufAndLen.buffer) / origBufAndLen.length);
        
        if (progress != oldProgress) {
            if (libData) {
                MLINK link = libData->getMathLink(libData);
                if (!MLPutFunction(link, "EvaluatePacket", 1)) {
                    assert(false);
                }
                if (!MLPutFunction(link, "AST`Library`SetConcreteParseProgress", 1)) {
                    assert(false);
                }
                if (!MLPutInteger(link, static_cast<int>(progress))) {
                    assert(false);
                }
                if (!libData->processMathLink(link)) {
                    assert(false);
                }
                auto pkt = MLNextPacket(link);
                if (pkt == RETURNPKT) {
                    if(!MLNewPacket(link)) {
                        assert(false);
                    }
                } else {
                    assert(false);
                }
            }
        }
    }
#endif // USE_MATHLINK
#endif // NDEBUG
    
    //
    // if eof, then force 0xff to be returned
    //
    // but try really hard to be branchless
    // this will pay off more as more code becomes branchless
    //
    //return b | ((*eof ^ 0xff) - 0xff);
    return b;
}

void ByteBuffer::nextByte() {
    
    assert((origBufAndLen.buffer <= buffer && buffer <= end) && "Fix at call site");
    
#ifndef NDEBUG
    size_t oldProgress;
    if (origBufAndLen.length != 0) {
        oldProgress = (100 * (buffer - origBufAndLen.buffer) / origBufAndLen.length);
    }
#endif
    
    if (buffer == end) {
        
        return;
    }
    
    ++buffer;
    
#ifndef NDEBUG
#if USE_MATHLINK
    if (origBufAndLen.length != 0) {
        
        size_t progress = (100 * (buffer - origBufAndLen.buffer) / origBufAndLen.length);
        
        if (progress != oldProgress) {
            if (libData) {
                MLINK link = libData->getMathLink(libData);
                if (!MLPutFunction(link, "EvaluatePacket", 1)) {
                    assert(false);
                }
                if (!MLPutFunction(link, "AST`Library`SetConcreteParseProgress", 1)) {
                    assert(false);
                }
                if (!MLPutInteger(link, static_cast<int>(progress))) {
                    assert(false);
                }
                if (!libData->processMathLink(link)) {
                    assert(false);
                }
                auto pkt = MLNextPacket(link);
                if (pkt == RETURNPKT) {
                    if(!MLNewPacket(link)) {
                        assert(false);
                    }
                } else {
                    assert(false);
                }
            }
        }
    }
#endif // USE_MATHLINK
#endif // NDEBUG
}

unsigned char ByteBuffer::currentByte() {
    
    assert((origBufAndLen.buffer <= buffer && buffer <= end) && "Fix at call site");
    
    if (buffer == end) {
        
        return 0xff;
    }
    
    return *buffer;
}

ByteBufferPtr TheByteBuffer = nullptr;
