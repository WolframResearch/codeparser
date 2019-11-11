
#pragma once

#include "Source.h"

#include "WolframLibrary.h"

#undef True

#undef False

#include <cstddef>
#include <vector>
#include <istream>
#include <memory> // for unique_ptr

class ByteBuffer {
    
    const unsigned char *data;
    size_t dataByteCount;
    size_t dataByteIndex;
    
    WolframLibraryData libData;
    
public:
    ByteBuffer();
    
    void init(const unsigned char *data, size_t dataLength, WolframLibraryData libData);
    
    void deinit();
    
    unsigned char nextByte(unsigned char *eof);
    
    size_t getDataByteIndex() const;
    
    void setDataByteIndex(size_t );
};

extern std::unique_ptr<ByteBuffer> TheByteBuffer;

