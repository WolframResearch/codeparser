
#include "ByteBuffer.h"

#include "ParserSession.h"

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

#include <cstddef> // for size_t


//
// Precondition: buffer is pointing to current byte
// Postcondition: buffer is pointing to 1 byte past current byte
//
// Return current byte
//
unsigned char ByteBuffer_nextByte0(ParserSessionPtr session) {
    
    assert((session->start <= session->buffer && session->buffer <= session->end) && "Fix at call site");
    
    if (session->buffer == session->end) {
        
        session->wasEOF = true;
        
        return 0xff;
    }
    
    auto b = *session->buffer;
    ++session->buffer;
    
    //
    // if eof, then force 0xff to be returned
    //
    // but try really hard to be branchless
    // this will pay off more as more code becomes branchless
    //
    //return b | ((*eof ^ 0xff) - 0xff);
    return b;
}

void ByteBuffer_nextByte(ParserSessionPtr session) {
    
    assert((session->start <= session->buffer && session->buffer <= session->end) && "Fix at call site");
    
    if (session->buffer == session->end) {
        return;
    }
    
    ++session->buffer;
}

unsigned char ByteBuffer_currentByte(ParserSessionPtr session) {
    
    assert((session->start <= session->buffer && session->buffer <= session->end) && "Fix at call site");
    
    if (session->buffer == session->end) {
        return 0xff;
    }
    
    return *session->buffer;
}
