
#include "ByteBuffer.h"

#include "ParserSession.h"

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS


//
// Precondition: buffer is pointing to current byte
// Postcondition: buffer is pointing to 1 byte past current byte
//
// Return current byte
//
unsigned char ByteBuffer_nextByte(ParserSessionPtr session) {
    
    assert(session->start <= session->buffer);
    assert(session->buffer != session->end);
    assert(session->buffer < session->end);
    
    return *(session->buffer++);
}

unsigned char ByteBuffer_currentByte(ParserSessionPtr session) {
    
    assert(session->start <= session->buffer);
    assert(session->buffer <= session->end);
    
    return *(session->buffer);
}
