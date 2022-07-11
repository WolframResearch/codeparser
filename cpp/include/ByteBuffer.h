
#pragma once

#include "Source.h" // for BufferAndLength

class ParserSession;

using ParserSessionPtr = ParserSession *;


//
// A byte buffer that can return the current byte and advance to the next byte.
//

unsigned char ByteBuffer_currentByte(ParserSessionPtr session);
unsigned char ByteBuffer_nextByte(ParserSessionPtr session);
