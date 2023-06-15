//! A byte buffer that can return the current byte and advance to the next byte.

use crate::tokenizer::Tokenizer;

//
// Precondition: buffer is pointing to current byte
// Postcondition: buffer is pointing to 1 byte past current byte
//
// Return current byte
//
pub fn ByteBuffer_nextByte(session: &mut Tokenizer) -> u8 {
    // assert!((session.start <= session.buffer && session.buffer <= session.end));

    // if session.buffer == session.end {
    if session.offset >= session.input.len() {
        session.wasEOF = true;
        // TODO: Make this return None.
        return 0xff;
    }

    // session.buffer += 1;
    // return *(session.buffer);

    let byte = session.buffer()[0];

    session.offset += 1;

    return byte;
}

pub fn ByteBuffer_currentByte(session: &Tokenizer) -> u8 {
    // assert!((session.start <= session.buffer && session.buffer <= session.end));

    // if session.buffer == session.end {
    if session.offset >= session.input.len() {
        return 0xff;
    }

    // return *(session.buffer);

    return session.buffer()[0];
}
