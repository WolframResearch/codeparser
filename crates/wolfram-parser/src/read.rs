//! Reading Wolfram source files.

mod byte_buffer;
mod byte_decoder;
mod character_decoder;

pub(crate) mod code_point;
pub(crate) mod wl_character;


use crate::{
    issue::{Issue, Severity},
    source::{Buffer, Location, NextPolicy, SourceCharacter},
    EncodingMode, UnsafeCharacterEncoding,
};

use self::{
    byte_decoder::ByteDecoder_nextSourceCharacter,
    character_decoder::CharacterDecoder_nextWLCharacter,
    wl_character::WLCharacter,
};

//==========================================================
// Types
//==========================================================

/// Read a buffer containing Wolfram input.
#[derive(Debug)]
pub(crate) struct Reader<'i> {
    /// The complete input buffer that is being parsed.
    pub(crate) input: &'i [u8],

    /// Offset of the current byte in [`input`][Reader::input] that is next up to be parsed.
    pub(crate) offset: usize,

    pub(crate) wasEOF: bool,

    pub(crate) SrcLoc: Location,
    pub(crate) tabWidth: u32,

    pub(crate) encodingMode: EncodingMode,

    pub(crate) fatalIssues: Vec<Issue>,
    pub(crate) nonFatalIssues: Vec<Issue>,

    pub(crate) unsafe_character_encoding_flag: Option<UnsafeCharacterEncoding>,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct InputMark {
    offset: usize,
    pub src_loc: Location,

    wasEOF: Option<bool>,
}

/// A set of fields of [`Reader`] used to update the current [`Location`].
//
// TODO(cleanup): Remove this type, just make the methods of this type methods
//                on Reader.
pub(crate) struct SourceManager<'t> {
    pub(crate) loc: &'t mut Location,

    pub(crate) tab_width: u32,
}

//==========================================================
// Impls
//==========================================================

impl<'i> Reader<'i> {
    /// Return a slice of the remaining input to be parsed.
    pub(crate) fn buffer(&self) -> Buffer<'i> {
        self.buffer_at(self.offset)
    }

    pub(crate) fn buffer_at(&self, offset: usize) -> Buffer<'i> {
        if offset == self.input.len() {
            // If `offset` points exactly one byte passed the end of the
            // input, treat it as an empty buffer. We do this instead of
            // panicking because `Buffer`'s are used similarly to start/end
            // pointers in C/C++ to define input ranges
            // (see BufferAndLength::between()), so to refer to a range that
            // includes the last byte in the input, it must be possible to
            // construct a buffer that points past the last byte.
            return Buffer {
                slice: &self.input[0..0],
                offset,
            };
        } else if offset >= self.input.len() {
            panic!(
                "offset ({}) is greater than length of input ({})",
                offset,
                self.input.len()
            )
        }

        let (_, rest) = self.input.split_at(offset);

        Buffer {
            slice: rest,
            offset,
        }
    }

    //==================================
    // Read source characters
    //==================================

    /// Returns the next [`SourceCharacter`] in the input without advancing.
    #[must_use]
    pub(crate) fn peek_source_char(
        &mut self,
        policy: NextPolicy,
    ) -> SourceCharacter {
        let mark = self.mark();

        let c = self.next_source_char(policy);

        self.seek(mark);

        c
    }

    pub(crate) fn next_source_char(
        &mut self,
        policy: NextPolicy,
    ) -> SourceCharacter {
        ByteDecoder_nextSourceCharacter(self, policy)
    }

    //==================================
    // Read Wolfram characters
    //==================================

    #[must_use]
    pub(crate) fn peek_wolfram_char(
        &mut self,
        policy: NextPolicy,
    ) -> WLCharacter {
        let mark = self.mark();

        let c = self.next_wolfram_char(policy);

        self.seek(mark);

        c
    }

    pub(crate) fn next_wolfram_char(
        &mut self,
        policy: NextPolicy,
    ) -> WLCharacter {
        CharacterDecoder_nextWLCharacter(self, policy)
    }

    //==================================
    // Source location updating
    //==================================

    /// Access a set of fields related to updating the current source location.
    pub(crate) fn src(&mut self) -> SourceManager {
        SourceManager {
            tab_width: self.tabWidth,
            loc: &mut self.SrcLoc,
        }
    }

    //==================================
    // Marking and seeking
    //==================================

    /// Returns a structure representing the current position of the input
    /// reader.
    pub(crate) fn mark(&self) -> InputMark {
        InputMark {
            offset: self.offset,
            wasEOF: Some(self.wasEOF),
            src_loc: self.SrcLoc,
        }
    }

    /// Reset the current position of the input reader to the specified marked
    /// point.
    pub(crate) fn seek(&mut self, mark: InputMark) {
        let InputMark {
            offset,
            wasEOF,
            src_loc,
        } = mark;

        self.offset = offset;
        self.SrcLoc = src_loc;

        if let Some(wasEOF) = wasEOF {
            self.wasEOF = wasEOF;
        }
    }

    //==================================
    // Issue tracking
    //==================================

    pub(crate) fn setUnsafeCharacterEncodingFlag(
        &mut self,
        flag: UnsafeCharacterEncoding,
    ) {
        self.unsafe_character_encoding_flag = Some(flag);
    }

    pub(crate) fn addIssue(&mut self, issue: Issue) {
        if issue.sev == Severity::Fatal {
            //
            // There may be situations where many (1000+) fatal errors are generated.
            // This has a noticeable impact on time to transfer for something that should be instantaneous.
            //
            // If there are, say, 10 fatal errors, then assume that the 11th is not going to give any new information,
            // and ignore.
            //
            if self.fatalIssues.len() >= 10 {
                return;
            }

            if !self.fatalIssues.contains(&issue) {
                //
                // Only insert if not already found in vector
                //
                // This preserves set-like behavior while also retaining insert-order
                //
                self.fatalIssues.push(issue);
            }
        } else {
            if !self.nonFatalIssues.contains(&issue) {
                //
                // Only insert if not already found in vector
                //
                // This preserves set-like behavior while also retaining insert-order
                //
                self.nonFatalIssues.push(issue);
            }
        }
    }
}

impl InputMark {
    // TODO(cleanup): Make this function unnecessary, always use Reader::mark().
    //                Then change wasEOF back to a non-Option field.
    pub(crate) fn new(offset: usize, src_loc: Location) -> Self {
        InputMark {
            offset,
            src_loc,
            wasEOF: None,
        }
    }
}
