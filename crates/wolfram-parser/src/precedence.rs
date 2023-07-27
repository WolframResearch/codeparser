use std::num::NonZeroU8;

/// All levels of precedence
///
/// The 1's bit denotes the associativity.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Precedence(NonZeroU8);

// Verify that Option<Precedence> is the same size as a u8.
const _: () = assert!(std::mem::size_of::<Option<Precedence>>() == 1);


impl Precedence {
    // TODO(cleanup): Take the precedence value and associativity as separate
    // arguments.
    pub(crate) const fn new(value: u8) -> Self {
        match NonZeroU8::new(value) {
            Some(value) => Precedence(value),
            None => panic!("invalid Precedence 0 value"),
        }
    }

    // TODO(cleanup): Make this unnecessary. What does it mean anyway?
    fn bits(self) -> u8 {
        let Precedence(bits) = self;

        bits.get()
    }

    /// Returns true if `lhs` is greater then `rhs`.
    pub(crate) fn greater(
        lhs: Option<Precedence>,
        rhs: Option<Precedence>,
    ) -> bool {
        let lhs = lhs.map(Precedence::bits).unwrap_or(0);
        let rhs = rhs.map(Precedence::bits).unwrap_or(0);

        lhs | 0x1 > rhs
    }
}

impl PartialEq<Precedence> for Option<Precedence> {
    fn eq(&self, other: &Precedence) -> bool {
        match self {
            Some(self_) => self_ == other,
            None => false,
        }
    }
}
