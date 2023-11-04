//! Constants that are true if the associated cargo feature is enabled.
//!
//! The constants in this module are intended to be used as:
//!
//! ```ignore
//! if feature::CHECK_ABORT {
//!     // ...
//! }
//! ```
//!
//! Using these constants is preferred over the standard alternatives of:
//!
//! ```ignore
//! #[cfg(feature = "CHECK_ABORT")]
//! // ...
//! ```
//!
//! or:
//!
//! ```ignore
//! if cfg!(feature = "CHECK_ABORT") {
//!     // ...
//! }
//! ```
//!
//! which have the disadvantage that the `feature = "..."` is not validated to
//! correspond to a feature that is declared in Cargo.toml.
//!
//! More generally, using a constant instead of a parse-time `#[cfg(..)]` to
//! disable sections of code has the advantage that the code inside the
//! condition is still validated and type checked, which doesn't happen if
//! `#[cfg(..)]` is used.
//!
//! This makes code controlled by feature flags easier to keep up-to-date as
//! refactoring occurs.

pub(crate) const COMPUTE_SOURCE: bool = cfg!(feature = "COMPUTE_SOURCE");

pub(crate) const CHECK_ABORT: bool = cfg!(feature = "CHECK_ABORT");

pub(crate) const FAST_STRING_SCAN: bool = cfg!(feature = "FAST_STRING_SCAN");
