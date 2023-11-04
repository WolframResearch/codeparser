use std::{cell::Cell, fmt::Debug};

thread_local! {
    // TODO(cleanup): Don't store these settings using error-prone global state.
    static QUIRK_SETTINGS: Cell<QuirkSettings> =
        Cell::new(QuirkSettings::const_default());
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct QuirkSettings {
    /// "InfixBinaryAt" quirk
    ///
    ///
    /// The kernel parses `a<>StringJoin@b` as `StringJoin[a, b]`
    ///
    /// Most infix operators can be used with this syntax.
    /// Notably, SameQ and UnsameQ do NOT work with this syntax.
    ///
    /// *Related bugs: 365013*
    pub infix_binary_at: bool,

    /// "FlattenTimes" quirk
    ///
    /// In 12.1 and before:
    ///
    /// * `a / b / c` is parsed as `Times[a, Power[b, -1], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[-1, a, Power[b, -1]]`
    ///
    /// In 12.2 and after:
    ///
    /// * `a / b / c` is parsed as `Times[Times[a, Power[b, -1]], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[Times[-1, a], Power[b, -1]]`
    ///
    /// TODO: when targeting v12.2 as a minimum, remove this quirk
    ///
    /// *Related bugs: 57064, 139531, 153875, 160919*
    pub flatten_times: bool,

    /// "OldAtAtAt" quirk
    ///
    /// Changed in 13.1: `@@@`
    ///
    /// In 13.0 and before:
    ///
    /// `a @@@ b` parsed as `Apply[a, b, {1}]`
    ///
    /// In 13.1 and after:
    ///
    /// `a @@@ b` parses as `MapApply[a, b]`
    pub old_at_at_at: bool,
}

pub enum Quirk {
    /// "InfixBinaryAt" quirk
    ///
    ///
    /// The kernel parses `a<>StringJoin@b` as `StringJoin[a, b]`
    ///
    /// Most infix operators can be used with this syntax.
    /// Notably, SameQ and UnsameQ do NOT work with this syntax.
    ///
    /// *Related bugs: 365013*
    InfixBinaryAt,

    /// "FlattenTimes" quirk
    ///
    /// In 12.1 and before:
    ///
    /// * `a / b / c` is parsed as `Times[a, Power[b, -1], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[-1, a, Power[b, -1]]`
    ///
    /// In 12.2 and after:
    ///
    /// * `a / b / c` is parsed as `Times[Times[a, Power[b, -1]], Power[c, -1]]`
    /// * `-a / b` is parsed as `Times[Times[-1, a], Power[b, -1]]`
    ///
    /// TODO: when targeting v12.2 as a minimum, remove this quirk
    ///
    /// *Related bugs: 57064, 139531, 153875, 160919*
    FlattenTimes,

    /// "OldAtAtAt" quirk
    ///
    /// Changed in 13.1: `@@@`
    ///
    /// In 13.0 and before:
    ///
    /// `a @@@ b` parsed as `Apply[a, b, {1}]`
    ///
    /// In 13.1 and after:
    ///
    /// `a @@@ b` parses as `MapApply[a, b]`
    OldAtAtAt,
}

impl QuirkSettings {
    pub const fn const_default() -> Self {
        Self {
            infix_binary_at: true,
            flatten_times: false,
            old_at_at_at: false,
        }
    }

    pub fn flatten_times(self, value: bool) -> Self {
        QuirkSettings {
            flatten_times: value,
            ..self
        }
    }

    pub fn infix_binary_at(self, value: bool) -> Self {
        QuirkSettings {
            infix_binary_at: value,
            ..self
        }
    }

    pub fn old_at_at_at(self, value: bool) -> Self {
        QuirkSettings {
            old_at_at_at: value,
            ..self
        }
    }
}

impl Default for QuirkSettings {
    fn default() -> Self {
        Self::const_default()
    }
}

pub fn set_quirks(quirks: QuirkSettings) {
    QUIRK_SETTINGS.set(quirks);
}

pub(crate) fn is_quirk_enabled(quirk: Quirk) -> bool {
    let settings = QUIRK_SETTINGS.get();

    match quirk {
        Quirk::InfixBinaryAt => settings.infix_binary_at,
        Quirk::FlattenTimes => settings.flatten_times,
        Quirk::OldAtAtAt => settings.old_at_at_at,
    }
}
