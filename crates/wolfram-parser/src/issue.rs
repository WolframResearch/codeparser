use std::cmp::Ordering;

use crate::{
    source::{Source, Span},
    symbol::Symbol,
    symbols as sym,
};


type AdditionalDescriptionVector = Vec<String>;

#[derive(Debug, Clone)]
pub struct Issue {
    pub make_sym: Symbol,
    pub tag: IssueTag,
    pub msg: String,
    pub sev: Severity,
    pub src: Source,
    pub val: f64,
    pub actions: Vec<CodeAction>,
    pub additional_descriptions: AdditionalDescriptionVector,
    pub additional_sources: Vec<Source>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IssueTag {
    Ambiguous,
    UnhandledCharacter,
    UnexpectedCharacter,
    UnexpectedCarriageReturn,
    UnexpectedSpaceCharacter,
    UnexpectedNewlineCharacter,
    UnexpectedDot,
    UnexpectedSign,
    UnexpectedImplicitTimes,
    UnexpectedLetterlikeCharacter,
    UnrecognizedLongName,
    UndocumentedSlotSyntax,
    NonASCIICharacter,
    IncompleteUTF8Sequence,
    StraySurrogate,
    BOM,
    //
    // Syntax Issues
    //
    SyntaxUndocumentedMessageName,
    PrefixNotNot,
    StrangeCall,
    StrangeCallSlotSequence,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Severity {
    Formatting,
    Remark,
    Warning,
    Error,
    Fatal,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct CodeAction {
    pub label: String,
    pub kind: CodeActionKind,
    pub src: Span,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum CodeActionKind {
    ReplaceText { replacement_text: String },
    InsertText { insertion_text: String },
    DeleteText,
}


//=======================================
// Impls
//=======================================

impl IssueTag {
    pub fn as_str(&self) -> &'static str {
        match self {
            IssueTag::Ambiguous => "Ambiguous",
            IssueTag::UnhandledCharacter => "UnhandledCharacter",
            IssueTag::UnexpectedCharacter => "UnexpectedCharacter",
            IssueTag::UnexpectedCarriageReturn => "UnexpectedCarriageReturn",
            IssueTag::UnexpectedSpaceCharacter => "UnexpectedSpaceCharacter",
            IssueTag::UnexpectedNewlineCharacter => {
                "UnexpectedNewlineCharacter"
            },
            IssueTag::UnexpectedDot => "UnexpectedDot",
            IssueTag::UnexpectedSign => "UnexpectedSign",
            IssueTag::UnexpectedImplicitTimes => "UnexpectedImplicitTimes",
            IssueTag::UnexpectedLetterlikeCharacter => {
                "UnexpectedLetterlikeCharacter"
            },
            IssueTag::UnrecognizedLongName => "UnrecognizedLongName",
            IssueTag::UndocumentedSlotSyntax => "UndocumentedSlotSyntax",
            IssueTag::NonASCIICharacter => "NonASCIICharacter",
            IssueTag::IncompleteUTF8Sequence => "IncompleteUTF8Sequence",
            IssueTag::StraySurrogate => "StraySurrogate",
            IssueTag::BOM => "BOM",
            IssueTag::SyntaxUndocumentedMessageName => {
                "SyntaxUndocumentedMessageName"
            },
            IssueTag::PrefixNotNot => "PrefixNotNot",
            IssueTag::StrangeCall => "StrangeCall",
            IssueTag::StrangeCallSlotSequence => "StrangeCallSlotSequence",
            // NOTE: When adding a case here, also update from_str().
        }
    }

    #[doc(hidden)]
    pub fn from_str(string: &str) -> Option<Self> {
        let value = match string {
            "Ambiguous" => IssueTag::Ambiguous,
            "UnhandledCharacter" => IssueTag::UnhandledCharacter,
            "UnexpectedCharacter" => IssueTag::UnexpectedCharacter,
            "UnexpectedCarriageReturn" => IssueTag::UnexpectedCarriageReturn,
            "UnexpectedSpaceCharacter" => IssueTag::UnexpectedSpaceCharacter,
            "UnexpectedNewlineCharacter" => {
                IssueTag::UnexpectedNewlineCharacter
            },
            "UnexpectedDot" => IssueTag::UnexpectedDot,
            "UnexpectedSign" => IssueTag::UnexpectedSign,
            "UnexpectedImplicitTimes" => IssueTag::UnexpectedImplicitTimes,
            "UnexpectedLetterlikeCharacter" => {
                IssueTag::UnexpectedLetterlikeCharacter
            },
            "UnrecognizedLongName" => IssueTag::UnrecognizedLongName,
            "UndocumentedSlotSyntax" => IssueTag::UndocumentedSlotSyntax,
            "NonASCIICharacter" => IssueTag::NonASCIICharacter,
            "IncompleteUTF8Sequence" => IssueTag::IncompleteUTF8Sequence,
            "StraySurrogate" => IssueTag::StraySurrogate,
            "BOM" => IssueTag::BOM,
            "SyntaxUndocumentedMessageName" => {
                IssueTag::SyntaxUndocumentedMessageName
            },
            "PrefixNotNot" => IssueTag::PrefixNotNot,
            "StrangeCall" => IssueTag::StrangeCall,
            "StrangeCallSlotSequence" => IssueTag::StrangeCallSlotSequence,
            _ => return None,
        };

        Some(value)
    }
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Severity::Formatting => "Formatting",
            Severity::Remark => "Remark",
            Severity::Warning => "Warning",
            Severity::Error => "Error",
            Severity::Fatal => "Fatal",
            // NOTE: When adding a case here, also update from_str().
        }
    }

    #[doc(hidden)]
    pub fn from_str(string: &str) -> Option<Self> {
        let value = match string {
            "Formatting" => Severity::Formatting,
            "Remark" => Severity::Remark,
            "Warning" => Severity::Warning,
            "Error" => Severity::Error,
            "Fatal" => Severity::Fatal,
            _ => return None,
        };

        Some(value)
    }
}


//==========================================================
// Code Actions
//==========================================================


impl CodeAction {
    pub fn replace_text(
        label: String,
        src: Span,
        replacement_text: String,
    ) -> Self {
        CodeAction {
            label,
            src,
            kind: CodeActionKind::ReplaceText { replacement_text },
        }
    }

    pub fn insert_text(
        label: String,
        src: Span,
        insertion_text: String,
    ) -> Self {
        CodeAction {
            label,
            src,
            kind: CodeActionKind::InsertText { insertion_text },
        }
    }

    pub fn delete_text(label: String, src: Span) -> Self {
        CodeAction {
            label,
            src,
            kind: CodeActionKind::DeleteText,
        }
    }
}

// bool IssuePtrCompare::operator()(const IssuePtr& L, const IssuePtr& R) const {

//     if (L->Src < R->Src) {
//         return true;
//     }

//     if (L->Tag < R->Tag) {
//         return true;
//     }

//     return false;
// }

impl PartialOrd for Issue {
    // TODO: Is this implementation of sorting for Issue's right?
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.src < other.src {
            return Some(Ordering::Less);
        }

        // Sort in reverse order by ID. Why? Not sure, but empirically it
        // matches the C++ version.
        // TODO: What is the reason for doing this? This seems to result in
        //       otherwise equal issues being sorted in reverse alphabetical
        //       order by issue name.
        Some(self.tag.cmp(&other.tag).reverse())
    }
}

impl Ord for Issue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl PartialEq for Issue {
    fn eq(&self, other: &Self) -> bool {
        self.make_sym == other.make_sym
            && self.tag == other.tag
            && self.msg == other.msg
            && self.sev == other.sev
            && self.src == other.src
            && self.val == other.val
            && self.actions == other.actions
            && self.additional_descriptions == other.additional_descriptions
    }
}

impl Eq for Issue {}

impl Issue {
    pub fn new(
        make_sym: Symbol,
        tag: IssueTag,
        msg: String,
        sev: Severity,
        src: Span,
        val: std::os::raw::c_double,
        actions: Vec<CodeAction>,
        additional_descriptions: AdditionalDescriptionVector,
    ) -> Issue {
        Issue {
            make_sym,
            tag,
            msg,
            sev,
            src: Source::Span(src),
            val,
            actions,
            additional_descriptions,
            additional_sources: Vec::new(),
        }
    }

    pub(crate) fn syntax(
        tag: IssueTag,
        msg: String,
        sev: Severity,
        src: Source,
        val: f64,
    ) -> Self {
        Issue {
            make_sym: sym::CodeParser_SyntaxIssue,
            tag,
            msg,
            sev,
            src,
            val,
            actions: Vec::new(),
            additional_descriptions: Vec::new(),
            additional_sources: Vec::new(),
        }
    }

    pub fn with_additional_sources(
        self,
        additional_sources: Vec<Source>,
    ) -> Self {
        debug_assert!(self.additional_sources.is_empty());

        Issue {
            additional_sources,
            ..self
        }
    }

    pub fn with_additional_descriptions(
        self,
        additional_descriptions: Vec<String>,
    ) -> Self {
        debug_assert!(self.additional_descriptions.is_empty());

        Issue {
            additional_descriptions,
            ..self
        }
    }

    // TODO: Display
    // fn print(std::ostream& s) const {

    //     MakeSym.print(s);
    //     s << "[";

    //     Tag.print(s);
    //     s << ", ";

    //     s << Msg;
    //     s << ", ";

    //     Sev.print(s);
    //     s << ", ";

    //     Src.print(s);
    //     s << ", ";

    //     s << Val;
    //     s << ", ";

    //     for (auto& A : Actions) {
    //         A->print(s);
    //         s << ", ";
    //     }

    //     for (auto& D : AdditionalDescriptions) {
    //         s << D.c_str();
    //         s << ", ";
    //     }

    //     s << "]";
    // }
}

pub(crate) fn SyntaxIssue(
    tag: IssueTag,
    msg: String,
    sev: Severity,
    src: Span,
    val: std::os::raw::c_double,
    actions: Vec<CodeAction>,
    additional_descriptions: AdditionalDescriptionVector,
) -> Issue {
    Issue::new(
        sym::CodeParser_SyntaxIssue,
        tag,
        msg,
        sev,
        src,
        val,
        actions,
        additional_descriptions,
    )
}

pub(crate) fn FormatIssue(
    tag: IssueTag,
    msg: String,
    sev: Severity,
    src: Span,
    val: std::os::raw::c_double,
    actions: Vec<CodeAction>,
    additional_descriptions: AdditionalDescriptionVector,
) -> Issue {
    Issue::new(
        sym::CodeParser_FormatIssue,
        tag,
        msg,
        sev,
        src,
        val,
        actions,
        additional_descriptions,
    )
}

pub(crate) fn EncodingIssue(
    tag: IssueTag,
    msg: String,
    sev: Severity,
    src: Span,
    val: std::os::raw::c_double,
    actions: Vec<CodeAction>,
    additional_descriptions: AdditionalDescriptionVector,
) -> Issue {
    Issue::new(
        sym::CodeParser_EncodingIssue,
        tag,
        msg,
        sev,
        src,
        val,
        actions,
        additional_descriptions,
    )
}

// TODO: Display
// void ReplaceTextCodeAction::print(std::ostream& s) const {

//     SYMBOL_CODEPARSER_CODEACTION.print(s);
//     s << "[";

//     s << Label;
//     s << ", ";

//     SYMBOL_CODEPARSER_REPLACETEXT.print(s);
//     s << ", ";

//     Src.print(s);
//     s << ", ";

//     s << ReplacementText;

//     s << "]";
// }

// void InsertTextCodeAction::print(std::ostream& s) const {

//     SYMBOL_CODEPARSER_CODEACTION.print(s);
//     s << "[";

//     s << Label;
//     s << ", ";

//     SYMBOL_CODEPARSER_INSERTTEXT.print(s);
//     s << ", ";

//     Src.print(s);
//     s << ", ";

//     s << InsertionText;

//     s << "]";
// }

// void DeleteTextCodeAction::print(std::ostream& s) const {

//     SYMBOL_CODEPARSER_CODEACTION.print(s);
//     s << "[";

//     s << Label;
//     s << ", ";

//     SYMBOL_CODEPARSER_DELETETEXT.print(s);
//     s << ", ";

//     Src.print(s);

//     s << "]";
// }
