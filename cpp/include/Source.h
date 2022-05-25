
#pragma once

#include "TokenEnum.h" // for TokenEnum
#include "CodePoint.h" // for codepoint

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <set>
#include <string>
#include <cassert>
#include <iterator>
#include <array>
#include <memory> // for unique_ptr
#include <vector>
#include <cstddef> // for size_t

class Issue;
class CodeAction;

class IssuePtrCompare;

using Buffer = const unsigned char *;
using MBuffer = unsigned char *;
using IssuePtr = std::shared_ptr<Issue>;
using CodeActionPtr = std::unique_ptr<CodeAction>;

using IssuePtrSet = std::set<IssuePtr, IssuePtrCompare>;
using CodeActionPtrVector = std::vector<CodeActionPtr>;
using AdditionalDescriptionVector = std::vector<std::string>;

//
//
//
struct BufferAndLength {
    
    Buffer buffer;
    Buffer end;
    
    BufferAndLength();
    BufferAndLength(Buffer buffer, size_t length = 0);
    
    size_t length() const;
    
    void printUTF8String(std::ostream& s) const;
    
#if USE_MATHLINK
    void putUTF8String(MLINK mlp) const;
#endif // USE_MATHLINK
};

static_assert((SIZEOF_VOID_P == 8 && sizeof(BufferAndLength) == 16) || (SIZEOF_VOID_P == 4), "Check your assumptions");

bool operator==(BufferAndLength a, BufferAndLength b);
bool operator!=(BufferAndLength a, BufferAndLength b);

//
//
//
enum NextPolicyBits : uint8_t {
    
//     UNUSED = 0x01,
    
    //
    // Enable character decoding issues
    //
    // "\c" gives a CharacterDecoding error (issues are ENABLED)
    //
    // (*\c*) does NOT give a CharacterDecoding error (issues are DISABLED)
    //
    // This is also used when peeking: no need to report issues while peeking
    //
    // Used By ByteDecoder, CharacterDecoder
    //
    ENABLE_CHARACTER_DECODING_ISSUES = 0x02,
    
    //
    // Needs to be 0b100, for easy or-ing of TOKEN_INTERNALNEWLINE to TOKEN_TOPLEVELNEWLINE
    //
    // Used by Tokenizer
    //
    RETURN_TOPLEVELNEWLINE = 0x04,
    
    //
    // This bit serves 2 purposes:
    // Complex line continuations
    // Decrease severity of unexpected characters
    //
    // These are exactly what we care about with strings and comments, so use only a single bit for both purposes.
    //
    // NOTE: If the set:
    // {strings, comments}
    // is ever not exactly the same as the set:
    // (things that care about complex line continuations and decreased severity of unexpected characters)
    // then we need to rethink these bits.
    //
    // Complex line continuations:
    // Line continuations inside of strings or comments are "complex":
    // Formatting matters
    //
    // All other line continuations are simple:
    // inside or outside of other tokens
    // outside of strings or comments
    //
    //
    // Decrease severity of unexpected characters:
    // Outside of strings, \[RightArrow] should be a warning
    // Inside of strings, \[RightArrow] should be a remark
    //
    STRING_OR_COMMENT = 0x08,
    
    //
    // Check for unlikely escape sequences?
    //
    // Check for sequences like \\[Alpa] and report them
    //
    // Used by CharacterDecoder
    //
    ENABLE_UNLIKELY_ESCAPE_CHECKING = 0x10,
    
    //
    // If inside #, then give syntax warnings for #"123" and #a`b syntax (which is undocumented syntax)
    //
    // But obviously "123" and a`b are fine outside of #
    //
    // Also return symbols as strings, e.g., the  abc  in  #abc  is a string
    //
    // Used by Tokenizer
    //
    SLOT_BEHAVIOR_FOR_STRINGS = 0x20,
    
    //
    // When tokenizing numbers, return immediately when an integer has been tokenized
    //
    // This is used when parsing Slot, SlotSequence, and Out
    //
    // For example, we must consider  #2.c  to be Slot[2] . c  and NOT  Slot[1] 2. c
    //
    INTEGER_SHORT_CIRCUIT = 0x40,
    
//    UNUSED = 0x80,
};

using NextPolicy = uint8_t;

const NextPolicy TOPLEVEL = ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE;

const NextPolicy INSIDE_SYMBOL = ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE;

const NextPolicy INSIDE_STRINGIFY_AS_TAG = ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE;
const NextPolicy INSIDE_STRINGIFY_AS_FILE = RETURN_TOPLEVELNEWLINE;

const NextPolicy INSIDE_SLOT = RETURN_TOPLEVELNEWLINE | SLOT_BEHAVIOR_FOR_STRINGS | INTEGER_SHORT_CIRCUIT;

const NextPolicy INSIDE_SLOTSEQUENCE = RETURN_TOPLEVELNEWLINE | INTEGER_SHORT_CIRCUIT;

const NextPolicy INSIDE_OUT = RETURN_TOPLEVELNEWLINE | INTEGER_SHORT_CIRCUIT;

//
//
//
enum SyntaxError : uint8_t {
    
    SYNTAXERROR_UNKNOWN,
    
    //
    // Something like  a ~b
    //
    SYNTAXERROR_EXPECTEDTILDE,
    
    //
    // Something like  a /: b
    //
    SYNTAXERROR_EXPECTEDSET,
};

std::string SyntaxErrorToString(SyntaxError Err);


typedef const std::string SyntaxIssueTag;

//
//
//
SyntaxIssueTag SYNTAXISSUETAG_UNHANDLEDCHARACTER = "UnhandledCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNSUPPORTEDCHARACTER = "UnsupportedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNDOCUMENTEDCHARACTER = "UndocumentedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDESCAPESEQUENCE = "UnexpectedEscapeSequence";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDCHARACTER = "UnexpectedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDNEWLINECHARACTER = "UnexpectedNewlineCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDSPACECHARACTER = "UnexpectedSpaceCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDLETTERLIKECHARACTER = "UnexpectedLetterlikeCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNDOCUMENTEDSLOTSYNTAX = "UndocumentedSlotSyntax";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDIMPLICITTIMES = "UnexpectedImplicitTimes";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDDOT = "UnexpectedDot";
SyntaxIssueTag SYNTAXISSUETAG_COMMA = "Comma";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDSIGN = "UnexpectedSign";

typedef const std::string FormatIssueTag;

//
// xxx
//
FormatIssueTag FORMATISSUETAG_INSERTSPACE = "InsertSpace";


typedef const std::string EncodingIssueTag;

EncodingIssueTag ENCODINGISSUETAG_INCOMPLETEUTF8SEQUENCE = "IncompleteUTF8Sequence";
EncodingIssueTag ENCODINGISSUETAG_STRAYSURROGATE = "StraySurrogate";
EncodingIssueTag ENCODINGISSUETAG_BOM = "BOM";
EncodingIssueTag ENCODINGISSUETAG_UNEXPECTEDCARRIAGERETURN = "UnexpectedCarriageReturn";
EncodingIssueTag ENCODINGISSUETAG_UNEXPECTEDCHARACTER = "UnexpectedCharacter";
EncodingIssueTag ENCODINGISSUETAG_NONASCIICHARACTER = "NonASCIICharacter";



//
// Used to be just SEVERITY_ERROR, etc.,
// but this was observed:
// c:\users\brenton\dropbox\wolfram\ast\ast\cpp\include\SyntaxIssue.h(19): warning C4005: 'SEVERITY_ERROR': macro redefinition
// C:\Program Files (x86)\Windows Kits\10\include\10.0.17763.0\shared\winerror.h(28563): note: see previous definition of 'SEVERITY_ERROR'
//
typedef const std::string SyntaxIssueSeverity;

//
// for example: unexpected escape sequence
//
SyntaxIssueSeverity SYNTAXISSUESEVERITY_REMARK = "Remark";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_WARNING = "Warning";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_ERROR = "Error";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_FATAL = "Fatal";

typedef const std::string FormatIssueSeverity;

FormatIssueSeverity FORMATISSUESEVERITY_FORMATTING = "Formatting";

typedef const std::string EncodingIssueSeverity;

//
// for example: non-ASCII character
//
EncodingIssueSeverity ENCODINGISSUESEVERITY_REMARK = "Remark";
EncodingIssueSeverity ENCODINGISSUESEVERITY_WARNING = "Warning";
//EncodingIssueSeverity ENCODINGISSUESEVERITY_ERROR = "Error";
EncodingIssueSeverity ENCODINGISSUESEVERITY_FATAL = "Fatal";


//
// A single character of source code
//
// The text  \[Alpha]  would be 8 separate SourceCharacters
//
struct SourceCharacter {
    
    codepoint val;
    
    explicit constexpr SourceCharacter(codepoint val) : val(val) {}
    
    explicit operator int() const noexcept = delete;
    
    constexpr bool operator==(const SourceCharacter& o) const {
        return val == o.val;
    }
    
    constexpr bool operator!=(const SourceCharacter& o) const {
        return val != o.val;
    }
    
    constexpr codepoint to_point() const {
        return val;
    }
    
    std::string graphicalString() const;
    
    std::string safeAndGraphicalString() const;
    
    bool isAlphaOrDigit() const;
    
    bool isHex() const;
    
    bool isOctal() const;
    
    bool isUpper() const;
    
    bool isEndOfFile() const;
    
    bool isDigit() const;
    
    bool isWhitespace() const;
    
    bool isNewline() const;
    
    bool isMBWhitespace() const;
    bool isMBNewline() const;
    
    
    class SourceCharacter_iterator {
        
    public:
        size_t size;
        size_t idx;
        codepoint val;
        std::array<unsigned char, 4> arr;
        
        SourceCharacter_iterator(codepoint val);
        
        unsigned char operator*() {
            return arr[idx];
        }
        
        bool operator!=(const SourceCharacter_iterator& other) {
            return val != other.val || idx != other.idx;
        }
        
        SourceCharacter_iterator& operator++() {
            assert(idx < size);
            ++idx;
            return *this;
        }
    };
    
    SourceCharacter_iterator begin();
    
    SourceCharacter_iterator end();
};

static_assert(sizeof(SourceCharacter) == 4, "Check your assumptions");

std::ostream& operator<<(std::ostream& stream, const SourceCharacter);

//
//
//
enum SourceConvention {
    SOURCECONVENTION_UNKNOWN,
    SOURCECONVENTION_LINECOLUMN,
    SOURCECONVENTION_SOURCECHARACTERINDEX
};

constexpr uint32_t DEFAULT_TAB_WIDTH = 4;


//
//
//
struct SourceLocation {
    
    //
    // if source convention is LineColumn, this is Line
    // if source convention is SourceCharacterIndex, this is unused (always 0)
    //
    uint32_t first;
    
    //
    // if source convention is LineColumn, this is Column
    // if source convention is SourceCharacterIndex, this is index
    //
    uint32_t second;
    
    SourceLocation();
    
    SourceLocation(uint32_t first, uint32_t second);
    
    SourceLocation next();
    SourceLocation previous();
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
    
    void putStructured(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
};

static_assert(sizeof(SourceLocation) == 8, "Check your assumptions");

//
// For LineContinuations and EmbeddedNewlines
//
bool operator<(SourceLocation a, SourceLocation b);

//
// For googletest
//
void PrintTo(const SourceLocation& Loc, std::ostream *s);

//
//
//
struct Source {
    
    SourceLocation Start;
    SourceLocation End;
    
    Source();
    
    Source(SourceLocation only);
    
    Source(size_t coerced) = delete;
    
    Source(SourceLocation start, SourceLocation end);
    
    Source(Source start, Source end);

#if USE_MATHLINK
    void put(MLINK mlp) const;
    
    void putStructured(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    size_t size() const;
};

static_assert(sizeof(Source) == 16, "Check your assumptions");

bool operator==(Source a, Source b);
bool operator!=(Source a, Source b);

bool operator<(Source a, Source b);

//
// For googletest
//
void PrintTo(const Source& Src, std::ostream *s);


//
// For std::set
//
class IssuePtrCompare {
public:
    bool operator() (const IssuePtr& L, const IssuePtr& R) const;
};

//
//
//
class Issue {
public:

    const SyntaxIssueTag Tag;
    const std::string Msg;
    const SyntaxIssueSeverity Sev;
    const Source Src;
    const double Val;
    const CodeActionPtrVector Actions;
    const AdditionalDescriptionVector AdditionalDescriptions;
    
    Issue(std::string Tag, std::string Msg, std::string Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
    
    Source getSource() const;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
#endif // USE_MATHLINK
    
    virtual void print(std::ostream& s) const = 0;
    
    virtual bool check() const = 0;
    
    virtual ~Issue() {}
};

//
//
//
class CodeAction {
protected:
    const std::string Label;
    Source Src;
    
public:
    CodeAction(std::string Label, Source Src);

    Source getSource() const;
    
    const std::string getLabel() const;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
#endif // USE_MATHLINK
    
    virtual void print(std::ostream& s) const = 0;
    
    virtual ~CodeAction() {}
};


//
//
//
class ReplaceTextCodeAction : public CodeAction {
    std::string ReplacementText;
public:
    
    ReplaceTextCodeAction(std::string Label, Source Src, std::string ReplacementText) : CodeAction(Label, Src), ReplacementText(ReplacementText) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

//
//
//
class InsertTextCodeAction : public CodeAction {
    std::string InsertionText;
public:
    
    InsertTextCodeAction(std::string Label, Source Src, std::string InsertionText) : CodeAction(Label, Src), InsertionText(InsertionText) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

//
//
//
class DeleteTextCodeAction : public CodeAction {
public:
    
    DeleteTextCodeAction(std::string Label, Source Src) : CodeAction(Label, Src) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

//
//
//
class SyntaxIssue : public Issue {
public:
    SyntaxIssue(SyntaxIssueTag Tag, std::string Msg, SyntaxIssueSeverity Sev, Source Src, double Con, CodeActionPtrVector Actions = {}, AdditionalDescriptionVector AdditionalDescriptions = {}) : Issue(Tag, Msg, Sev, Src, Con, std::move(Actions), AdditionalDescriptions) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK

    void print(std::ostream& s) const override;
    
    bool check() const override;
};

//
//
//
class FormatIssue : public Issue {
public:
    FormatIssue(FormatIssueTag Tag, std::string Msg, FormatIssueSeverity Sev, Source Src, CodeActionPtrVector Actions = {}, AdditionalDescriptionVector AdditionalDescriptions = {}) : Issue(Tag, Msg, Sev, Src, 0.0, std::move(Actions), AdditionalDescriptions) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override;
};

//
//
//
class EncodingIssue : public Issue {
public:
    EncodingIssue(EncodingIssueTag Tag, std::string Msg, EncodingIssueSeverity Sev, Source Src, double Con, CodeActionPtrVector Actions = {}, AdditionalDescriptionVector AdditionalDescriptions = {}) : Issue(Tag, Msg, Sev, Src, Con, std::move(Actions), AdditionalDescriptions) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
    bool check() const override;
};
