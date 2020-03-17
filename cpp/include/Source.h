
#pragma once

#include "TokenEnum.h" // for TokenEnum
#include "CodePoint.h" // for codepoint

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <string>
#include <cassert>
#include <iterator>
#include <array>
#include <vector>
#include <memory> // for unique_ptr

class Issue;
class CodeAction;

using Buffer = const unsigned char *;
using MBuffer = unsigned char *;
using IssuePtr = std::unique_ptr<Issue>;
using CodeActionPtr = std::unique_ptr<CodeAction>;


enum UTF8Status : uint8_t {
    
    UTF8STATUS_NORMAL,
    
    //
    // Could be:
    // invalid first, second, third, or fourth byte
    // Hit EOF in the middle of a sequence
    // Surrogate
    //
    // It doesn't really matter what the problem is.
    //
    // Anything that is invalid gets turned into \[UnknownGlyph]
    //
    UTF8STATUS_INVALID,
    
    //
    // Non-characters and BOM is preserved
    //
    UTF8STATUS_NONCHARACTER_OR_BOM
};

struct BufferAndLength {
    
    Buffer buffer;
    Buffer end;
    UTF8Status status;
    
    BufferAndLength();
    BufferAndLength(Buffer buffer, size_t length = 0, UTF8Status status = UTF8STATUS_NORMAL);
    
    size_t length() const;
    
    void printUTF8String(std::ostream& s) const;
    
#if USE_MATHLINK
    void putUTF8String(MLINK ) const;
#endif // USE_MATHLINK
    
    BufferAndLength createNiceBufferAndLength(std::string *str) const;
};

static_assert((SIZEOF_VOID_P == 8 && sizeof(BufferAndLength) == 24) || (SIZEOF_VOID_P == 4), "Check your assumptions");

bool operator==(BufferAndLength a, BufferAndLength b);



enum NextCharacterPolicyBits : uint8_t {
    
    //
    // Preserve whitespace after line continuation
    //
    // ToExpression["0.\\\n  6"] evaluates to 0.6 (whitespace is NOT preserved)
    //
    // But ToExpression["\"0.\\\n  6\""] evaluates to "0.  6" (whitespace IS preserved)
    //
    // FIXME: this could be handled by line continuation processing
    //
    PRESERVE_WS_AFTER_LC = 0x01,
    
    //
    // Enable character decoding issues
    //
    // "\c" gives a CharacterDecoding error (issues are ENABLED)
    //
    // (*\c*) does NOT give a CharacterDecoding error (issues are DISABLED)
    //
    // This is also used when peeking: no need to report issues while peeking
    //
    ENABLE_CHARACTER_DECODING_ISSUES = 0x02,
    
    //
    // Needs to be 0b100, for easy or-ing of TOKEN_INTERNALNEWLINE to TOKEN_TOPLEVELNEWLINE
    //
    RETURN_TOPLEVELNEWLINE = 0x04,
    
    //
    // This code:
    // { a, \
    //   b }
    //
    // would give a line continuation warning (line continuation is NOT meaningful)
    //
    // This code:
    // { a, "x\
    //   y", b }
    //
    // would NOT give a warning (line continuation IS meaningful)
    //
    LC_IS_MEANINGFUL = 0x08,
    
    //
    // Check for unlikely escape sequences?
    //
    // Check for sequences like \\[Alpa] and report them
    //
    ENABLE_UNLIKELY_ESCAPE_CHECKING = 0x10,
};

using NextCharacterPolicy = uint8_t;

const NextCharacterPolicy TOPLEVEL = ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE;

const NextCharacterPolicy INSIDE_SYMBOL = ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE | LC_IS_MEANINGFUL;

#if STARTOFLINE
const NextCharacterPolicy INSIDE_STRINGIFY_LINE = ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE | ENABLE_STRANGE_CHARACTER_CHECKING;
#endif // STARTOFLINE
const NextCharacterPolicy INSIDE_STRINGIFY_SYMBOL = PRESERVE_WS_AFTER_LC | ENABLE_CHARACTER_DECODING_ISSUES | RETURN_TOPLEVELNEWLINE;
const NextCharacterPolicy INSIDE_STRINGIFY_FILE = RETURN_TOPLEVELNEWLINE;


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
    
    //
    // Something like:
    // { + }
    // where the prefix operator  +  does not have an operand
    //
    SYNTAXERROR_EXPECTEDOPERAND,
    
    SYNTAXERROR_EXPECTEDINTEGRAND,
    
    SYNTAXERROR_UNEXPECTEDCLOSER,
    
    //
    // These are translated over from Token Errors
    //
    SYNTAXERROR_TOKEN_EXPECTEDEQUAL,
    SYNTAXERROR_TOKEN_UNHANDLEDCHARACTER,
    SYNTAXERROR_TOKEN_EXPECTEDDIGITORALPHA,
    SYNTAXERROR_TOKEN_EXPECTEDLETTERLIKE,
    SYNTAXERROR_TOKEN_UNTERMINATEDCOMMENT,
    SYNTAXERROR_TOKEN_UNTERMINATEDSTRING,
    SYNTAXERROR_TOKEN_INVALIDBASE,
    SYNTAXERROR_TOKEN_EXPECTEDACCURACY,
    SYNTAXERROR_TOKEN_EXPECTEDEXPONENT,
    SYNTAXERROR_TOKEN_EMPTYSTRING,
    SYNTAXERROR_TOKEN_UNHANDLEDDOT,
    SYNTAXERROR_TOKEN_UNRECOGNIZEDDIGIT,
};

std::string SyntaxErrorToString(SyntaxError Err);

SyntaxError TokenErrorToSyntaxError(TokenEnum);



typedef const std::string SyntaxIssueTag;

SyntaxIssueTag SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER = "UnrecognizedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNSUPPORTEDCHARACTER = "UnsupportedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNDOCUMENTEDCHARACTER = "UndocumentedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDESCAPESEQUENCE = "UnexpectedEscapeSequence";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDCHARACTER = "UnexpectedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNDOCUMENTEDSLOTSYNTAX = "UndocumentedSlotSyntax";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDIMPLICITTIMES = "UnexpectedImplicitTimes";

typedef const std::string FormatIssueTag;

//
// When the FormatIssue is made, details for SyntaxAmbiguitySpace will be filled in
//
// SyntaxAmbiguitySpace is: insert space between characters
//
FormatIssueTag FORMATISSUETAG_SPACE = "Space";
FormatIssueTag FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN = "UnexpectedCarriageReturn";
FormatIssueTag FORMATISSUETAG_UNEXPECTEDLINECONTINUATION = "UnexpectedLineContinuation";


typedef const std::string EncodingIssueTag;

EncodingIssueTag ENCODINGISSUETAG_INVALIDCHARACTERENCODING = "InvalidCharacterEncoding";



//
// Used to be just SEVERITY_ERROR, etc.,
// but this was observed:
// c:\users\brenton\dropbox\wolfram\ast\ast\cpp\include\SyntaxIssue.h(19): warning C4005: 'SEVERITY_ERROR': macro redefinition
// C:\Program Files (x86)\Windows Kits\10\include\10.0.17763.0\shared\winerror.h(28563): note: see previous definition of 'SEVERITY_ERROR'

typedef const std::string SyntaxIssueSeverity;

SyntaxIssueSeverity SYNTAXISSUESEVERITY_REMARK = "Remark";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_WARNING = "Warning";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_ERROR = "Error";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_FATAL = "Fatal";

typedef const std::string FormatIssueSeverity;

FormatIssueSeverity FORMATISSUESEVERITY_FORMATTING = "Formatting";

//
// A single character of source code
//
// The text  \[Alpha]  would be 8 separate SourceCharacters
//
struct SourceCharacter {
    
    codepoint val;
    
    explicit constexpr SourceCharacter(codepoint val) : val(val) {}
    
    explicit operator int() const noexcept = delete;
    
    constexpr bool operator==(const SourceCharacter &o) const {
        return val == o.val;
    }
    
    constexpr bool operator!=(const SourceCharacter &o) const {
        return val != o.val;
    }
    
    constexpr codepoint to_point() const {
        return val;
    }
    
    std::string graphicalString() const;
    
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





enum SourceConvention {
    SOURCECONVENTION_UNKNOWN,
    SOURCECONVENTION_LINECOLUMN,
    SOURCECONVENTION_SOURCECHARACTERINDEX
};

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
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
};

static_assert(sizeof(SourceLocation) == 8, "Check your assumptions");

//
// For googletest
//
void PrintTo(const SourceLocation&, std::ostream*);



struct Source {
    
    SourceLocation Start;
    SourceLocation End;
    
    Source();
    
    Source(SourceLocation only);
    
    Source(size_t ) = delete;
    
    Source(SourceLocation start, SourceLocation end);
    
    Source(Source start, Source end);

#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    size_t size() const;
};

static_assert(sizeof(Source) == 16, "Check your assumptions");

bool operator==(Source a, Source b);

//
// For googletest
//
void PrintTo(const Source&, std::ostream*);




class Issue {
public:

    const SyntaxIssueTag Tag;
    const std::string Msg;
    const SyntaxIssueSeverity Sev;
    const Source Src;
    const double Val;
    const std::vector<CodeActionPtr> Actions;
    
    Issue(std::string Tag, std::string Msg, std::string Sev, Source Src, double Val, std::vector<CodeActionPtr> Actions);
    
    Source getSource() const;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
#endif // USE_MATHLINK
    
    virtual void print(std::ostream& s) const = 0;
    
    virtual ~Issue() {}
};

class CodeAction {
protected:
    const std::string Label;
    Source Src;
    
public:
    CodeAction(std::string Label, Source Src);

    Source getSource() const;
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
#endif // USE_MATHLINK
    
    virtual void print(std::ostream& s) const = 0;
    
    virtual ~CodeAction() {}
};

class ReplaceTextCodeAction : public CodeAction {
    std::string ReplacementText;
public:
    
    ReplaceTextCodeAction(std::string Label, Source Src, std::string ReplacementText) : CodeAction(Label, Src), ReplacementText(ReplacementText) {}

#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

class InsertTextCodeAction : public CodeAction {
    std::string InsertionText;
public:
    
    InsertTextCodeAction(std::string Label, Source Src, std::string InsertionText) : CodeAction(Label, Src), InsertionText(InsertionText) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

class InsertTextAfterCodeAction : public CodeAction {
    std::string InsertionText;
public:
    
    InsertTextAfterCodeAction(std::string Label, Source Src, std::string InsertionText) : CodeAction(Label, Src), InsertionText(InsertionText) {}

#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

class DeleteTextCodeAction : public CodeAction {
public:
    
    DeleteTextCodeAction(std::string Label, Source Src) : CodeAction(Label, Src) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

class DeleteTriviaCodeAction : public CodeAction {
public:
    
    DeleteTriviaCodeAction(std::string Label, Source Src) : CodeAction(Label, Src) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

class SyntaxIssue : public Issue {
public:
    SyntaxIssue(std::string Tag, std::string Msg, std::string Sev, Source Src, double Con, std::vector<CodeActionPtr> Actions) : Issue(Tag, Msg, Sev, Src, Con, std::move(Actions)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK

    void print(std::ostream& s) const override;
};

class FormatIssue : public Issue {
public:
    FormatIssue(std::string Tag, std::string Msg, std::string Sev, Source Src, double Air, std::vector<CodeActionPtr> Actions) : Issue(Tag, Msg, Sev, Src, Air, std::move(Actions)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

class EncodingIssue : public Issue {
public:
    EncodingIssue(std::string Tag, std::string Msg, std::string Sev, Source Src) : Issue(Tag, Msg, Sev, Src, 0.0, {}) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

