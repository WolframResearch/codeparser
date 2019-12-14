
#pragma once

#include "Symbol.h"
#include "TokenEnum.h"

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <string>
#include <cassert>
#include <iterator>
#include <array>
#include <vector>
#include <memory>

class Issue;
class CodeAction;

using IssuePtr = std::unique_ptr<Issue>;
using CodeActionPtr = std::unique_ptr<CodeAction>;


enum NextCharacterPolicyBits : uint8_t {
    
    //
    // Enable byte decoding issues
    //
    // This is used when peeking: no need to report issues while peeking
    //
    ENABLE_BYTE_DECODING_ISSUES = 0x01,
    
    //
    // Preserve whitespace after line continuation
    //
    // ToExpression["0.\\\n  6"] evaluates to 0.6 (whitespace is NOT preserved)
    //
    // But ToExpression["\"0.\\\n  6\""] evaluates to "0.  6" (whitespace IS preserved)
    //
    // FIXME: this could be handled by line continuation processing
    //
    PRESERVE_WS_AFTER_LC = 0x02,
    
    //
    // Enable character decoding issues
    //
    // "\c" gives a CharacterDecoding error (issues are ENABLED)
    //
    // (*\c*) does NOT give a CharacterDecoding error (issues are DISABLED)
    //
    // This is also used when peeking: no need to report issues while peeking
    //
    ENABLE_CHARACTER_DECODING_ISSUES = 0x04,
    
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
    
    //
    // Check for strange characters, such as control characters, REPLACEMENT CHARACTER, etc.
    //
    ENABLE_STRANGE_CHARACTER_CHECKING = 0x20,
};

using NextCharacterPolicy = uint8_t;

const NextCharacterPolicy TOPLEVEL = ENABLE_BYTE_DECODING_ISSUES | ENABLE_CHARACTER_DECODING_ISSUES | ENABLE_STRANGE_CHARACTER_CHECKING;

//
// Use this to disable checks
//
// newPolicy = policy & DISABLE_CHECKS_MASK;
//
// Using ~ and | promotes to int, so make sure to static_cast back to uint8_t
//
const NextCharacterPolicy DISABLE_BYTE_CHECKS_MASK = static_cast<uint8_t>(~(ENABLE_BYTE_DECODING_ISSUES) );

const NextCharacterPolicy DISABLE_CHARACTER_CHECKS_MASK = static_cast<uint8_t>(~(ENABLE_CHARACTER_DECODING_ISSUES | ENABLE_UNLIKELY_ESCAPE_CHECKING | ENABLE_STRANGE_CHARACTER_CHECKING) );

enum SyntaxError {
    
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
    // Something like  1:2  or  a:b:1:2
    //
    SYNTAXERROR_COLONERROR,
    
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

SyntaxIssueTag SYNTAXISSUETAG_SYNTAXERROR = "SyntaxError";
SyntaxIssueTag SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER = "UnrecognizedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNSUPPORTEDCHARACTER = "UnsupportedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNDOCUMENTEDCHARACTER = "UndocumentedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDESCAPESEQUENCE = "UnexpectedEscapeSequence";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDCHARACTER = "UnexpectedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT = "SyntaxUndocumentedSlot";
SyntaxIssueTag SYNTAXISSUETAG_IMPLICITTIMES = "ImplicitTimes";
SyntaxIssueTag SYNTAXISSUETAG_ENDOFLINE = "EndOfLine";

typedef const std::string FormatIssueTag;

//
// When the FormatIssue is made, details for SyntaxAmbiguitySpace will be filled in
//
// SyntaxAmbiguitySpace is: insert space between characters
//
FormatIssueTag FORMATISSUETAG_SPACE = "Space";
FormatIssueTag FORMATISSUETAG_SPACEAFTER = "SpaceAfter";
FormatIssueTag FORMATISSUETAG_NOTCONTIGUOUS = "NotContiguous";
FormatIssueTag FORMATISSUETAG_CHARACTERENCODING = "CharacterEncoding";
FormatIssueTag FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN = "UnexpectedCarriageReturn";
FormatIssueTag FORMATISSUETAG_UNEXPECTEDLINECONTINUATION = "UnexpectedLineContinuation";
SyntaxIssueTag FORMATISSUETAG_DIFFERENTLINE = "DifferentLine";


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
    
    int32_t val;
    
    explicit constexpr SourceCharacter(int val) : val(val) {}
    
    explicit operator int() const noexcept = delete;
    
    constexpr bool operator==(const SourceCharacter &o) const {
        return val == o.val;
    }
    
    constexpr bool operator!=(const SourceCharacter &o) const {
        return val != o.val;
    }
    
    constexpr int32_t to_point() const {
        return val;
    }
    
    bool isAlphaOrDigit() const;
    
    bool isHex() const;
    
    bool isOctal() const;
    
    bool isUpper() const;
    
    bool isEndOfFile() const;
    
    bool isDigit() const;
    
    bool isSpace() const;
    
    bool isNewline() const;
    
    bool isMBSpace() const;
    bool isMBNewline() const;
    
    
    class SourceCharacter_iterator {
        
    public:
        int32_t val;
        size_t size;
        size_t idx;
        std::array<unsigned char, 4> arr;
        
        SourceCharacter_iterator(int32_t val);
        
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

std::ostream& operator<<(std::ostream& stream, const SourceCharacter);


struct SourceLocation {
    
    size_t Line;
    size_t Column;
    
    SourceLocation();
    
    SourceLocation(size_t Line, size_t Column);
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
};

bool operator==(SourceLocation a, SourceLocation b);

bool operator<=(SourceLocation a, SourceLocation b);

bool isContiguous(SourceLocation a, SourceLocation b);

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
};

bool operator==(Source a, Source b);

bool isContiguous(Source a, Source b);

//
// For googletest
//
void PrintTo(const Source&, std::ostream*);




class Issue {
public:

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
    CodeAction(std::string Label, Source Src) : Label(Label), Src(Src) {}

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
    const SyntaxIssueTag Tag;
    const std::string Msg;
    const SyntaxIssueSeverity Sev;
    const Source Src;
    const double Con;
    const std::vector<CodeActionPtr> Actions;
    
    SyntaxIssue(std::string Tag, std::string Msg, std::string Sev, Source Src, double Con, std::vector<CodeActionPtr> Actions) : Tag(Tag), Msg(Msg), Sev(Sev), Src(Src), Con(Con), Actions(std::move(Actions)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};

class FormatIssue : public Issue {
public:
    const FormatIssueTag Tag;
    const std::string Msg;
    const FormatIssueSeverity Sev;
    const Source Src;
    const double Con;
    const std::vector<CodeActionPtr> Actions;
    
    FormatIssue(std::string Tag, std::string Msg, std::string Sev, Source Src, double Con, std::vector<CodeActionPtr> Actions) : Tag(Tag), Msg(Msg), Sev(Sev), Src(Src), Con(Con), Actions(std::move(Actions)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
};
