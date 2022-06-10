
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
class Symbol;
class MyString;

using Buffer = const unsigned char *;
using MBuffer = unsigned char *;
using IssuePtr = std::shared_ptr<Issue>;
using CodeActionPtr = std::unique_ptr<CodeAction>;
using IssuePtrSet = std::set<IssuePtr, IssuePtrCompare>;
using CodeActionPtrVector = std::vector<CodeActionPtr>;
using AdditionalDescriptionVector = std::vector<std::string>;

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


//
//
//
struct BufferAndLength {
    
    Buffer buffer;
    Buffer end;
    
    BufferAndLength();
    BufferAndLength(Buffer buffer, size_t length = 0);
    
    size_t length() const;
    
    void print(std::ostream& s) const;
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
#if USE_EXPR_LIB
    expr toExpr() const;
#endif // USE_EXPR_LIB
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
        const codepoint val;
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
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
#if USE_EXPR_LIB
    expr toExpr() const;
#endif // USE_EXPR_LIB
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
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
#if USE_EXPR_LIB
    expr toExpr() const;
#endif // USE_EXPR_LIB
    
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
    
    const Symbol& MakeSym;
    const MyString& Tag;
    const std::string Msg;
    const MyString& Sev;
    const Source Src;
    const double Val;
    const CodeActionPtrVector Actions;
    const AdditionalDescriptionVector AdditionalDescriptions;
    
    Issue(const Symbol& MakeSym, const MyString& Tag, std::string Msg, const MyString& Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    bool check() const;
    
#if USE_EXPR_LIB
    expr toExpr() const;
#endif // USE_EXPR_LIB
};


//
//
//
class CodeAction {
protected:
    
    const std::string Label;
    const Source Src;
    
public:
    
    CodeAction(std::string Label, Source Src);
    
#if USE_MATHLINK
    virtual void put(MLINK mlp) const = 0;
#endif // USE_MATHLINK
    
    virtual void print(std::ostream& s) const = 0;
    
#if USE_EXPR_LIB
    virtual expr toExpr() const = 0;
#endif // USE_EXPR_LIB
    
    virtual ~CodeAction() {}
};


//
//
//
class ReplaceTextCodeAction : public CodeAction {
private:
    
    const std::string ReplacementText;
    
public:
    
    ReplaceTextCodeAction(std::string Label, Source Src, std::string ReplacementText) : CodeAction(Label, Src), ReplacementText(ReplacementText) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
//
//
class InsertTextCodeAction : public CodeAction {
private:
    
    const std::string InsertionText;
    
public:
    
    InsertTextCodeAction(std::string Label, Source Src, std::string InsertionText) : CodeAction(Label, Src), InsertionText(InsertionText) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const override;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const override;
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
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
    
#if USE_EXPR_LIB
    expr toExpr() const override;
#endif // USE_EXPR_LIB
};

//
//
//
class SyntaxIssue : public Issue {
public:
    
    SyntaxIssue(const MyString& Tag, std::string Msg, const MyString& Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
};

//
//
//
class FormatIssue : public Issue {
public:
    
    FormatIssue(const MyString& Tag, std::string Msg, const MyString& Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
};

//
//
//
class EncodingIssue : public Issue {
public:
    
    EncodingIssue(const MyString& Tag, std::string Msg, const MyString& Sev, Source Src, double Val, CodeActionPtrVector Actions, AdditionalDescriptionVector AdditionalDescriptions);
};
