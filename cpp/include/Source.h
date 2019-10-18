
#pragma once

#include "Symbol.h"
#include "TokenEnum.h"

#include "mathlink.h"

#include <string>
#include <cassert>
#include <iterator>
#include <array>
#include <vector>
#include <memory>

//
// https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
//
#if defined NDEBUG
# define X_ASSERT(CHECK) void(0)
#else
# define X_ASSERT(CHECK) \
( (CHECK) ? void(0) : []{assert(false && #CHECK);}() )
#endif

class CodeAction;

using CodeActionPtr = std::unique_ptr<CodeAction>;

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
SyntaxIssueTag SYNTAXISSUETAG_IMPLICITTIMESSPAN = "ImplicitTimesSpan";
SyntaxIssueTag SYNTAXISSUETAG_DIFFERENTLINE = "DifferentLine";
SyntaxIssueTag SYNTAXISSUETAG_ENDOFLINE = "EndOfLine";
SyntaxIssueTag SYNTAXISSUETAG_SPACE = "Space";

typedef const std::string FormatIssueTag;

//
// When the FormatIssue is made, details for SyntaxAmbiguitySpace will be filled in
//
// SyntaxAmbiguitySpace is: insert space between characters
//
FormatIssueTag FORMATISSUETAG_SPACE = "Space";
FormatIssueTag FORMATISSUETAG_NOTCONTIGUOUS = "NotContiguous";
FormatIssueTag FORMATISSUETAG_CHARACTERENCODING = "CharacterEncoding";
FormatIssueTag FORMATISSUETAG_UNEXPECTEDCARRIAGERETURN = "UnexpectedCarriageReturn";
FormatIssueTag FORMATISSUETAG_UNEXPECTEDLINECONTINUATION = "UnexpectedLineContinuation";


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
    
    int32_t valBits;
    
    explicit constexpr SourceCharacter(int val) : valBits(val) {}
    
    explicit operator int() const noexcept = delete;
    
    constexpr bool operator==(const SourceCharacter &o) const {
        return valBits == o.valBits;
    }
    
    constexpr bool operator!=(const SourceCharacter &o) const {
        return valBits != o.valBits;
    }
    
    constexpr int to_point() const {
        return valBits;
    }
    
    constexpr char to_char() const {
        //
        // https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
        //
        return X_ASSERT(0x00 <= valBits && valBits <= 0xff), valBits;
    }
    
    bool isAlphaOrDigit() const;
    
    bool isHex() const;
    
    bool isOctal() const;
    
    bool isUpper() const;
    
    bool isEndOfFile() const;
    
    bool isDigit() const;
    
    
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
            idx++;
            return *this;
        }
    };
    
    SourceCharacter_iterator begin();
    
    SourceCharacter_iterator end();
};

std::ostream& operator<<(std::ostream& stream, const SourceCharacter);



struct LineCol {
    size_t Line;
    size_t Col;
    
    LineCol();
    
    LineCol(size_t Line, size_t Col);
    
    LineCol operator+(size_t i) const {
        return LineCol(Line, Col+i);
    }
    
    LineCol operator-(size_t i) const {
        assert(Col > 0);
        return LineCol(Line, Col-i);
    }
};

bool isContiguous(LineCol a, LineCol b);

bool operator==(LineCol a, LineCol b);
bool operator<=(LineCol a, LineCol b);


struct Source_LineCol_struct {
    LineCol start;
    LineCol end;
    
    Source_LineCol_struct();
    Source_LineCol_struct(LineCol, LineCol);
};

bool operator==(Source_LineCol_struct a, Source_LineCol_struct b);



struct Offset {
    
    size_t val;
    
    Offset();
    
    Offset(size_t offset);
    
    Offset operator+(size_t i) const {
        return Offset(val + i);
    }
    
    Offset operator++(int ignored) {
        auto Tmp = *this;
        val++;
        return Tmp;
    }
};

Offset operator-(Offset a, Offset b);

bool operator==(Offset a, Offset b);
bool operator<=(Offset a, Offset b);

struct Source_OffsetLen_struct {
    Offset offset;
    size_t len;
};





enum SourceStyle {
    SOURCESTYLE_UNKNOWN,
    SOURCESTYLE_LINECOL,
    SOURCESTYLE_OFFSETLEN,
};

struct SourceLocation {
    
    SourceStyle style;
    
    union {
        
        LineCol lineCol;
        
        Offset offset;
    };
    
    SourceLocation();
    SourceLocation(SourceStyle);
    SourceLocation(LineCol loc);
    SourceLocation(Offset loc);
    
    SourceLocation(size_t loc) = delete;
    
    SourceLocation operator+(size_t);
    SourceLocation operator-(size_t);
    
    SourceLocation operator++(int);
    
    SourceLocation nextLine();
};

bool operator<=(SourceLocation a, SourceLocation b);






//
// There are several different kinds of Sources
//
// 1. The traditional {{startLine, startCol}, {endLine, endCol}} information
// 2. The less common positional identifier that is used by boxes
//
// There could be more kinds:
// Box ids
// Buffer offset and length
//
struct Source {
    
    SourceStyle style;
    
    union {
        
        Source_LineCol_struct lineCol;
        
        Source_OffsetLen_struct offsetLen;
    };
    
    Source();
    Source(SourceStyle);
    Source(SourceLocation loc);
    
    Source(LineCol) = delete;
    Source(size_t) = delete;
    
    Source(SourceLocation start, SourceLocation end);
    
    Source(Source start, Source end);
    
    Source(const Source& o);
    Source(Source&& o);
    
    Source& operator=(Source o);
    
    ~Source();
    
    void put(MLINK mlp) const;
    
    size_t size() const;
    
    size_t count() const;
    
    SourceLocation start() const;
    SourceLocation end() const;
};

bool isContiguous(Source a, Source b);


class Issue {
public:
    virtual void put(MLINK mlp) const = 0;
    
    virtual ~Issue() {}
};

class CodeAction {
protected:
    const std::string Label;
    Source Src;
public:
    CodeAction(std::string Label, Source Src) : Label(Label), Src(Src) {}
    
    virtual void put(MLINK mlp) const = 0;
    
    virtual ~CodeAction() {}
};

class ReplaceTextCodeAction : public CodeAction {
    std::string ReplacementText;
public:
    
    ReplaceTextCodeAction(std::string Label, Source Src, std::string ReplacementText) : CodeAction(Label, Src), ReplacementText(ReplacementText) {}
    
    void put(MLINK mlp) const override;
};

class InsertTextCodeAction : public CodeAction {
    std::string InsertionText;
public:
    
    InsertTextCodeAction(std::string Label, Source Src, std::string InsertionText) : CodeAction(Label, Src), InsertionText(InsertionText) {}
    
    void put(MLINK mlp) const override;
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
    
    void put(MLINK mlp) const override;
};

class FormatIssue : public Issue {
public:
    const FormatIssueTag Tag;
    const std::string Msg;
    const FormatIssueSeverity Sev;
    const Source Src;
    
    FormatIssue(std::string Tag, std::string Msg, std::string Sev, Source Src) : Tag(Tag), Msg(Msg), Sev(Sev), Src(Src) {}
    
    void put(MLINK mlp) const override;
};




