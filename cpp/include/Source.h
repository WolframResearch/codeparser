
#pragma once

#include "Token.h"

#include "mathlink.h"

#include <string>
#include <cassert>

//
// https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
//
#if defined NDEBUG
# define X_ASSERT(CHECK) void(0)
#else
# define X_ASSERT(CHECK) \
( (CHECK) ? void(0) : []{assert(false && #CHECK);}() )
#endif


enum Associativity {
    ASSOCIATIVITY_NONE,
    ASSOCIATIVITY_LEFT,
    ASSOCIATIVITY_RIGHT,
    ASSOCIATIVITY_NONASSOCIATIVE,
};


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
    
    //
    // Expected possible beginning of expression
    // Something like  a }
    //
    SYNTAXERROR_EXPECTEDPOSSIBLEEXPRESSION,
    
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
};

std::string SyntaxErrorToString(SyntaxError Err);

SyntaxError TokenErrorToSyntaxError(TokenEnum);



typedef const std::string SyntaxIssueTag;

SyntaxIssueTag SYNTAXISSUETAG_SYNTAXERROR = "SyntaxError";
SyntaxIssueTag SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER = "UnrecognizedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNSUPPORTEDCHARACTER = "UnsupportedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNDOCUMENTEDCHARACTER = "UndocumentedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNLIKELYESCAPESEQUENCE = "UnlikelyEscapeSequence";
SyntaxIssueTag SYNTAXISSUETAG_UNEXPECTEDEXPRESSION = "UnexpectedExpression";
SyntaxIssueTag SYNTAXISSUETAG_STRANGECHARACTER = "StrangeCharacter";
SyntaxIssueTag SYNTAXISSUETAG_SYNTAXAMBIGUITY = "SyntaxAmbiguity";
SyntaxIssueTag SYNTAXISSUETAG_NOTCONTIGUOUS = "NotContiguous";
SyntaxIssueTag SYNTAXISSUETAG_DIFFERENTLINE = "DifferentLine";
SyntaxIssueTag SYNTAXISSUETAG_ENDOFLINE = "EndOfLine";
SyntaxIssueTag SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT = "SyntaxUndocumentedSlot";
SyntaxIssueTag SYNTAXISSUETAG_CHARACTERENCODING = "CharacterEncoding";
SyntaxIssueTag SYNTAXISSUETAG_STRAYCARRIAGERETURN = "StrayCarriageReturn";
SyntaxIssueTag SYNTAXISSUETAG_STRAYLINECONTINUATION = "StrayLineContinuation";
SyntaxIssueTag SYNTAXISSUETAG_IMPLICITTIMESSPAN = "ImplicitTimesSpan";



//
// Used to be just SEVERITY_ERROR, etc.,
// but this was observed:
// c:\users\brenton\dropbox\wolfram\ast\ast\cpp\include\SyntaxIssue.h(19): warning C4005: 'SEVERITY_ERROR': macro redefinition
// C:\Program Files (x86)\Windows Kits\10\include\10.0.17763.0\shared\winerror.h(28563): note: see previous definition of 'SEVERITY_ERROR'

typedef const std::string SyntaxIssueSeverity;

SyntaxIssueSeverity SYNTAXISSUESEVERITY_FORMATTING = "Formatting";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_REMARK = "Remark";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_WARNING = "Warning";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_ERROR = "Error";
SyntaxIssueSeverity SYNTAXISSUESEVERITY_FATAL = "Fatal";




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
    
    std::string string() const;
    
    bool isAlphaOrDigit() const;
    
    bool isHex() const;
    
    bool isOctal() const;
    
    bool isUpper() const;
    
    bool isEndOfFile() const;
    
    bool isDigit() const;
};

std::ostream& operator<<(std::ostream& stream, const SourceCharacter);


struct SourceLocation {
    size_t Line;
    size_t Col;
    
    SourceLocation();
    
    SourceLocation(size_t Line, size_t Col);
    
    SourceLocation operator+(size_t i) const {
        return SourceLocation{Line, Col+i};
    }
    
    SourceLocation operator-(size_t i) const {
        return SourceLocation{Line, Col-i};
    }
};

struct Source_SourceLocation_struct {
    SourceLocation start;
    SourceLocation end;
};

struct Source_File_struct {
    uint64_t offset;
    uint64_t len;
};

//
// There are 2 different origins for Source
//
// 1. The traditional {{startLine, startCol}, {endLine, endCol}} information
// 2. The less common identifier that is used by boxes
//
union Source {
    
    //
    // Lines
    //
    Source_SourceLocation_struct lines;
    
    //
    // Front end boxes
    //
    uint64_t iid;
    
    //
    // File bytes
    //
    Source_File_struct file;
    
    Source();
    
    Source(SourceLocation loc);
    
    Source(SourceLocation start, SourceLocation end);
    
    void putLineCols(MLINK mlp) const;
    
    size_t size() const;
};

bool isContiguous(SourceLocation a, SourceLocation b);
bool isContiguous(Source a, Source b);

bool operator<=(SourceLocation a, SourceLocation b);


struct Token {
    
    TokenEnum Tok;
    std::string Str;
    Source Span;
    
    Token(TokenEnum, std::string, Source);
};


struct SyntaxIssue {
    const SyntaxIssueTag Tag;
    const std::string Msg;
    const SyntaxIssueSeverity Severity;
    const Source Span;
    
    //
    // Tag + Msg + Severity + 4 Source integers = 7
    //
    static const size_t SYNTAXISSUE_LENGTH = 7;
    
    SyntaxIssue(std::string Tag, std::string Msg, std::string Severity, Source Span) : Tag(Tag), Msg(Msg), Severity(Severity), Span(Span) {}
    
    void put(MLINK mlp) const;
};

struct Metadata {
    std::string Key;
    std::string Val;
    
    Metadata(std::string Key, std::string Val) : Key(Key), Val(Val) {}
    
    void put(MLINK mlp) const;
};

