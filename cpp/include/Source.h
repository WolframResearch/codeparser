
#pragma once

#include "Symbol.h"

#include "mathlink.h"

#include <string>
#include <cassert>
#include <vector>
#include <sstream>
#include <chrono>

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
    // Something like  1:2
    //
    SYNTAXERROR_EXPECTEDSYMBOL,
    
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
    // Something like  a ? b ? c
    //
    SYNTAXERROR_NONASSOCIATIVE,
    
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
};

std::string SyntaxErrorToString(SyntaxError Err);

SyntaxError TokenErrorToSyntaxError(TokenEnum);



typedef const std::string SyntaxIssueTag;

SyntaxIssueTag SYNTAXISSUETAG_SYNTAXERROR = "SyntaxError";
SyntaxIssueTag SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER = "UnrecognizedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNSUPPORTEDCHARACTER = "UnsupportedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_UNDOCUMENTEDCHARACTER = "UndocumentedCharacter";
SyntaxIssueTag SYNTAXISSUETAG_ESCAPESEQUENCE = "EscapeSequence";
SyntaxIssueTag SYNTAXISSUETAG_STRANGECHARACTER = "StrangeCharacter";
SyntaxIssueTag SYNTAXISSUETAG_SYNTAXAMBIGUITY = "SyntaxAmbiguity";
SyntaxIssueTag SYNTAXISSUETAG_NOTCONTIGUOUS = "NotContiguous";
SyntaxIssueTag SYNTAXISSUETAG_DIFFERENTLINE = "DifferentLine";
SyntaxIssueTag SYNTAXISSUETAG_ENDOFLINE = "EndOfLine";
SyntaxIssueTag SYNTAXISSUETAG_MAXEXPRESSIONDEPTH = "MaxExpressionDepth";
SyntaxIssueTag SYNTAXISSUETAG_MAXEXPRESSIONBREADTH = "MaxExpressionBreadth";
SyntaxIssueTag SYNTAXISSUETAG_MAXCOMMENTLENGTH = "MaxCommentLength";
SyntaxIssueTag SYNTAXISSUETAG_MAXSYMBOLLENGTH = "MaxSymbolLength";
SyntaxIssueTag SYNTAXISSUETAG_MAXSTRINGLENGTH = "MaxStringLength";
SyntaxIssueTag SYNTAXISSUETAG_MAXPRECISIONLENGTH = "MaxPrecisionLength";
SyntaxIssueTag SYNTAXISSUETAG_MAXACCURACYLENGTH = "MaxAccuracyLength";
SyntaxIssueTag SYNTAXISSUETAG_MAXDIGITSLENGTH = "MaxDigitsLength";
SyntaxIssueTag SYNTAXISSUETAG_MAXLONGNAMELENGTH = "MaxLongNameLength";
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
class SourceCharacter {
private:
    int value_;
public:
    explicit constexpr SourceCharacter(int val) : value_(val) {}

    explicit operator int() const noexcept = delete;

    bool operator==(const SourceCharacter &o) const {
        return value_ == o.value_;
    }

    bool operator!=(const SourceCharacter &o) const {
        return value_ != o.value_;
    }

   constexpr int to_point() const {
       return value_;
   }

   constexpr char to_char() const {
        //
        // https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
        //
        return X_ASSERT(0x00 <= value_ && value_ <= 0xff), value_;
    }
    
    std::string string() const;
    
    bool isDigitOrAlpha() const;

    bool isHex() const;

    bool isOctal() const;

    bool isUpper() const;
};

std::ostream& operator<<(std::ostream& stream, const SourceCharacter);


constexpr SourceCharacter SOURCECHARACTER_ENDOFFILE(EOF);
constexpr SourceCharacter SOURCECHARACTER_BACKSLASH('\\');

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
    
    void putSourceRule(MLINK mlp) const;
    
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

    SyntaxIssue(std::string Tag, std::string Msg, std::string Severity, Source Span) : Tag(Tag), Msg(Msg), Severity(Severity), Span(Span) {}

    void put(MLINK mlp) const;
};

struct Metadata {
    std::string Key;
    std::string Val;
    
    Metadata(std::string Key, std::string Val) : Key(Key), Val(Val) {}
    
    void put(MLINK mlp) const;
};




class SourceManager {
    
    bool lastCharacterWasCarriageReturn;
    bool eof;

    std::vector<SyntaxIssue> Issues;
    
    SourceLocation SourceLoc;

    SourceLocation TokenStartLoc;

    SourceLocation WLCharacterStartLoc;
    SourceLocation WLCharacterEndLoc;
    
    SourceLocation PrevWLCharacterStartLoc;
    SourceLocation PrevWLCharacterEndLoc;
    
public:
    SourceManager();
    
    void init();

    void deinit();

    void advanceSourceLocation(SourceCharacter c);
    
    void setTokenStart();
    
    void setWLCharacterStart();
    void setWLCharacterEnd();

    Source getTokenSpan() const;
    
    SourceLocation getWLCharacterStart() const;

    Source getWLCharacterSpan() const;

    SourceLocation getTokenStart() const;
    
    void setSourceLocation(SourceLocation Loc);
    SourceLocation getSourceLocation() const;
    
    std::vector<SyntaxIssue> getIssues() const;
};

extern SourceManager *TheSourceManager;
