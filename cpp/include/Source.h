
#pragma once

#include "Symbol.h"

#include "mathlink.h"

#include <string>
#include <cassert>
#include <vector>
#include <sstream>

//
// https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
//
#if defined NDEBUG
# define X_ASSERT(CHECK) void(0)
#else
# define X_ASSERT(CHECK) \
( (CHECK) ? void(0) : []{assert(false && #CHECK);}() )
#endif

const std::string SYNTAXISSUETAG_SYNTAXERROR = "SyntaxError";
const std::string SYNTAXISSUETAG_UNRECOGNIZEDCHARACTER = "UnrecognizedCharacter";
const std::string SYNTAXISSUETAG_STRANGECHARACTER = "StrangeCharacter";
const std::string SYNTAXISSUETAG_SYNTAXAMBIGUITY = "SyntaxAmbiguity";
const std::string SYNTAXISSUETAG_NOTCONTIGUOUS = "NotContiguous";
const std::string SYNTAXISSUETAG_DIFFERENTLINE = "DifferentLine";
const std::string SYNTAXISSUETAG_MAXEXPRESSIONDEPTH = "MaxExpressionDepth";
const std::string SYNTAXISSUETAG_MAXEXPRESSIONBREADTH = "MaxExpressionBreadth";
const std::string SYNTAXISSUETAG_MAXCOMMENTLENGTH = "MaxCommentLength";
const std::string SYNTAXISSUETAG_MAXSYMBOLLENGTH = "MaxSymbolLength";
const std::string SYNTAXISSUETAG_MAXSTRINGLENGTH = "MaxStringLength";
const std::string SYNTAXISSUETAG_MAXPRECISIONLENGTH = "MaxPrecisionLength";
const std::string SYNTAXISSUETAG_MAXACCURACYLENGTH = "MaxAccuracyLength";
const std::string SYNTAXISSUETAG_MAXDIGITSLENGTH = "MaxDigitsLength";
const std::string SYNTAXISSUETAG_SYNTAXUNDOCUMENTEDSLOT = "SyntaxUndocumentedSlot";
const std::string SYNTAXISSUETAG_CHARACTERENCODING = "CharacterEncoding";
const std::string SYNTAXISSUETAG_STRANGECALL = "StrangeCall";

//
// Used to be just SEVERITY_ERROR, etc.,
// but this was observed:
// c:\users\brenton\dropbox\wolfram\ast\ast\cpp\include\SyntaxIssue.h(19): warning C4005: 'SEVERITY_ERROR': macro redefinition
// C:\Program Files (x86)\Windows Kits\10\include\10.0.17763.0\shared\winerror.h(28563): note: see previous definition of 'SEVERITY_ERROR'
//
const std::string SYNTAXISSUESEVERITY_FORMATTING = "Formatting";
const std::string SYNTAXISSUESEVERITY_REMARK = "Remark";
const std::string SYNTAXISSUESEVERITY_WARNING = "Warning";
const std::string SYNTAXISSUESEVERITY_ERROR = "Error";
const std::string SYNTAXISSUESEVERITY_FATAL = "Fatal";




class SourceCharacter
{
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
    
    std::vector<unsigned char> bytes() const;

private:
    int value_;
};

std::ostream& operator<<(std::ostream& stream, const SourceCharacter&);


constexpr SourceCharacter SOURCECHARACTER_BACKSLASH('\\');

struct SourceLocation {
    size_t Line;
    size_t Col;

    SourceLocation operator+(size_t i) {
        return SourceLocation{Line, Col+i};
    }

    SourceLocation operator-(size_t i) {
        return SourceLocation{Line, Col-i};
    }
};

struct SourceSpan {

    SourceLocation start;
    SourceLocation end;

    void putSourceRule(MLINK mlp) {

        MLPutFunction(mlp, SYMBOL_RULE.name(), 2);

        MLPutSymbol(mlp, SYMBOL_SOURCE.name());

        const int data[4] = {
            static_cast<int>(start.Line), static_cast<int>(start.Col),
            static_cast<int>(end.Line), static_cast<int>(end.Col)};

        const long dims[2] = {2, 2};

        MLPutIntegerArray(mlp, data, dims, nullptr, 2);
    }
};

bool isContiguous(SourceLocation a, SourceLocation b);
bool isContiguous(SourceSpan a, SourceSpan b);



struct SyntaxIssue {
    std::string Tag;
    std::string Msg;
    std::string Severity;
    SourceSpan Span;

    SyntaxIssue(std::string Tag, std::string Msg, std::string Severity, SourceSpan Span) : Tag(Tag), Msg(Msg), Severity(Severity), Span(Span) {}

    void put(MLINK mlp);
};

struct Comment {
	std::string Msg;
	SourceSpan Span;

	Comment(std::string Msg, SourceSpan Span) : Msg(Msg), Span(Span) {}

	void put(MLINK mlp);
};



class SourceManager {
    
    bool lastCharacterWasCarriageReturn;
    bool eof;

    std::vector<SyntaxIssue> Issues;
    
    SourceLocation SourceLoc;

    SourceLocation TokenStartLoc;
    SourceLocation TokenEndLoc;

    SourceLocation WLCharacterStartLoc;
    SourceLocation WLCharacterEndLoc;
    
    size_t CurLineWidth;
    
public:
    SourceManager();
    
    void init();

    void deinit();

    void advanceSourceLocation(SourceCharacter c);
    
    void setTokenStart();
    void setTokenEnd();
    
    void setWLCharacterStart();
    void setWLCharacterEnd();

    void setEOF();

    SourceSpan getTokenSpan();
    
    SourceLocation getWLCharacterStart();

    SourceSpan getWLCharacterSpan();

    void setSourceLocation(SourceLocation Loc);
    SourceLocation getSourceLocation();

    size_t getCurrentLineWidth();
    
    std::vector<SyntaxIssue> getIssues();
};

extern SourceManager *TheSourceManager;
