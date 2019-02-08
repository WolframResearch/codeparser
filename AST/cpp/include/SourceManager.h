
#pragma once

#include <map>
#include <string>
#include <iostream>
#include <cassert>
#include <vector>

//
// https://akrzemi1.wordpress.com/2017/05/18/asserts-in-constexpr-functions/
//
#if defined NDEBUG
# define X_ASSERT(CHECK) void(0)
#else
# define X_ASSERT(CHECK) \
( (CHECK) ? void(0) : []{assert(false && #CHECK);}() )
#endif


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

    bool isDigitOrAlpha() const;

    bool isHex() const;

    bool isOctal() const;

    std::vector<unsigned char> bytes() const;

private:
    int value_;
};


constexpr SourceCharacter SOURCECHARACTER_BACKSLASH('\\');

struct SourceLocation {
    size_t Line;
    size_t Col;
    
    std::string string() const {
        return "{" + std::to_string(Line) + ", " + std::to_string(Col) + "}";
    }
    
    void string(std::ostream& os) const {
        os << "{" << Line << ", " << Col << "}";
    }

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
    
    std::string string() const {
        return "{" + start.string() + ", " + end.string() + "}";
    }
    
    void string(std::ostream& os) const {
        os << "{";
        start.string(os);
        os << ", ";
        end.string(os);
        os << "}";
    }
};

bool isContiguous(SourceLocation a, SourceLocation b);
bool isContiguous(SourceSpan a, SourceSpan b);


class SourceManager {
    
    bool eof;

    SourceLocation SourceLoc;

    SourceLocation TokenStartLoc;
    SourceLocation TokenEndLoc;

    SourceLocation WLCharacterStartLoc;
    SourceLocation WLCharacterEndLoc;
    
    size_t CurLineWidth;
    
public:
    SourceManager();
    
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
};

extern SourceManager *TheSourceManager;
