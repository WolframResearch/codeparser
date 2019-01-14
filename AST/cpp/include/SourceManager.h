
#pragma once

#include <map>
#include <string>
#include <iostream>

typedef int SourceCharacter;

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
