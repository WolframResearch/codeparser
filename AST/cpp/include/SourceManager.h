
#pragma once

#include "ByteDecoder.h"

#include <map>
#include <string>
#include <iostream>

struct SourceLocation {
    int Line;
    int Col;
    
    std::string string() const {
        return "{" + std::to_string(Line) + ", " + std::to_string(Col) + "}";
    }
    
    void string(std::ostream& os) const {
        os << "{" << Line << ", " << Col << "}";
    }

    SourceLocation operator+(int i) {
        return SourceLocation{Line, Col+i};
    }

    SourceLocation operator-(int i) {
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
    
    int CurLineWidth;
    
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
};

extern SourceManager *TheSourceManager;
