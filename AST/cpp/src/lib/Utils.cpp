
#include "Utils.h"

#include <sstream>

int parseInteger(std::string s, int base) {
    return std::stoi(s, nullptr, base);
}

std::string stringEscape(std::string s) {
    std::ostringstream escaped;
    escaped << "\"";
    for (size_t i = 0; i < s.size(); i++) {
        auto c = s[i];
        switch (c) {
            case '\\':
                escaped << "\\\\";
                break;
            case '"':
                escaped << "\\\"";
                break;
            default:
                escaped << c;
                break;
        }
    }
    escaped << "\"";
    
    return escaped.str();
}

//
// Convert whitespace characters to graphical representations,
// appropriate for displaying
//
std::string makeGraphical(std::string s) {
    std::ostringstream graph;
    for (size_t i = 0; i < s.size(); i++) {
        auto c = s[i];
        switch (c) {
            case '\r':
                graph << "\\r";
                break;
            case '\t':
                graph << "\\t";
                break;
            case '\n':
                graph << "\\n";
                break;
            default:
                graph << c;
                break;
        }
    }
    
    return graph.str();
}

