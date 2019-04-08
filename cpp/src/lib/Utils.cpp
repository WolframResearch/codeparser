
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
// Convert whitespace characters and control characters to graphical representations,
// appropriate for displaying
//
std::string makeGraphical(std::string s) {
    std::ostringstream graph;
    for (size_t i = 0; i < s.size(); i++) {
        auto c = s[i];
        switch (c) {
            //
            // whitespace characters
            //
            case '\r':
                graph << "\\r";
                break;
            case '\t':
                graph << "\\t";
                break;
            case '\n':
                graph << "\\n";
                break;
            //
            // control characters
            //
            case 0x00:
                graph << "\\.00";
                break;
            case 0x01:
                graph << "\\.01";
                break;
            case 0x02:
                graph << "\\.02";
                break;
            case 0x03:
                graph << "\\.03";
                break;
            case 0x04:
                graph << "\\.04";
                break;
            case 0x05:
                graph << "\\.05";
                break;
            case 0x06:
                graph << "\\.06";
                break;
            case 0x07:
                graph << "\\.07";
                break;
            case 0x08:
                graph << "\\.08";
                break;
            case 0x0e:
                graph << "\\.0e";
                break;
            case 0x0f:
                graph << "\\.0f";
                break;
            case 0x10:
                graph << "\\.10";
                break;
            case 0x11:
                graph << "\\.11";
                break;
            case 0x12:
                graph << "\\.12";
                break;
            case 0x13:
                graph << "\\.13";
                break;
            case 0x14:
                graph << "\\.14";
                break;
            case 0x15:
                graph << "\\.15";
                break;
            case 0x16:
                graph << "\\.16";
                break;
            case 0x17:
                graph << "\\.17";
                break;
            case 0x18:
                graph << "\\.18";
                break;
            case 0x19:
                graph << "\\.19";
                break;
            case 0x1a:
                graph << "\\.1a";
                break;
            case 0x1b:
                graph << "\\.1b";
                break;
            case 0x1c:
                graph << "\\.1c";
                break;
            case 0x1d:
                graph << "\\.1d";
                break;
            case 0x1e:
                graph << "\\.1e";
                break;
            case 0x01f:
                graph << "\\.1f";
                break;
            case 0x7f:
                graph << "\\.7f";
                break;
            //
            // everything else is untouched
            //
            default:
                graph << c;
                break;
        }
    }
    
    return graph.str();
}

