
#include "Node.h"
#include "API.h"
#include "Symbol.h"

#include "mathlink.h"

#include <string>
#include <iostream>

int EXPRESSION = 0;
int TOKENIZE = 1;
int LEAF = 2;

std::string Style = "LineCol";

void readStdIn(int mode, bool printOutput);

void readFile(std::string file, int mode, bool printOutput);

int main(int argc, char *argv[]) {
    
    auto file = false;
    auto tokenize = false;
    auto leaf = false;
    auto printOutput = true;
    
    std::string fileInput;
    
    for (int i = 1; i < argc; i++) {
        auto arg = std::string(argv[i]);
        if (arg == "-file") {
            
            file = true;
            
            i++;
            fileInput = std::string(argv[i]);

        } else if (arg == "-tokenize") {
            
            tokenize = true;
            
        } else if (arg == "-leaf") {
            
            leaf = true;
            
        } else if (arg == "-n") {
            
            printOutput = false;
            
        } else {
            return 1;
        }
    }
    
    if (file) {
        if (leaf) {
            readFile(fileInput, LEAF, printOutput);
        } else if (tokenize) {
            readFile(fileInput, TOKENIZE, printOutput);
        } else {
            readFile(fileInput, EXPRESSION, printOutput);
        }
    } else {
        if (leaf) {
            readStdIn(LEAF, printOutput);
        } else if (tokenize) {
            readStdIn(TOKENIZE, printOutput);
        } else {
            readStdIn(EXPRESSION, printOutput);
        }
    }
    
    return 0;
}

void readStdIn(int mode, bool printOutput) {
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);
    
    ParserSession TheParserSession;
    
    WolframLibraryData libData = nullptr;
    
    Node *N;
    
    if (mode == TOKENIZE) {
        
        auto inputStr = reinterpret_cast<const unsigned char*>(input.c_str());
        
        N = TokenizeString(libData, inputStr, input.size(), Style.c_str());
        
    } else if (mode == LEAF) {
        
        auto inputStr = reinterpret_cast<const unsigned char*>(input.c_str());
        
        N = ParseLeaf(libData, inputStr, input.size(), Style.c_str(), "False", "False");
        
    } else {
        
        auto inputStr = reinterpret_cast<const unsigned char*>(input.c_str());
        
        N = ConcreteParseString(libData, inputStr, input.size(), Style.c_str());
    }
    
    if (printOutput) {
        N->print(std::cout);
        std::cout << "\n";
    }
}

void readFile(std::string file, int mode, bool printOutput) {
    
    ParserSession TheParserSession;
    
    WolframLibraryData libData = nullptr;
    
    Node *N;
    
    if (mode == TOKENIZE) {
        
        auto fileStr = reinterpret_cast<const unsigned char*>(file.c_str());
        
        N = TokenizeFile(libData, fileStr, file.size(), Style.c_str());
        
    } else {
        
        auto fileStr = reinterpret_cast<const unsigned char*>(file.c_str());
        
        N = ConcreteParseFile(libData, fileStr, file.size(), Style.c_str());
        
    }
    
    if (printOutput) {
        N->print(std::cout);
        std::cout << "\n";
    }
}
