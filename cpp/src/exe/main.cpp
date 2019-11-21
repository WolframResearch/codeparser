
#include "Node.h"
#include "API.h"
#include "Symbol.h"

#if USE_MATHLINK
#include "mathlink.h"
#endif

#include <string>
#include <iostream>

int EXPRESSION = 0;
int TOKENIZE = 1;
int LEAF = 2;

std::string Style = "LineCol";

void readStdIn(int mode, bool printOutput);

void readFile(std::string file, int mode, bool printOutput);

class ScopedFileBuffer {

    unsigned char *buf;
    size_t len;

    bool inited;

public:

    ScopedFileBuffer(const unsigned char *inStrIn, size_t inLen);

    ~ScopedFileBuffer();

    unsigned char *getBuf() const;

    size_t getLen() const;

    bool fail() const;

};

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
    
    TheParserSession = std::unique_ptr<ParserSession>(new ParserSession());
    
    WolframLibraryData libData = nullptr;
    
    Node *N;
    
    if (mode == TOKENIZE) {
        
        auto inputStr = reinterpret_cast<const unsigned char*>(input.c_str());
        
        N = TokenizeBytes(libData, inputStr, input.size(), Style.c_str());
        
    } else if (mode == LEAF) {
        
        auto inputStr = reinterpret_cast<const unsigned char*>(input.c_str());
        
        N = ParseLeaf(libData, inputStr, input.size(), Style.c_str(), 0);
        
    } else {
        
        auto inputStr = reinterpret_cast<const unsigned char*>(input.c_str());
        
        N = ConcreteParseBytes(libData, inputStr, input.size(), Style.c_str());
    }
    
    if (printOutput) {
        N->print(std::cout);
        std::cout << "\n";
    }
}

void readFile(std::string file, int mode, bool printOutput) {
    
    ScopedFileBuffer fb(reinterpret_cast<const unsigned char *>(file.c_str()), file.size());

    if (fb.fail()) {
        return;
    }
    
    TheParserSession = std::unique_ptr<ParserSession>(new ParserSession());
    
    WolframLibraryData libData = nullptr;
    
    Node *N;
    
    if (mode == TOKENIZE) {
        
        N = TokenizeBytes(libData, fb.getBuf(), fb.getLen(), Style.c_str());
        
    } else {
        
        N = ConcreteParseBytes(libData, fb.getBuf(), fb.getLen(), Style.c_str());
        
    }
    
    if (printOutput) {
        N->print(std::cout);
        std::cout << "\n";
    }
}

ScopedFileBuffer::ScopedFileBuffer(const unsigned char *inStrIn, size_t inLen) : buf(), len(), inited(false) {

    auto inStr = reinterpret_cast<const char *>(inStrIn);

    FILE * file = fopen(inStr, "rb");

    if (file == NULL) {
        return;
    }

    fseek(file, 0, SEEK_END);

    len = ftell(file);

    fclose(file);

    file = fopen(inStr, "rb");

    buf = new unsigned char[len];

    inited = true;

    fread(buf, sizeof(unsigned char), len, file);

    fclose(file);
}

ScopedFileBuffer::~ScopedFileBuffer() {

    if (!inited) {
        return;
    }

    delete[] buf;
}

unsigned char *ScopedFileBuffer::getBuf() const {
    return buf;
}

size_t ScopedFileBuffer::getLen() const {
    return len;
}

bool ScopedFileBuffer::fail() const {
    return !inited;
}

