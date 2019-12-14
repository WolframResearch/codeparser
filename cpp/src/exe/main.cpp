
#include "Node.h"
#include "API.h"
#include "Symbol.h"

#include <string>
#include <iostream>
#include <fstream>

const int EXPRESSION = 0;
const int TOKENIZE = 1;
const int LEAF = 2;
const int SOURCECHARACTERS = 3;

const int NONE = 0;
const int PRINT = 1;
const int PUT = 2;
const int PRINT_DRYRUN = 3;

void readStdIn(int mode, int outputMode);

void readFile(std::string file, int mode, int outputMode);

class ScopedFileBuffer {

    MBuffer buf;
    size_t len;

    bool inited;

public:

    ScopedFileBuffer(Buffer inStrIn, size_t inLen);

    ~ScopedFileBuffer();

    Buffer getBuf() const;

    size_t getLen() const;

    bool fail() const;

};

int main(int argc, char *argv[]) {
    
    auto file = false;
    auto tokenize = false;
    auto leaf = false;
    auto outputMode = PRINT;
    auto sourceCharacters = false;
    
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
            
            outputMode = NONE;
            
        } else if (arg == "-sc") {
            
            sourceCharacters = true;
            
        } else {
            return 1;
        }
    }
    
//    file = true;
//    fileInput = "/Users/brenton/Downloads/Helped Code Bin and Count.nb";
//    fileInput = "/Users/brenton/development/stash/WA/alphasource/CalculateParse/Disambiguation/DisambiguationRaw.m";
//    fileInput = "/Applications/Mathematica121-6519725.app/Contents/AddOns/Applications/FormulaData/Kernel/downvalues.m";
//    fileInput = "/Users/brenton/development/stash/COD/ast/build/test.m";
//    fileInput = "/Users/brenton/development/stash/COD/ast/Tests/files/inputs-0001.txt";
//    fileInput = "/Users/brenton/development/stash/PAC/semanticimport/mathematica/scoresAndSpellings/data/UnitScores.m";
//    outputMode = PUT;
    
    if (file) {
        if (leaf) {
            readFile(fileInput, LEAF, outputMode);
        } else if (sourceCharacters) {
            readFile(fileInput, SOURCECHARACTERS, outputMode);
        } else if (tokenize) {
            readFile(fileInput, TOKENIZE, outputMode);
        } else {
            readFile(fileInput, EXPRESSION, outputMode);
        }
    } else {
        if (leaf) {
            readStdIn(LEAF, outputMode);
        } else if (sourceCharacters) {
            readStdIn(SOURCECHARACTERS, outputMode);
        } else if (tokenize) {
            readStdIn(TOKENIZE, outputMode);
        } else {
            readStdIn(EXPRESSION, outputMode);
        }
    }
    
    return 0;
}

void readStdIn(int mode, int outputMode) {
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);
    
    TheParserSession = ParserSessionPtr(new ParserSession());
    
    WolframLibraryData libData = nullptr;
    
    if (mode == TOKENIZE) {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size(), false);
        
        TheParserSession->init(inputBufAndLen, libData, INCLUDE_SOURCE);
    
        auto N = TheParserSession->tokenize();
        
        switch (outputMode) {
            case PRINT:
                N->print(std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                N->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                N->print(nullStream);
                nullStream << "\n";
            }
                break;
            case NONE:
                break;
        }
        
        TheParserSession->releaseNode(N);
        
        TheParserSession->deinit();
        
    } else if (mode == SOURCECHARACTERS) {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size(), false);
        
        TheByteBuffer->init(inputBufAndLen, libData);
        TheByteDecoder->init();
    
        auto N = TheParserSession->listSourceCharacters();
    
        switch (outputMode) {
            case PRINT:
                N->print(std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                N->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                N->print(nullStream);
                nullStream << "\n";
            }
                break;
            case NONE:
                break;
        }
        
        TheParserSession->releaseNode(N);
        
        TheByteDecoder->deinit();
        TheByteBuffer->deinit();
        
    } else if (mode == LEAF) {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size(), false);
        
        TheParserSession->init(inputBufAndLen, libData, INCLUDE_SOURCE);
    
        auto N = TheParserSession->parseLeaf(mode);
    
        switch (outputMode) {
            case PRINT:
                N->print(std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                N->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                N->print(nullStream);
                nullStream << "\n";
            }
                break;
            case NONE:
                break;
        }
        
        TheParserSession->releaseNode(N);
        
        TheParserSession->deinit();
        
    } else {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size(), false);
        
        TheParserSession->init(inputBufAndLen, libData, INCLUDE_SOURCE);
        
        auto N = TheParserSession->parseExpressions();
        
        switch (outputMode) {
            case PRINT:
                N->print(std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                N->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                N->print(nullStream);
                nullStream << "\n";
            }
                break;
            case NONE:
                break;
        }
        
        TheParserSession->releaseNode(N);
        
        TheParserSession->deinit();
    }
}

void readFile(std::string file, int mode, int outputMode) {
    
    ScopedFileBuffer fb(reinterpret_cast<Buffer>(file.c_str()), file.size());

    if (fb.fail()) {
        return;
    }
    
    TheParserSession = ParserSessionPtr(new ParserSession());
    
    WolframLibraryData libData = nullptr;
    
    if (mode == TOKENIZE) {
        
        auto fBufAndLen = BufferAndLength(fb.getBuf(), fb.getLen(), false);
        
        TheParserSession->init(fBufAndLen, libData, INCLUDE_SOURCE);
        
        auto N = TheParserSession->tokenize();
        
        switch (outputMode) {
            case PRINT:
                N->print(std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                N->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                N->print(nullStream);
                nullStream << "\n";
            }
                break;
            case NONE:
                break;
        }
        
        TheParserSession->releaseNode(N);
        
        TheParserSession->deinit();
        
    } else {
        
        auto fBufAndLen = BufferAndLength(fb.getBuf(), fb.getLen(), false);
        
        TheParserSession->init(fBufAndLen, libData, INCLUDE_SOURCE);
        
        auto N = TheParserSession->parseExpressions();
        
        switch (outputMode) {
            case PRINT:
                N->print(std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                N->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                N->print(nullStream);
                nullStream << "\n";
            }
                break;
            case NONE:
                break;
        }
        
        TheParserSession->releaseNode(N);
        
        TheParserSession->deinit();
    }
}

ScopedFileBuffer::ScopedFileBuffer(Buffer inStrIn, size_t inLen) : buf(), len(), inited(false) {

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

Buffer ScopedFileBuffer::getBuf() const {
    return buf;
}

size_t ScopedFileBuffer::getLen() const {
    return len;
}

bool ScopedFileBuffer::fail() const {
    return !inited;
}

