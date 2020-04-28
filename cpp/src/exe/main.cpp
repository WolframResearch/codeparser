
#include "ByteDecoder.h" // for TheByteDecoder
#include "ByteBuffer.h" // for TheByteBuffer
#include "API.h" // for TheParserSession

#include "Source.h" // for MBuffer

#include <memory> // for unique_ptr
#include <iostream>
#include <fstream> // for ofstream
#include <cstdio> // for rewind

class ScopedFileBuffer;
using ScopedFileBufferPtr = std::unique_ptr<ScopedFileBuffer>;

enum APIMode {
    EXPRESSION,
    TOKENIZE,
    LEAF,
    SOURCECHARACTERS,
};

enum OutputMode {
    NONE,
    PRINT,
    PUT,
    PRINT_DRYRUN,
};


void readStdIn(APIMode mode, OutputMode outputMode);

void readFile(std::string file, APIMode mode, OutputMode outputMode);

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
            
        } else if (arg == "-m") {
            
            outputMode = PUT;
            
        } else if (arg == "-sc") {
            
            sourceCharacters = true;
            
        } else {
            return 1;
        }
    }
    
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

void readStdIn(APIMode mode, OutputMode outputMode) {
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);
    
    TheParserSession = ParserSessionPtr(new ParserSession());
    
    WolframLibraryData libData = nullptr;
    
    if (mode == TOKENIZE) {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size());
        
        TheParserSession->init(inputBufAndLen, libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, TAB_WIDTH);
    
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
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size());
        
        TheByteBuffer->init(inputBufAndLen, libData);
        TheByteDecoder->init(SOURCECONVENTION_LINECOLUMN, TAB_WIDTH);
    
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
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size());
        
        TheParserSession->init(inputBufAndLen, libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, TAB_WIDTH);
        
        auto stringifyMode = STRINGIFYMODE_NORMAL;
        
        auto N = TheParserSession->concreteParseLeaf(stringifyMode);
    
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
        
        auto inputBufAndLen = BufferAndLength(inputStr, input.size());
        
        TheParserSession->init(inputBufAndLen, libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, TAB_WIDTH);
        
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

void readFile(std::string file, APIMode mode, OutputMode outputMode) {
    
    auto fb = ScopedFileBufferPtr(new ScopedFileBuffer(reinterpret_cast<Buffer>(file.c_str()), file.size()));

    if (fb->fail()) {
        switch (outputMode) {
            case PRINT:
                std::cout << "file open failed\n";
                break;
            case PUT: {
#if USE_MATHLINK

#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                
            }
                break;
            case NONE:
                break;
        }
        return;
    }
    
    TheParserSession = ParserSessionPtr(new ParserSession());
    
    WolframLibraryData libData = nullptr;
    
    if (mode == TOKENIZE) {
        
        auto fBufAndLen = BufferAndLength(fb->getBuf(), fb->getLen());
        
        TheParserSession->init(fBufAndLen, libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, TAB_WIDTH);
        
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
        
        auto fBufAndLen = BufferAndLength(fb->getBuf(), fb->getLen());
        
        TheParserSession->init(fBufAndLen, libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, TAB_WIDTH);
        
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
    
    TheParserSession.reset(nullptr);
}

ScopedFileBuffer::ScopedFileBuffer(Buffer inStrIn, size_t inLen) : buf(), len(), inited(false) {
    
    auto inStr = reinterpret_cast<const char *>(inStrIn);
    
    FILE * file = fopen(inStr, "rb");
    
    if (file == NULL) {
        return;
    }
    
    if (fseek(file, 0, SEEK_END)) {
        return;
    }
    
    auto res = ftell(file);
    if (res < 0) {
        return;
    }
    len = res;
    
    rewind(file);
    
    buf = new unsigned char[len];
    
    inited = true;
    
    auto r = fread(buf, sizeof(unsigned char), len, file);
    if (r != len) {
        inited = false;
        delete[] buf;
    }
    
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

