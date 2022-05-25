
#include "API.h"

#include <memory> // for unique_ptr
#include <iostream>
#include <fstream> // for ofstream
#include <cstdio> // for rewind
#include <cstdlib> // for EXIT_SUCCESS

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
    CHECK,
};


int readStdIn(APIMode mode, OutputMode outputMode, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode);

int readFile(std::string file, APIMode mode, OutputMode outputMode, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode);

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
    auto firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    auto encodingMode = ENCODINGMODE_NORMAL;
    
    std::string fileInput;
    
    for (int i = 1; i < argc; i++) {
        auto arg = std::string(argv[i]);
        if (arg == "-file") {
            
            file = true;
            firstLineBehavior = FIRSTLINEBEHAVIOR_CHECK;
            
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
            
        } else if (arg == "-check") {
            
            outputMode = CHECK;
            
        } else {
            return EXIT_FAILURE;
        }
    }
    
    int result;
    
    if (file) {
        if (leaf) {
            result = readFile(fileInput, LEAF, outputMode, firstLineBehavior, encodingMode);
        } else if (sourceCharacters) {
            result = readFile(fileInput, SOURCECHARACTERS, outputMode, firstLineBehavior, encodingMode);
        } else if (tokenize) {
            result = readFile(fileInput, TOKENIZE, outputMode, firstLineBehavior, encodingMode);
        } else {
            result = readFile(fileInput, EXPRESSION, outputMode, firstLineBehavior, encodingMode);
        }
    } else {
        if (leaf) {
            result = readStdIn(LEAF, outputMode, firstLineBehavior, encodingMode);
        } else if (sourceCharacters) {
            result = readStdIn(SOURCECHARACTERS, outputMode, firstLineBehavior, encodingMode);
        } else if (tokenize) {
            result = readStdIn(TOKENIZE, outputMode, firstLineBehavior, encodingMode);
        } else {
            result = readStdIn(EXPRESSION, outputMode, firstLineBehavior, encodingMode);
        }
    }
    
    return result;
}

int readStdIn(APIMode mode, OutputMode outputMode, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode) {
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);
    
    ParserSessionCreate();
    
    WolframLibraryData libData = nullptr;
    
    int result = EXIT_SUCCESS;
    
    if (mode == TOKENIZE) {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        ParserSessionInit(inputStr, input.size(), libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
    
        auto C = ParserSessionTokenize();
        
        switch (outputMode) {
            case PRINT:
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                C->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
            }
                break;
            case NONE: case CHECK:
                break;
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
        
    } else if (mode == SOURCECHARACTERS) {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        ByteBufferInit(inputStr, input.size(), libData);
        ByteDecoderInit(SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, ENCODINGMODE_NORMAL);
    
        auto C = ParserSessionListSourceCharacters();
    
        switch (outputMode) {
            case PRINT:
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                C->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
            }
                break;
            case NONE: case CHECK:
                break;
        }
        
        ParserSessionReleaseContainer(C);
        
        ByteDecoderDeinit();
        ByteBufferDeinit();
        
    } else if (mode == LEAF) {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        ParserSessionInit(inputStr, input.size(), libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
        
        auto stringifyMode = STRINGIFYMODE_NORMAL;
        
        auto C = ParserSessionConcreteParseLeaf(stringifyMode);
    
        switch (outputMode) {
            case PRINT:
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                C->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
            }
                break;
            case NONE: case CHECK:
                break;
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
        
    } else {
        
        auto inputStr = reinterpret_cast<Buffer>(input.c_str());
        
        ParserSessionInit(inputStr, input.size(), libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
        
        auto C = ParserSessionParseExpressions();
        
        switch (outputMode) {
            case PRINT:
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                C->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
            }
                break;
            case CHECK: {
                if (!NodeContainerCheck(C)) {
                    result = EXIT_FAILURE;
                }
            }
                break;
            case NONE:
                break;
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
    }
    
    ParserSessionDestroy();
    
    return result;
}

int readFile(std::string file, APIMode mode, OutputMode outputMode, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode) {
    
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
            case PRINT_DRYRUN:
                break;
            case NONE: case CHECK:
                break;
        }
        return EXIT_FAILURE;
    }
    
    ParserSessionCreate();
    
    WolframLibraryData libData = nullptr;
    
    int result = EXIT_SUCCESS;
    
    if (mode == TOKENIZE) {
        
        ParserSessionInit(fb->getBuf(), fb->getLen(), libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
        
        auto C = ParserSessionTokenize();
        
        switch (outputMode) {
            case PRINT:
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                C->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
            }
                break;
            case NONE: case CHECK:
                break;
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
        
    } else if (mode == LEAF) {
        
        ParserSessionInit(fb->getBuf(), fb->getLen(), libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, ENCODINGMODE_NORMAL);
        
        auto stringifyMode = STRINGIFYMODE_NORMAL;
        
        auto C = ParserSessionConcreteParseLeaf(stringifyMode);
    
        switch (outputMode) {
            case PRINT:
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                C->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
            }
                break;
            case NONE: case CHECK:
                break;
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
        
    } else {
        
        ParserSessionInit(fb->getBuf(), fb->getLen(), libData, INCLUDE_SOURCE, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, ENCODINGMODE_NORMAL);
        
        auto C = ParserSessionParseExpressions();
        
        switch (outputMode) {
            case PRINT:
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            case PUT: {
#if USE_MATHLINK
                ScopedMLLoopbackLink loop;
                C->put(loop.get());
#endif // USE_MATHLINK
            }
                break;
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
            }
                break;
            case NONE:
                break;
            case CHECK: {
                if (!NodeContainerCheck(C)) {
                    result = EXIT_FAILURE;
                }
            }
                break;
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
    }
    
    ParserSessionDestroy();
    
    return result;
}

ScopedFileBuffer::ScopedFileBuffer(Buffer inStrIn, size_t inLen) : buf(), len(), inited(false) {
    
    auto inStr = reinterpret_cast<const char *>(inStrIn);
    
    FILE *file = fopen(inStr, "rb");
    
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

