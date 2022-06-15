
#include "API.h"

#include <memory> // for unique_ptr
#include <iostream>
#include <fstream> // for ofstream
#include <cstdio> // for rewind
#include <cstdlib> // for EXIT_SUCCESS
#include <cstddef> // for size_t

class ScopedFileBuffer;

using ScopedFileBufferPtr = std::unique_ptr<ScopedFileBuffer>;


enum APIMode {
    EXPRESSION,
    TOKENIZE,
    LEAF,
};

enum OutputMode {
    NONE,
    PRINT,
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
        } else if (tokenize) {
            result = readFile(fileInput, TOKENIZE, outputMode, firstLineBehavior, encodingMode);
        } else {
            result = readFile(fileInput, EXPRESSION, outputMode, firstLineBehavior, encodingMode);
        }
    } else {
        if (leaf) {
            result = readStdIn(LEAF, outputMode, firstLineBehavior, encodingMode);
        } else if (tokenize) {
            result = readStdIn(TOKENIZE, outputMode, firstLineBehavior, encodingMode);
        } else {
            result = readStdIn(EXPRESSION, outputMode, firstLineBehavior, encodingMode);
        }
    }
    
#if DIAGNOSTICS
                
    std::cout << "CharacterDecoder_LineContinuationCount: " << CharacterDecoder_LineContinuationCount << "\n";

    std::cout << "CharacterDecoder_LongNameCount: " << CharacterDecoder_LongNameCount << "\n";

    std::cout << "CharacterDecoder_4HexCount: " << CharacterDecoder_4HexCount << "\n";

    std::cout << "CharacterDecoder_2HexCount: " << CharacterDecoder_2HexCount << "\n";

    std::cout << "CharacterDecoder_6HexCount: " << CharacterDecoder_6HexCount << "\n";
    
    std::cout << "CharacterDecoder_OctalCount: " << CharacterDecoder_OctalCount << "\n";

    std::cout << "CharacterDecoder_StringMetaBackspace: " << CharacterDecoder_StringMetaBackspace << "\n";

    std::cout << "CharacterDecoder_StringMetaFormFeed: " << CharacterDecoder_StringMetaFormFeed << "\n";

    std::cout << "CharacterDecoder_StringMetaLineFeedCount: " << CharacterDecoder_StringMetaLineFeedCount << "\n";

    std::cout << "CharacterDecoder_StringMetaCarriageReturn: " << CharacterDecoder_StringMetaCarriageReturn << "\n";

    std::cout << "CharacterDecoder_StringMetaTab: " << CharacterDecoder_StringMetaTab << "\n";

    std::cout << "CharacterDecoder_StringMetaDoubleQuoteCount: " << CharacterDecoder_StringMetaDoubleQuoteCount << "\n";

    std::cout << "CharacterDecoder_StringMetaBackslashCount: " << CharacterDecoder_StringMetaBackslashCount << "\n";

    std::cout << "CharacterDecoder_StringMetaOpenCount: " << CharacterDecoder_StringMetaOpenCount << "\n";

    std::cout << "CharacterDecoder_StringMetaCloseCount: " << CharacterDecoder_StringMetaCloseCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxBangCount: " << CharacterDecoder_LinearSyntaxBangCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxPercentCount: " << CharacterDecoder_LinearSyntaxPercentCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxAmpCount: " << CharacterDecoder_LinearSyntaxAmpCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxOpenParenCount: " << CharacterDecoder_LinearSyntaxOpenParenCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxCloseParenCount: " << CharacterDecoder_LinearSyntaxCloseParenCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxStarCount: " << CharacterDecoder_LinearSyntaxStarCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxPlusCount: " << CharacterDecoder_LinearSyntaxPlusCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxSlashCount: " << CharacterDecoder_LinearSyntaxSlashCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxAtCount: " << CharacterDecoder_LinearSyntaxAtCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxCaretCount: " << CharacterDecoder_LinearSyntaxCaretCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxUnderscoreCount: " << CharacterDecoder_LinearSyntaxUnderscoreCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxBacktickCount: " << CharacterDecoder_LinearSyntaxBacktickCount << "\n";
    
    std::cout << "CharacterDecoder_LinearSyntaxSpaceCount: " << CharacterDecoder_LinearSyntaxSpaceCount << "\n";

    std::cout << "CharacterDecoder_UnhandledCount: " << CharacterDecoder_UnhandledCount << "\n";
    
#endif // DIAGNOSTICS
    
    return result;
}

int readStdIn(APIMode mode, OutputMode outputMode, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode) {
    
    WolframLibraryData libData = nullptr;
    
    ParserSessionCreate();
    
    int result = EXIT_SUCCESS;
    
    while (true) {
        
        std::string input;
        std::cout << ">>> ";
        std::getline(std::cin, input);
        
        if (mode == TOKENIZE) {
            
            auto inputStr = reinterpret_cast<Buffer>(input.c_str());
            
            ParserSessionInit(inputStr, input.size(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
        
            auto C = ParserSessionTokenize();
            
            switch (outputMode) {
                case PRINT: {
                    NodeContainerPrint(C, std::cout);
                    std::cout << "\n";
                    break;
                }
                case PRINT_DRYRUN: {
                    std::ofstream nullStream;
                    NodeContainerPrint(C, nullStream);
                    nullStream << "\n";
                    break;
                }
                case NONE: case CHECK: {
                    break;
                }
            }
            
            ParserSessionReleaseContainer(C);
            
            ParserSessionDeinit();
            
        } else if (mode == LEAF) {
            
            auto inputStr = reinterpret_cast<Buffer>(input.c_str());
            
            ParserSessionInit(inputStr, input.size(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
            
            auto stringifyMode = STRINGIFYMODE_NORMAL;
            
            auto C = ParserSessionConcreteParseLeaf(stringifyMode);
        
            switch (outputMode) {
                case PRINT: {
                    NodeContainerPrint(C, std::cout);
                    std::cout << "\n";
                    break;
                }
                case PRINT_DRYRUN: {
                    std::ofstream nullStream;
                    NodeContainerPrint(C, nullStream);
                    nullStream << "\n";
                    break;
                }
                case NONE: case CHECK: {
                    break;
                }
            }
            
            ParserSessionReleaseContainer(C);
            
            ParserSessionDeinit();
            
        } else {
            
            auto inputStr = reinterpret_cast<Buffer>(input.c_str());
            
            ParserSessionInit(inputStr, input.size(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
            
            auto C = ParserSessionParseExpressions();
            
            switch (outputMode) {
                case PRINT: {
                    NodeContainerPrint(C, std::cout);
                    std::cout << "\n";
                    break;
                }
                case PRINT_DRYRUN: {
                    std::ofstream nullStream;
                    NodeContainerPrint(C, nullStream);
                    nullStream << "\n";
                    break;
                }
                case CHECK: {
                    if (!NodeContainerCheck(C)) {
                        result = EXIT_FAILURE;
                        break;
                    }
                    
                    break;
                }
                case NONE: {
                    break;
                }
            }
            
            ParserSessionReleaseContainer(C);
            
            ParserSessionDeinit();
        }
        
    } // while (true)
    
    ParserSessionDestroy();
    
    return result;
}

int readFile(std::string file, APIMode mode, OutputMode outputMode, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode) {
    
    auto fb = ScopedFileBufferPtr(new ScopedFileBuffer(reinterpret_cast<Buffer>(file.c_str()), file.size()));

    if (fb->fail()) {
        switch (outputMode) {
            case PRINT: {
                std::cout << "file open failed\n";
                break;
            }
            case PRINT_DRYRUN: {
                break;
            }
            case NONE: case CHECK: {
                break;
            }
        }
        return EXIT_FAILURE;
    }
    
    WolframLibraryData libData = nullptr;
    
    ParserSessionCreate();
    
    int result = EXIT_SUCCESS;
    
    if (mode == TOKENIZE) {
        
        ParserSessionInit(fb->getBuf(), fb->getLen(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, encodingMode);
        
        auto C = ParserSessionTokenize();
        
        switch (outputMode) {
            case PRINT: {
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            }
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
                break;
            }
            case NONE: case CHECK: {
                break;
            }
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
        
    } else if (mode == LEAF) {
        
        ParserSessionInit(fb->getBuf(), fb->getLen(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, ENCODINGMODE_NORMAL);
        
        auto stringifyMode = STRINGIFYMODE_NORMAL;
        
        auto C = ParserSessionConcreteParseLeaf(stringifyMode);
    
        switch (outputMode) {
            case PRINT: {
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            }
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
                break;
            }
            case NONE: case CHECK: {
                break;
            }
        }
        
        ParserSessionReleaseContainer(C);
        
        ParserSessionDeinit();
        
    } else {
        
        ParserSessionInit(fb->getBuf(), fb->getLen(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, firstLineBehavior, ENCODINGMODE_NORMAL);
        
        auto C = ParserSessionParseExpressions();
        
        switch (outputMode) {
            case PRINT: {
                NodeContainerPrint(C, std::cout);
                std::cout << "\n";
                break;
            }
            case PRINT_DRYRUN: {
                std::ofstream nullStream;
                NodeContainerPrint(C, nullStream);
                nullStream << "\n";
                break;
            }
            case NONE: {
                break;
            }
            case CHECK: {
                if (!NodeContainerCheck(C)) {
                    result = EXIT_FAILURE;
                }
                
                break;
            }
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
