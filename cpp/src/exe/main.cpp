
#include "API.h"

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS

#include <iostream>
#include <fstream> // for ofstream
#include <cstdio> // for rewind
#include <cstdlib> // for EXIT_SUCCESS
#include <cstddef> // for size_t


enum APIMode {
    EXPRESSION,
    TOKENIZE,
    LEAF,
    SAFESTRING,
};

enum OutputMode {
    NONE,
    PRINT,
    PRINT_DRYRUN,
    CHECK,
};


int readStdIn(APIMode mode, OutputMode outputMode, ParserSessionOptions opts);

int readFile(std::string file, APIMode mode, OutputMode outputMode, ParserSessionOptions opts);


int main(int argc, char *argv[]) {
    
    auto file = false;
    auto tokenize = false;
    auto leaf = false;
    auto safeString = false;
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
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = encodingMode;
    
    int result;
    
    if (file) {
        if (leaf) {
            result = readFile(fileInput, LEAF, outputMode, opts);
        } else if (tokenize) {
            result = readFile(fileInput, TOKENIZE, outputMode, opts);
        } else if (safeString) {
            result = readFile(fileInput, SAFESTRING, outputMode, opts);
        } else {
            result = readFile(fileInput, EXPRESSION, outputMode, opts);
        }
    } else {
        if (leaf) {
            result = readStdIn(LEAF, outputMode, opts);
        } else if (tokenize) {
            result = readStdIn(TOKENIZE, outputMode, opts);
        } else if (safeString) {
            result = readStdIn(SAFESTRING, outputMode, opts);
        } else {
            result = readStdIn(EXPRESSION, outputMode, opts);
        }
    }
    
    return result;
}

int readStdIn(APIMode mode, OutputMode outputMode, ParserSessionOptions opts) {
    
    opts.alreadyHasEOFSentinel = false;
    
    WolframLibraryData libData = nullptr;
    
    auto session = CreateParserSession();
    
    int result = EXIT_SUCCESS;
    
    while (true) {
        
        std::string input;
        std::cout << ">>> ";
        std::getline(std::cin, input);
        
        if (mode == TOKENIZE) {
            
            auto inputStr = reinterpret_cast<Buffer>(input.c_str());
            
            ParserSessionInit(session, inputStr, input.size(), libData, opts);
        
            auto C = ParserSessionTokenize(session);
            
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
            
            ParserSessionReleaseNodeContainer(session, C);
            
            ParserSessionDeinit(session);
            
        } else if (mode == LEAF) {
            
            auto inputStr = reinterpret_cast<Buffer>(input.c_str());
            
            ParserSessionInit(session, inputStr, input.size(), libData, opts);
            
            auto stringifyMode = STRINGIFYMODE_NORMAL;
            
            auto C = ParserSessionConcreteParseLeaf(session, stringifyMode);
        
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
            
            ParserSessionReleaseNodeContainer(session, C);
            
            ParserSessionDeinit(session);
            
#if DIAGNOSTICS
            DiagnosticsPrint();
#endif // DIAGNOSTICS
            
        } else if (mode == SAFESTRING) {
            
            auto inputStr = reinterpret_cast<Buffer>(input.c_str());
            
            ParserSessionInit(session, inputStr, input.size(), libData, opts);
            
            auto C = ParserSessionSafeString(session);
        
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
            
            ParserSessionReleaseNodeContainer(session, C);
            
            ParserSessionDeinit(session);
            
#if DIAGNOSTICS
            DiagnosticsPrint();
#endif // DIAGNOSTICS
            
        } else {
            
            auto inputStr = reinterpret_cast<Buffer>(input.c_str());
            
            ParserSessionInit(session, inputStr, input.size(), libData, opts);
            
            auto C = ParserSessionParseExpressions(session);
            
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
            
            ParserSessionReleaseNodeContainer(session, C);
            
            ParserSessionDeinit(session);
            
#if DIAGNOSTICS
            DiagnosticsPrint();
#endif // DIAGNOSTICS
        }
        
    } // while (true)
    
    DestroyParserSession(session);
    
    return result;
}

int readFile(std::string file, APIMode mode, OutputMode outputMode, ParserSessionOptions opts) {
    
    auto fb = ScopedFileBuffer(reinterpret_cast<Buffer>(file.c_str()), file.size());

    if (fb.fail()) {
        
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
    
    opts.alreadyHasEOFSentinel = true;
    
    WolframLibraryData libData = nullptr;
    
    auto session = CreateParserSession();
    
    int result = EXIT_SUCCESS;
    
    if (mode == TOKENIZE) {
        
        ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts);
        
        auto C = ParserSessionTokenize(session);
        
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
        
        ParserSessionReleaseNodeContainer(session, C);
        
        ParserSessionDeinit(session);
        
#if DIAGNOSTICS
        DiagnosticsPrint();
#endif // DIAGNOSTICS
        
    } else if (mode == LEAF) {
        
        ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts);
        
        auto stringifyMode = STRINGIFYMODE_NORMAL;
        
        auto C = ParserSessionConcreteParseLeaf(session, stringifyMode);
    
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
        
        ParserSessionReleaseNodeContainer(session, C);
        
        ParserSessionDeinit(session);
        
#if DIAGNOSTICS
        DiagnosticsPrint();
#endif // DIAGNOSTICS
        
    } else if (mode == SAFESTRING) {
        
        ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts);
        
        auto C = ParserSessionSafeString(session);
    
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
        
        ParserSessionReleaseNodeContainer(session, C);
        
        ParserSessionDeinit(session);
        
#if DIAGNOSTICS
        DiagnosticsPrint();
#endif // DIAGNOSTICS
        
    } else {
        
        ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts);
        
        auto C = ParserSessionParseExpressions(session);
        
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
        
        ParserSessionReleaseNodeContainer(session, C);
        
        ParserSessionDeinit(session);
        
#if DIAGNOSTICS
        DiagnosticsPrint();
#endif // DIAGNOSTICS
    }
    
    DestroyParserSession(session);
    
    return result;
}
