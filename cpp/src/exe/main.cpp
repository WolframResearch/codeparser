
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


int readStdIn(APIMode mode, OutputMode outputMode);

int readFile(std::string file, APIMode mode, OutputMode outputMode);


int main(int argc, char *argv[]) {
    
    auto file = false;
    auto apiMode = EXPRESSION;
    auto outputMode = PRINT;
    
    std::string fileInput;
    
    for (int i = 1; i < argc; i++) {
        
        auto arg = std::string(argv[i]);
        
        if (arg == "-file") {
            
            file = true;
            
            i++;
            fileInput = std::string(argv[i]);

        } else if (arg == "-tokenize") {
            
            apiMode = TOKENIZE;
            
        } else if (arg == "-leaf") {
            
            apiMode = LEAF;
            
        } else if (arg == "-safestring") {
            
            apiMode = SAFESTRING;
            
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
        
        result = readFile(fileInput, apiMode, outputMode);
        
    } else {
        
        result = readStdIn(apiMode, outputMode);
    }
    
    return result;
}

int readStdIn(APIMode mode, OutputMode outputMode) {
    
    ParserSessionPtr session;
    
    if (CreateParserSession(&session)) {
        return EXIT_FAILURE;
    }
    
    while (true) {
        
        std::string input;
        std::cout << ">>> ";
        std::getline(std::cin, input);
        
        if (ParserSessionInitSimple(session, reinterpret_cast<Buffer>(input.c_str()), input.size(), 0)) {
            return EXIT_FAILURE;
        }
        
        NodePtr N;
        
        switch (mode) {
            case TOKENIZE: {
                
                if (ParserSessionTokenize(session, &N)) {
                    return EXIT_FAILURE;
                }
                
                break;
            }
            case LEAF: {
                
                if (ParserSessionConcreteParseLeaf(session, STRINGIFYMODE_NORMAL, &N)) {
                    return EXIT_FAILURE;
                }
                
                break;
            }
            case SAFESTRING: {
                
                if (ParserSessionSafeString(session, &N)) {
                    return EXIT_FAILURE;
                }
                
                break;
            }
            default: {
                
                if (ParserSessionConcreteParse(session, &N)) {
                    return EXIT_FAILURE;
                }
                
                break;
            }
        }
        
        switch (outputMode) {
            case PRINT: {
                
                NodePrint(N, std::cout);
                
                std::cout << "\n";
                
                break;
            }
            case PRINT_DRYRUN: {
                
                std::ofstream nullStream;
                
                NodePrint(N, nullStream);
                
                nullStream << "\n";
                
                break;
            }
            case NONE: case CHECK: {
                break;
            }
        }
        
        ParserSessionReleaseNode(session, N);
        
        ParserSessionDeinit(session);
        
#if DIAGNOSTICS
        DiagnosticsPrint();
#endif // DIAGNOSTICS
        
    } // while (true)
    
    DestroyParserSession(session);
    
    return EXIT_SUCCESS;
}

int readFile(std::string file, APIMode mode, OutputMode outputMode) {
    
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
    
    ParserSessionPtr session;
    
    if (CreateParserSession(&session)) {
        return EXIT_FAILURE;
    }
    
    if (ParserSessionInitSimple(session, fb.getBuf(), fb.getLen(), 1)) {
        return EXIT_FAILURE;
    }
    
    NodePtr N;
    
    switch (mode) {
        case TOKENIZE: {
            
            if (ParserSessionTokenize(session, &N)) {
                return EXIT_FAILURE;
            }
            
            break;
        }
        case LEAF: {
            
            if (ParserSessionConcreteParseLeaf(session, STRINGIFYMODE_NORMAL, &N)) {
                return EXIT_FAILURE;
            }
            
            break;
        }
        case SAFESTRING: {
            
            if (ParserSessionSafeString(session, &N)) {
                return EXIT_FAILURE;
            }
            
            break;
        }
        default: {
            
            if (ParserSessionConcreteParse(session, &N)) {
                return EXIT_FAILURE;
            }
            
            break;
        }
    }
    
    switch (outputMode) {
        case PRINT: {
            
            NodePrint(N, std::cout);
            
            std::cout << "\n";
            
            break;
        }
        case PRINT_DRYRUN: {
            
            std::ofstream nullStream;
            
            NodePrint(N, nullStream);
            
            nullStream << "\n";
            
            break;
        }
        case NONE: case CHECK: {
            break;
        }
    }
    
    ParserSessionReleaseNode(session, N);
    
    ParserSessionDeinit(session);
    
#if DIAGNOSTICS
    DiagnosticsPrint();
#endif // DIAGNOSTICS
    
    DestroyParserSession(session);
    
    return EXIT_SUCCESS;
}
