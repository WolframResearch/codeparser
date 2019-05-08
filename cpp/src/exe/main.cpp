
#include "API.h"

#include "Symbol.h"

#include "mathlink.h"

#include <string>
#include <iostream>

int readStdIn(bool tokenize);
int readFile(std::string file);
void printExpression(MLINK mlp);

int main(int argc, char *argv[]) {
    
    auto tokenize = false;
    for (int i = 1; i < argc; i++) {
        auto arg = std::string(argv[i]);
        if (arg == "-file") {
            i++;
            auto file = std::string(argv[i]);

            return readFile(file);

        } else if (arg == "-tokenize") {
            
            tokenize = true;
            
        } else {
            return 1;
        }
    }

    return readStdIn(tokenize);
}

int readStdIn(bool tokenize) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);
    
    MLENV ep;
    MLEnvironmentParameter p;
    int err;
    
    p = MLNewParameters(MLREVISION, MLAPIREVISION);
#ifdef WINDOWS_MATHLINK

#else
    //
    // Needed because MathLink intercepts all signals
    //
    MLDoNotHandleSignalParameter(p, SIGINT);
#endif
    ep = MLInitialize(p);
    if (ep == (MLENV)0) {
        return 1;
    }
    
    WolframLibrary_initialize(nullptr);
    
    MLINK mlp;
    mlp = MLLoopbackOpen(ep, &err);
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 1)) {
        goto retPt;
    }
    if (!MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(input.c_str()), static_cast<int>(input.size()))) {
        goto retPt;
    }
    
    if (tokenize) {
        res = TokenizeString(nullptr, mlp);
    } else {
        res = ConcreteParseString(nullptr, mlp);
    }
    
    if (res != LIBRARY_NO_ERROR) {
        goto retPt;
    }
    
    printExpression(mlp);
    std::cout << "\n";
    
retPt:
    if (mlp != nullptr) {
        MLClose(mlp);
    }
    if (ep != 0) {
        MLDeinitialize(ep);
    }
    WolframLibrary_uninitialize(nullptr);
    
    return res;
}

int readFile(std::string file) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    
    MLENV ep;
    MLEnvironmentParameter p;
    int err;
    
    p = MLNewParameters(MLREVISION, MLAPIREVISION);
#ifdef WINDOWS_MATHLINK

#else
    //
    // Needed because MathLink intercepts all signals
    //
    MLDoNotHandleSignalParameter(p, SIGINT);
#endif
    ep = MLInitialize(p);
    if (ep == (MLENV)0) {
        return 1;
    }
    
    WolframLibrary_initialize(nullptr);
    
    MLINK mlp;
    mlp = MLLoopbackOpen(ep, &err);
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 2)) {
        goto retPt;
    }
    if (!MLPutString(mlp, file.c_str())) {
        goto retPt;
    }
    if (!MLPutSymbol(mlp, "False")) {
        goto retPt;
    }
    
    res = ConcreteParseFile(nullptr, mlp);
    if (res != LIBRARY_NO_ERROR) {
        goto retPt;
    }
    
    printExpression(mlp);
    std::cout << "\n";
    
retPt:
    if (mlp != nullptr) {
        MLClose(mlp);
    }
    if (ep != 0) {
        MLDeinitialize(ep);
    }
    WolframLibrary_uninitialize(nullptr);
    
    return res;
}

void printExpression(MLINK mlp) {
    int i;
    double r;
    const unsigned char *string;
    int bytes;
    int chars;
    const char *symbol;
    const char *func;
    int a;
    int ready;
    int err = 0;
    
    ready = MLReady(mlp);
    if (!ready) {
        return;
    }

    switch(MLGetType(mlp)) {
        case MLTKINT:
            if (!MLGetInteger(mlp, &i)) {
                return;
            }
            std::cout << i;
            break;
        case MLTKREAL:
            if (!MLGetReal(mlp, &r)) {
                return;
            }
            std::cout << r;
            break;
        case MLTKSTR:
            if (!MLGetUTF8String(mlp, &string, &bytes, &chars)) {
                return;
            }
            std::cout << string;
            MLReleaseUTF8String(mlp, string, bytes);
            break;
        case MLTKSYM:
            if (!MLGetSymbol(mlp, &symbol)) {
                return;
            }
            std::cout << symbol;
            MLReleaseSymbol(mlp, symbol);
            break;
        case MLTKFUNC:
            if (!MLGetFunction(mlp, &func, &a)) {
                return;
            }
            std::cout << func << "[";
            if (a > 0) {
                for (int i = 0; i < a-1; i++) {
                    printExpression(mlp);
                    std::cout << ", ";
                }
                printExpression(mlp);
            }
            std::cout << "]";
            MLReleaseSymbol(mlp, func);
            break;
        default:
            err = MLError(mlp);
            std::cout << "\nerr: " << err << "\n";
            break;
    }
}

