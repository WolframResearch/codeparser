
#include "API.h"

#include "Symbol.h"

#include "mathlink.h"

#include <string>
#include <iostream>

void printExpression(MLINK mlp);

int main(int argc, char *argv[]) {

    int res = LIBRARY_FUNCTION_ERROR;
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);

    MLENV ep;
    MLEnvironmentParameter p;
    int err;

    p = MLNewParameters(MLREVISION, MLAPIREVISION);

    MLDoNotHandleSignalParameter(p, SIGINT);

    ep = MLInitialize(p);
    if (ep == (MLENV)0) {
        return 1;
    }

    MLINK mlp;
    mlp = MLLoopbackOpen(ep, &err);

    MLPutFunction(mlp, SYMBOL_LIST.name().c_str(), 1);
    MLPutString(mlp, input.c_str());
    
    res = ConcreteParseString(nullptr, mlp);
    if (res != LIBRARY_NO_ERROR) {
        std::cerr << "library error: " << res << "\n";
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
    
    return res;
}

void printExpression(MLINK mlp) {
    int i;
    double r;
    const char *string;
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
            MLGetInteger(mlp, &i);
            std::cout << i;
            break;
        case MLTKREAL:
            MLGetReal(mlp, &r);
            std::cout << r;
            break;
        case MLTKSTR:
            MLGetString(mlp, &string);
            std::cout << string;
            MLReleaseString(mlp, string);
            break;
        case MLTKSYM:
            MLGetSymbol(mlp, &symbol);
            std::cout << symbol;
            MLReleaseSymbol(mlp, symbol);
            break;
        case MLTKFUNC:
            MLGetFunction(mlp, &func, &a);
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

