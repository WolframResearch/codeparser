
#include "API.h"

#include "Symbol.h"

#include "mathlink.h"

#include <string>
#include <iostream>

int EXPRESSION = 0;
int TOKENIZE = 1;
int LEAF = 2;

std::string Style = "LineCol";

int readStdIn(int mode, bool printOutput);

int readFile(std::string file, int mode, bool printOutput);
void printExpression(MLINK mlp);

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
            return readFile(fileInput, LEAF, printOutput);
        } else if (tokenize) {
            return readFile(fileInput,TOKENIZE, printOutput);
        } else {
            return readFile(fileInput,EXPRESSION, printOutput);
        }
    } else {
        if (leaf) {
            return readStdIn(LEAF, printOutput);
        } else if (tokenize) {
            return readStdIn(TOKENIZE, printOutput);
        } else {
            return readStdIn(EXPRESSION, printOutput);
        }
    }
}

int readStdIn(int mode, bool printOutput) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);
    
    MLSession TheMLSession;
    
    ParserSession TheParserSession;
    
    auto mlp = TheMLSession.getMLINK();
    
    if (mode == TOKENIZE) {
        
        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 2)) {
            return res;
        }
        if (!MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(input.c_str()), static_cast<int>(input.size()))) {
            return res;
        }
        if (!MLPutString(mlp, Style.c_str())) {
            return res;
        }
        
        res = TokenizeString_LibraryLink(nullptr, mlp);
        
    } else if (mode == LEAF) {
        
        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 2)) {
            return res;
        }
        if (!MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(input.c_str()), static_cast<int>(input.size()))) {
            return res;
        }
        if (!MLPutString(mlp, Style.c_str())) {
            return res;
        }
        
        res = ParseLeaf_LibraryLink(nullptr, mlp);
        
    } else {
        
        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 2)) {
            return res;
        }
        if (!MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(input.c_str()), static_cast<int>(input.size()))) {
            return res;
        }
        if (!MLPutString(mlp, Style.c_str())) {
            return res;
        }
        
        res = ConcreteParseString_LibraryLink(nullptr, mlp);
    }
    
    if (res != LIBRARY_NO_ERROR) {
        return res;
    }
    
    if (printOutput) {
        printExpression(mlp);
        std::cout << "\n";
    }
    
    return res;
}

int readFile(std::string file, int mode, bool printOutput) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    
    MLSession TheMLSession;
    
    ParserSession TheParserSession;
    
    auto mlp = TheMLSession.getMLINK();
    
    if (mode == TOKENIZE) {
        
        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 3)) {
            return res;
        }
        if (!MLPutString(mlp, file.c_str())) {
            return res;
        }
        if (!MLPutSymbol(mlp, "False")) {
            return res;
        }
        if (!MLPutString(mlp, Style.c_str())) {
            return res;
        }
        
        res = TokenizeFile_LibraryLink(nullptr, mlp);
        
    } else {
        
        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 3)) {
            return res;
        }
        if (!MLPutString(mlp, file.c_str())) {
            return res;
        }
        if (!MLPutSymbol(mlp, "False")) {
            return res;
        }
        if (!MLPutString(mlp, Style.c_str())) {
            return res;
        }
        
        res = ConcreteParseFile_LibraryLink(nullptr, mlp);
        
    }
    if (res != LIBRARY_NO_ERROR) {
        return res;
    }
    
    if (printOutput) {
        printExpression(mlp);
        std::cout << "\n";
    }
    
    return res;
}

void printExpression(MLINK mlp) {
    
    auto ready = MLReady(mlp);
    if (!ready) {
        return;
    }

    switch(MLGetType(mlp)) {
        case MLTKINT: {
            int i;
            if (!MLGetInteger(mlp, &i)) {
                return;
            }
            std::cout << i;
        }
            break;
        case MLTKREAL: {
            double r;
            if (!MLGetReal(mlp, &r)) {
                return;
            }
            std::cout << r;
        }
            break;
        case MLTKSTR: {
            ScopedMLString string(mlp);
            if (!string.read()) {
                return;
            }
            std::cout << string.get();
        }
            break;
        case MLTKSYM: {
            ScopedMLSymbol symbol(mlp);
            if (!symbol.read()) {
                return;
            }
            std::cout << symbol.get();
        }
            break;
        case MLTKFUNC: {
            ScopedMLFunction func(mlp);
            if (!func.read()) {
                return;
            }
            std::cout << func.getHead() << "[";
            auto a = func.getArgCount();
            if (a > 0) {
                for (int i = 0; i < a-1; i++) {
                    printExpression(mlp);
                    std::cout << ", ";
                }
                printExpression(mlp);
            }
            std::cout << "]";
        }
            break;
        default: {
            auto err = MLError(mlp);
            std::cout << "\nerr: " << err << "\n";
        }
            break;
    }
}

