
#include "Node.h"
#include "API.h"
#include "Symbol.h"

#include "mathlink.h"

#include <string>
#include <iostream>

int EXPRESSION = 0;
int TOKENIZE = 1;
int LEAF = 2;

std::string Style = "LineCol";

void readStdIn(int mode, bool printOutput);

void readFile(std::string file, int mode, bool printOutput);
//void printExpression(MLINK mlp);
//void printExpression(Node *);

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
            readFile(fileInput,TOKENIZE, printOutput);
        } else {
            readFile(fileInput,EXPRESSION, printOutput);
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
    
//    int res = LIBRARY_FUNCTION_ERROR;
    
    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);
    
//    MLSession TheMLSession;
    
    ParserSession TheParserSession;
    
//    auto mlp = TheMLSession.getMLINK();
    
    WolframLibraryData libData = nullptr;
    
    Node *N;
    
    if (mode == TOKENIZE) {
        
//        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 2)) {
//            return res;
//        }
//        if (!MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(input.c_str()), static_cast<int>(input.size()))) {
//            return res;
//        }
//        if (!MLPutString(mlp, Style.c_str())) {
//            return res;
//        }
        
        N = TokenizeString(libData, input.c_str(), Style.c_str());
        
    } else if (mode == LEAF) {
        
//        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 4)) {
//            return res;
//        }
//        if (!MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(input.c_str()), static_cast<int>(input.size()))) {
//            return res;
//        }
//        if (!MLPutString(mlp, Style.c_str())) {
//            return res;
//        }
//        if (!MLPutSymbol(mlp, "False")) {
//            return res;
//        }
//        if (!MLPutSymbol(mlp, "False")) {
//            return res;
//        }
        
        N = ParseLeaf(libData, input.c_str(), Style.c_str(), "False", "False");
        
    } else {
        
//        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 2)) {
//            return res;
//        }
//        if (!MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(input.c_str()), static_cast<int>(input.size()))) {
//            return res;
//        }
//        if (!MLPutString(mlp, Style.c_str())) {
//            return res;
//        }
        
        N = ConcreteParseString(libData, input.c_str(), Style.c_str());
    }
    
    if (printOutput) {
        N->print(std::cout);
        std::cout << "\n";
    }
    
//    return res;
}

void readFile(std::string file, int mode, bool printOutput) {
    
//    int res = LIBRARY_FUNCTION_ERROR;
    
//    MLSession TheMLSession;
    
    ParserSession TheParserSession;
    
//    auto mlp = TheMLSession.getMLINK();
    
    WolframLibraryData libData = nullptr;
    
    Node *N;
    
    if (mode == TOKENIZE) {
        
//        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 3)) {
//            return res;
//        }
//        if (!MLPutString(mlp, file.c_str())) {
//            return res;
//        }
//        if (!MLPutString(mlp, Style.c_str())) {
//            return res;
//        }
//        if (!MLPutSymbol(mlp, "False")) {
//            return res;
//        }
        
        N = TokenizeFile(libData, file.c_str(), Style.c_str(), "False");
        
    } else {
        
//        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 3)) {
//            return res;
//        }
//        if (!MLPutString(mlp, file.c_str())) {
//            return res;
//        }
//        if (!MLPutString(mlp, Style.c_str())) {
//            return res;
//        }
//        if (!MLPutSymbol(mlp, "False")) {
//            return res;
//        }
        
        N = ConcreteParseFile(libData, file.c_str(), Style.c_str(), "False");
        
    }
    
    if (printOutput) {
        N->print(std::cout);
        std::cout << "\n";
    }
    
//    return res;
}

//void printExpression(Node *N) {
//
//    auto ready = MLReady(mlp);
//    if (!ready) {
//        return;
//    }
//
//    switch(MLGetType(mlp)) {
//        case MLTKINT: {
//            int i;
//            if (!MLGetInteger(mlp, &i)) {
//                return;
//            }
//            std::cout << i;
//        }
//            break;
//        case MLTKREAL: {
//            double r;
//            if (!MLGetReal(mlp, &r)) {
//                return;
//            }
//            std::cout << r;
//        }
//            break;
//        case MLTKSTR: {
//            ScopedMLString string(mlp);
//            if (!string.read()) {
//                return;
//            }
//            std::cout << string.get();
//        }
//            break;
//        case MLTKSYM: {
//            ScopedMLSymbol symbol(mlp);
//            if (!symbol.read()) {
//                return;
//            }
//            std::cout << symbol.get();
//        }
//            break;
//        case MLTKFUNC: {
//            ScopedMLFunction func(mlp);
//            if (!func.read()) {
//                return;
//            }
//            std::cout << func.getHead() << "[";
//            auto a = func.getArgCount();
//            if (a > 0) {
//                for (int i = 0; i < a-1; i++) {
//                    printExpression(mlp);
//                    std::cout << ", ";
//                }
//                printExpression(mlp);
//            }
//            std::cout << "]";
//        }
//            break;
//        default: {
//            auto err = MLError(mlp);
//            std::cout << "\nerr: " << err << "\n";
//        }
//            break;
//    }
//}

