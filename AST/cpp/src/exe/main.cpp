
#include "API.h"

// #include "Parser.h"
// #include "Precedence.h"
// #include "ByteDecoder.h"
// #include "ByteEncoder.h"
// #include "CharacterDecoder.h"
// #include "Tokenizer.h"

#include "mathlink.h"

#include <string>
// #include <sstream>
#include <iostream>
// #include <fstream>
// #include <cassert>

int main(int argc, char *argv[]) {

    std::string input;
    std::cout << ">>> ";
    std::getline(std::cin, input);

    MLENV ep;
    int err;

    ep = MLInitialize((MLEnvironmentParameter)0);
    if (ep == (MLENV)0) {
        return 1;
    }

    auto mlp = MLLoopbackOpen(ep, &err);

    // std::cout << err << "\n";

    // MLPutFunction(mlp, "Power", 2);
    //   MLPutSymbol(mlp, "x");
    //   MLPutInteger32(mlp, 3);

    MLPutFunction(mlp, "List", 2);
    MLPutString(mlp, input.c_str());
    MLPutSymbol(mlp, "False");
    
    ConcreteParseString(nullptr, mlp);

    const char *head;
    int n;
    const char *sname;
    int k;

    MLGetFunction(mlp, &head, &n);
      MLGetSymbol(mlp, &sname);
      MLGetInteger32(mlp, &k);

    MLClose(mlp);
    MLDeinitialize(ep);

    std::cout << head << "\n";
    std::cout << sname << "\n";
    std::cout << k << "\n";

    return 0;
}


