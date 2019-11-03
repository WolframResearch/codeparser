
#pragma once

#include "Source.h"

//
// Despite being mentioned here:
// language/LibraryLink/tutorial/LibraryStructure.html
//
// It is not actually possible to include "wstp.h" for use with
// WolframLibrary.h
//
// Using wstp.h results in errors like:
// error: unknown type name 'MLINK'
//
// The closest related bug is 357133
//
// This bug was fixed in v12.0
//
// When we no longer support any version < 12.0, then we can switch to using WSTP
//
// Also be a good citizen and cleanup the leftover defines from mathlink and WolframLibrary
//
#include "mathlink.h"

#undef P

#include "WolframLibrary.h"

#undef True

#undef False


#include <fstream>
#include <memory> // for unique_ptr

class Node;

//
// CMake defines ast_lib_EXPORTS
//
#ifdef _WIN32
# ifdef ast_lib_EXPORTS
#   define ASTLIB_EXPORTED  __declspec( dllexport )
# else
#   define ASTLIB_EXPORTED  __declspec( dllimport )
# endif
#else
# define ASTLIB_EXPORTED
#endif



EXTERN_C DLLEXPORT Node *ConcreteParseString(WolframLibraryData libData, const char *input, const char *style);

EXTERN_C DLLEXPORT Node *ConcreteParseFile(WolframLibraryData libData, const char *input, const char *style, const char *skipFirstLine);

EXTERN_C DLLEXPORT Node *TokenizeString(WolframLibraryData libData, const char *input, const char *style);

EXTERN_C DLLEXPORT Node *TokenizeFile(WolframLibraryData libData, const char *input, const char *style, const char *skipFirstLine);

EXTERN_C DLLEXPORT Node *ParseLeaf(WolframLibraryData libData, const char *input, const char *style, const char *stringifyNextTokenSymbol, const char *stringifyNextTokenFile);

EXTERN_C DLLEXPORT void ReleaseNode(Node *node);




EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ConcreteParseString_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeString_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeFile_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp);

class ScopedMLString {
    MLINK mlp;
    const char *str;
    int b;
    int c;
    
public:
    
    ScopedMLString(MLINK mlp) : mlp(mlp), str(NULL), b(), c() {}
    
    ~ScopedMLString() {
        if (str != NULL) {
            MLReleaseUTF8String(mlp, reinterpret_cast<const unsigned char *>(str), b);
        }
    }
    
    bool read() {
        return MLGetUTF8String(mlp, reinterpret_cast<const unsigned char **>(&str), &b, &c);
    }
    
    const char *get() const {
        return str;
    }
    
    size_t size() const {
        return b;
    }
};

class ScopedMLSymbol {
    MLINK mlp;
    const char *sym;
    
public:
    
    ScopedMLSymbol(MLINK mlp) : mlp(mlp), sym(NULL) {}
    
    ~ScopedMLSymbol() {
        if (sym != NULL) {
            MLReleaseSymbol(mlp, sym);
        }
    }
    
    bool read() {
        return MLGetSymbol(mlp, &sym);
    }
    
    const char *get() const {
        return sym;
    }
};

class ScopedMLFunction {
    MLINK mlp;
    const char *func;
    int a;
    
public:
    
    ScopedMLFunction(MLINK mlp) : mlp(mlp), func(NULL), a() {}
    
    ~ScopedMLFunction() {
        if (func != NULL) {
            MLReleaseSymbol(mlp, func);
        }
    }
    
    bool read() {
        return MLGetFunction(mlp, &func, &a);
    }
    
    const char *getHead() const {
        return func;
    }
    
    int getArgCount() const {
        return a;
    }
};

class ScopedIFS : public std::ifstream {
    
    char *data;
    size_t dataLength;
    
    bool inited;
    
public:
    
    ScopedIFS(const char *inStr) : std::ifstream(inStr, std::ifstream::in | std::ifstream::binary), data(), dataLength(), inited(false) {
        
        seekg(0, end);
    
        auto off = tellg();
        if (off == -1) {
            
            close();
            
            return;
        }
    
        dataLength = static_cast<size_t>(off);
    
        seekg(0, beg);
    
        data = new char[dataLength];
    
        read(data, dataLength);
        
        inited = true;
    }
    
    ~ScopedIFS() {
        
        if (inited) {
            return;
        }
        
        delete[] data;
        
        close();
    }
    
    char *getData() const {
        return data;
    }
    
    size_t getDataLength() const {
        return dataLength;
    }
    
};

class ScopedMLEnvironmentParameter {
    
    MLEnvironmentParameter p;
    
public:
    ScopedMLEnvironmentParameter() : p(MLNewParameters(MLREVISION, MLAPIREVISION)) {}
    
    ~ScopedMLEnvironmentParameter() {
        MLReleaseParameters(p);
    }
    
    MLEnvironmentParameter get() {
        return p;
    }
    
};

class ParserSession {
public:
    
    ParserSession();
    
    ~ParserSession();
    
    void init(WolframLibraryData libData, const char *data, size_t dataLen, SourceStyle style, bool stringifyNextTokenSymbol, bool stringifyNextTokenFile, bool skipFirstLine);
    
    void deinit();
};

extern std::unique_ptr<ParserSession> TheParserSession;


class MLSession {
    bool inited;
    MLENV ep;
    MLINK mlp;
    
public:
    
    MLSession();
    
    ~MLSession();
    
    MLINK getMLINK() const {
        return mlp;
    }
};
