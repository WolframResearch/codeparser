
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



EXTERN_C DLLEXPORT Node *ConcreteParseString(WolframLibraryData libData, const unsigned char *input, size_t len, const char *style);

EXTERN_C DLLEXPORT Node *ConcreteParseFile(WolframLibraryData libData, const unsigned char *input, size_t len, const char *style);

EXTERN_C DLLEXPORT Node *TokenizeString(WolframLibraryData libData, const unsigned char *input, size_t len, const char *style);

EXTERN_C DLLEXPORT Node *TokenizeFile(WolframLibraryData libData, const unsigned char *input, size_t len, const char *style);

EXTERN_C DLLEXPORT Node *ParseLeaf(WolframLibraryData libData, const unsigned char *input, size_t len, const char *style, const char *stringifyNextTokenSymbol, const char *stringifyNextTokenFile);

EXTERN_C DLLEXPORT void ReleaseNode(Node *node);




EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ConcreteParseString_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeString_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeFile_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp);

class MLSession {
    bool inited;
    MLENV ep;
    MLINK mlp;
    
public:
    
    MLSession();
    
    ~MLSession();
    
    MLINK getMLINK() const;
};

class ScopedMLUTF8String {
    MLINK mlp;
    const unsigned char *buf;
    int b;
    int c;
    
public:
    
    ScopedMLUTF8String(MLINK mlp);
    
    ~ScopedMLUTF8String();
    
    bool read();
    
    const unsigned char *get() const;
    
    size_t getByteCount() const;
};

class ScopedMLString {
    MLINK mlp;
    const char *buf;
    
public:
    
    ScopedMLString(MLINK mlp);
    
    ~ScopedMLString();
    
    bool read();
    
    const char *get() const;
};

class ScopedMLSymbol {
    MLINK mlp;
    const char *sym;
    
public:
    
    ScopedMLSymbol(MLINK mlp);
    
    ~ScopedMLSymbol();
    
    bool read();
    
    const char *get() const;
};

class ScopedMLFunction {
    MLINK mlp;
    const char *func;
    int count;
    
public:
    
    ScopedMLFunction(MLINK mlp);
    
    ~ScopedMLFunction();
    
    bool read();
    
    const char *getHead() const;
    
    int getArgCount() const;
};

class ScopedMLEnvironmentParameter {
    
    MLEnvironmentParameter p;
    
public:
    ScopedMLEnvironmentParameter();
    
    ~ScopedMLEnvironmentParameter();
    
    MLEnvironmentParameter get();
    
};

class ScopedFileBuffer {
    
    unsigned char *buf;
    size_t len;
    
    bool inited;
    
public:
    
    ScopedFileBuffer(const unsigned char *inStrIn, size_t inLen);
    
    ~ScopedFileBuffer();
    
    unsigned char *getBuf() const;
    
    size_t getLen() const;
    
    bool fail() const;
    
};

class ParserSession {
public:
    
    ParserSession();
    
    ~ParserSession();
    
    void init(WolframLibraryData libData, const unsigned char *data, size_t dataLen, SourceStyle style, bool stringifyNextTokenSymbol, bool stringifyNextTokenFile);
    
    void deinit();
};

extern std::unique_ptr<ParserSession> TheParserSession;
