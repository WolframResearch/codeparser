
#pragma once

#include "Node.h" // for NodePtr, Node, etc.
#include "Source.h" // for BufferAndLength
#include "ExpressionLibrary.h" // for expr

//
// Despite being mentioned here:
// language/LibraryLink/tutorial/LibraryStructure.html
//
// It is not actually possible to include "wstp.h" for use with WolframLibrary.h
//
// Using wstp.h results in errors like:
// error: unknown type name 'MLINK'
//
// Related bugs: 357133
//
// This bug was fixed in v12.0
//
// When we no longer support any version < 12.0, then we can switch to using WSTP
//
// Also be a good citizen and cleanup the leftover defines from mathlink and WolframLibrary
//
#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include "WolframLibrary.h"
#undef True
#undef False

#include <memory> // for unique_ptr
#include <functional> // for function with GCC and MSVC

class ParserSession;
class ScopedMLByteArray;
class ScopedMLUTF8String;
class ScopedMLString;
class ScopedMLEnvironmentParameter;

using ParserSessionPtr = std::unique_ptr<ParserSession>;
using ScopedMLByteArrayPtr = std::unique_ptr<ScopedMLByteArray>;
using ScopedMLUTF8StringPtr = std::unique_ptr<ScopedMLUTF8String>;
using ScopedMLStringPtr = std::unique_ptr<ScopedMLString>;
using ScopedMLEnvironmentParameterPtr = std::unique_ptr<ScopedMLEnvironmentParameter>;

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


enum StringifyMode {
    STRINGIFYMODE_NORMAL,
    STRINGIFYMODE_SYMBOL,
    STRINGIFYMODE_FILE,
};

enum ParserSessionBits : uint8_t {
    
    //
    //
    //
    INCLUDE_SOURCE = 0x01,
};

using ParserSessionPolicy = uint8_t;


class ParserSession {
    
    BufferAndLength bufAndLen;
    
    NodePtr parseLeaf0(int mode);
    
public:
    
#if !NABORT
    std::function<bool ()> currentAbortQ;
#endif // !NABORT
    
    ParserSessionPolicy policy;
    
    
    ParserSession();
    
    ~ParserSession();
    
    void init(BufferAndLength bufAndLen, WolframLibraryData libData, ParserSessionPolicy policy);
    
    void deinit();
    
    
    Node *parseExpressions();
    Node *tokenize();
    Node *listSourceCharacters();
    Node *parseLeaf(StringifyMode mode);
    
    void releaseNode(Node *N);
    
#if !NABORT
    bool isAbort() const;
    
    NodePtr handleAbort() const;
#endif // !NABORT
};

extern ParserSessionPtr TheParserSession;


EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);


#if USE_MATHLINK

EXTERN_C DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeBytes_Listable_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int SafeString_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int SetupLongNames_LibraryLink(WolframLibraryData libData, MLINK mlp);

class ScopedMLUTF8String {
    MLINK mlp;
    Buffer buf;
    int b;
    int c;
    
public:
    
    ScopedMLUTF8String(MLINK mlp);
    
    ~ScopedMLUTF8String();
    
    bool read();
    
    Buffer get() const;
    
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

class ScopedMLByteArray {
    MLINK mlp;
    MBuffer buf;
    int *dims;
    char **heads;
    int depth;
    
public:
    
    ScopedMLByteArray(MLINK mlp);
    
    ~ScopedMLByteArray();
    
    bool read();
    
    Buffer get() const;
    
    size_t getByteCount() const;
};

class ScopedMLEnvironmentParameter {
    
    MLEnvironmentParameter p;
    
public:
    ScopedMLEnvironmentParameter();
    
    ~ScopedMLEnvironmentParameter();
    
    MLEnvironmentParameter get();
    
};

class ScopedMLLoopbackLink {
    
    MLINK mlp;
    MLENV ep;
    
public:
    ScopedMLLoopbackLink();
    
    ~ScopedMLLoopbackLink();
    
    MLINK get();
    
};

#endif // USE_MATHLINK
