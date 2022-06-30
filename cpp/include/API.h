
#pragma once

#include "Source.h" // for BufferAndLength

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
// When targeting 12.0 as a minimum, then we can switch to using WSTP
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
#include <cstddef> // for size_t

class NodeContainer;

using NodeContainerPtr = NodeContainer *;


//
// The modes that stringifying could happen in
//
// Normal:
// Tokens are treated normally
//
// Tag:
// Stringify the next token as a tag:
// a::bcd
// a::"bcd"
// #abc
// #"abc"
//
// File:
// Stringify the next token as a file:
// << foo
// foo >> bar
// foo >>> bar
//
enum StringifyMode {
    STRINGIFYMODE_NORMAL = 0,
    STRINGIFYMODE_TAG = 1,
    STRINGIFYMODE_FILE = 2,
};

//
// Different encoding modes
//
// Normal
// generates issues that you would expect if coming from a file or a string
//
//
// Box
// Coming from a box, so some issues will be disabled
// These issues will be disabled:
// NonASCIICharacters
// Unexpected newline character: \[IndentingNewLine]
//
enum EncodingMode {
    ENCODINGMODE_NORMAL = 0,
    ENCODINGMODE_BOX = 1,
};

enum FirstLineBehavior {
    //
    // Source is a string or something, so if #! is on first line, then do not treat special
    //
    FIRSTLINEBEHAVIOR_NOTSCRIPT = 0,
    
    //
    // Source is something like .wl file that is being treated as a script
    // Or source is .wl file that is NOT being treated as a script
    // #! may be present, or it might not
    //
    FIRSTLINEBEHAVIOR_CHECK = 1,
    
    //
    // Source is a .wls file and there is definitely a #! on first line
    //
    FIRSTLINEBEHAVIOR_SCRIPT = 2,
};

enum UnsafeCharacterEncodingFlag {
    UNSAFECHARACTERENCODING_OK = 0,
    UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE = 1,
    UNSAFECHARACTERENCODING_STRAYSURROGATE = 2,
    UNSAFECHARACTERENCODING_BOM = 3,
};


EXTERN_C DLLEXPORT void ParserSessionCreate();
EXTERN_C DLLEXPORT void ParserSessionDestroy();

EXTERN_C DLLEXPORT void ParserSessionInit(Buffer buf,
                                          size_t bufLen,
                                          WolframLibraryData libData,
                                          SourceConvention srcConvention,
                                          uint32_t tabWidth,
                                          FirstLineBehavior firstLineBehavior,
                                          EncodingMode encodingMode);
EXTERN_C DLLEXPORT void ParserSessionDeinit();

EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionParseExpressions();
EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionTokenize();
EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionConcreteParseLeaf(StringifyMode mode);
EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionSafeString();
EXTERN_C DLLEXPORT void ParserSessionReleaseContainer(NodeContainerPtr C);

EXTERN_C DLLEXPORT void NodeContainerPrint(NodeContainerPtr C, std::ostream& s);
EXTERN_C DLLEXPORT int NodeContainerCheck(NodeContainerPtr C);


EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK mlp);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK mlp);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int SafeString_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int SafeString_LibraryLink(WolframLibraryData libData, MLINK mlp);
#endif // USE_EXPR_LIB

#if USE_MATHLINK
//
// A UTF8 String from MathLink that has lexical scope
//
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
#endif // USE_MATHLINK


#if USE_MATHLINK
//
// A String from MathLink that has lexical scope
//
class ScopedMLString {
    MLINK mlp;
    const char *buf;
    
public:
    
    ScopedMLString(MLINK mlp);
    
    ~ScopedMLString();
    
    bool read();
    
    const char *get() const;
};
#endif // USE_MATHLINK


#if USE_MATHLINK
//
// A Symbol from MathLink that has lexical scope
//
class ScopedMLSymbol {
    MLINK mlp;
    const char *sym;
    
public:
    
    ScopedMLSymbol(MLINK mlp);
    
    ~ScopedMLSymbol();
    
    bool read();
    
    const char *get() const;
};
#endif // USE_MATHLINK


#if USE_MATHLINK
//
// A Function from MathLink that has lexical scope
//
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
#endif // USE_MATHLINK


#if USE_MATHLINK
//
// A ByteArray from MathLink that has lexical scope
//
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
#endif // USE_MATHLINK


#if USE_MATHLINK
//
// An EnvironmentParameter from MathLink that has lexical scope
//
class ScopedMLEnvironmentParameter {
    
    MLEnvironmentParameter p;
    
public:
    ScopedMLEnvironmentParameter();
    
    ~ScopedMLEnvironmentParameter();
    
    MLEnvironmentParameter get();
};
#endif // USE_MATHLINK


#if USE_MATHLINK
//
// A Loopback Link from MathLink that has lexical scope
//
class ScopedMLLoopbackLink {
    
    MLINK mlp;
    MLENV ep;
    
public:
    ScopedMLLoopbackLink();
    
    ~ScopedMLLoopbackLink();
    
    MLINK get();
};
#endif // USE_MATHLINK


#if USE_EXPR_LIB
//
//
//
class ScopedNumericArray {
    
    WolframLibraryData libData;
    MNumericArray arr;
    
public:
    ScopedNumericArray(WolframLibraryData libData, MArgument Arg);
    
    ~ScopedNumericArray();
    
    size_t size() const;
    
    Buffer data() const;
};
#endif // USE_EXPR_LIB
