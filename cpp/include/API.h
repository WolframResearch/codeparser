
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

#include <cstddef> // for size_t

class NodeContainer;
class ParserSession;

using NodeContainerPtr = NodeContainer *;
using ParserSessionPtr = ParserSession *;

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


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


EXTERN_C DLLEXPORT ParserSessionPtr CreateParserSession();
EXTERN_C DLLEXPORT void DestroyParserSession(ParserSessionPtr session);

EXTERN_C DLLEXPORT int ParserSessionInit(ParserSessionPtr session, Buffer buf, size_t bufLen, WolframLibraryData libData, SourceConvention srcConvention, uint32_t tabWidth, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode);
EXTERN_C DLLEXPORT void ParserSessionDeinit(ParserSessionPtr session);

EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionParseExpressions(ParserSessionPtr session);
EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionTokenize(ParserSessionPtr session);
EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionConcreteParseLeaf(ParserSessionPtr session, StringifyMode mode);
EXTERN_C DLLEXPORT NodeContainerPtr ParserSessionSafeString(ParserSessionPtr session);
EXTERN_C DLLEXPORT void ParserSessionReleaseNodeContainer(ParserSessionPtr session, NodeContainerPtr C);

EXTERN_C DLLEXPORT int NodeContainerCheck(NodeContainerPtr C);

DLLEXPORT void NodeContainerPrint(NodeContainerPtr C, std::ostream& s);

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT expr NodeContainerToExpr(ParserSessionPtr session, NodeContainerPtr C);
#endif // USE_EXPR_LIB

#if USE_MATHLINK
EXTERN_C DLLEXPORT void NodeContainerPut(ParserSessionPtr session, NodeContainerPtr C);
#endif // USE_MATHLINK


EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int CreateParserSession_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int CreateParserSession_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int DestroyParserSession_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int DestroyParserSession_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int SafeString_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int SafeString_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB


//
//
//
class ScopedFileBuffer {
private:
    
    MBuffer buf;
    size_t len;

    bool inited;

public:

    DLLEXPORT ScopedFileBuffer(Buffer inStrIn, size_t inLen);

    DLLEXPORT ~ScopedFileBuffer();

    DLLEXPORT Buffer getBuf() const;

    DLLEXPORT size_t getLen() const;

    DLLEXPORT bool fail() const;
};


#if USE_MATHLINK
//
// A String from MathLink that has lexical scope
//
class ScopedMLString {
private:
    
    MLINK link;
    const char *buf;
    
public:
    
    ScopedMLString(MLINK link);
    
    ~ScopedMLString();
    
    bool read();
    
    const char *get() const;
};
#endif // USE_MATHLINK


#if USE_MATHLINK
//
// A ByteArray from MathLink that has lexical scope
//
class ScopedMLByteArray {
private:
    
    MLINK link;
    MBuffer buf;
    int *dims;
    char **heads;
    int depth;
    
public:
    
    ScopedMLByteArray(MLINK link);
    
    ~ScopedMLByteArray();
    
    bool read();
    
    Buffer get() const;
    
    size_t getByteCount() const;
};
#endif // USE_MATHLINK


#if USE_EXPR_LIB
//
//
//
class ScopedNumericArray {
private:
    
    WolframLibraryData libData;
    MNumericArray arr;
    
public:
    ScopedNumericArray(WolframLibraryData libData, MArgument Arg);
    
    ~ScopedNumericArray();
    
    size_t size() const;
    
    Buffer data() const;
};
#endif // USE_EXPR_LIB
