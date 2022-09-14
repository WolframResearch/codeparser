
#pragma once

#include "Source.h" // for SourceConvention

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
#include <cstdint> // for uint_8
#include <ostream>

class Node;
class ParserSession;

using NodePtr = const Node *;
using ParserSessionPtr = ParserSession *;
using Buffer = const unsigned char *;
using MBuffer = unsigned char *;

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
enum StringifyMode : uint8_t {
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
enum EncodingMode : uint8_t {
    ENCODINGMODE_NORMAL = 0,
    ENCODINGMODE_BOX = 1,
};

enum FirstLineBehavior : uint8_t {
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

enum UnsafeCharacterEncodingFlag : uint8_t {
    UNSAFECHARACTERENCODING_OK = 0,
    UNSAFECHARACTERENCODING_INCOMPLETEUTF8SEQUENCE = 1,
    UNSAFECHARACTERENCODING_STRAYSURROGATE = 2,
    UNSAFECHARACTERENCODING_BOM = 3,
};

struct ParserSessionOptions {
    SourceConvention srcConvention;
    uint32_t tabWidth;
    FirstLineBehavior firstLineBehavior;
    EncodingMode encodingMode;
    int8_t alreadyHasEOFSentinel;
};


EXTERN_C DLLEXPORT int CreateParserSession(ParserSessionPtr *sessionOut);
EXTERN_C DLLEXPORT void DestroyParserSession(ParserSessionPtr session);

EXTERN_C DLLEXPORT int ParserSessionInit(ParserSessionPtr session, Buffer buf, uint64_t len, WolframLibraryData libData, ParserSessionOptions opts);
EXTERN_C DLLEXPORT int ParserSessionInitSimple(ParserSessionPtr session, Buffer Buf, uint64_t Len, int AlreadyHasEOFSentinel);
EXTERN_C DLLEXPORT void ParserSessionDeinit(ParserSessionPtr session);

EXTERN_C DLLEXPORT int ParserSessionConcreteParse(ParserSessionPtr session, NodePtr *nOut);
EXTERN_C DLLEXPORT int ParserSessionTokenize(ParserSessionPtr session, NodePtr *nOut);
EXTERN_C DLLEXPORT int ParserSessionConcreteParseLeaf(ParserSessionPtr session, StringifyMode mode, NodePtr *nOut);
EXTERN_C DLLEXPORT int ParserSessionSafeString(ParserSessionPtr session, NodePtr *nOut);
EXTERN_C DLLEXPORT void ParserSessionReleaseNode(ParserSessionPtr session, NodePtr N);

EXTERN_C DLLEXPORT int NodeSyntaxQ(NodePtr N);

DLLEXPORT void NodePrint(ParserSessionPtr session, NodePtr N, std::ostream& s);

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int NodeToExpr(ParserSessionPtr session, NodePtr N, expr *eOut);
#endif // USE_EXPR_LIB

#if USE_MATHLINK
EXTERN_C DLLEXPORT int NodePut(ParserSessionPtr session, NodePtr N, MLINK callLink);
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
EXTERN_C DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK link);
#endif // USE_EXPR_LIB

#if USE_EXPR_LIB
EXTERN_C DLLEXPORT int TokenizeFile_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);
#elif USE_MATHLINK
EXTERN_C DLLEXPORT int TokenizeFile_LibraryLink(WolframLibraryData libData, MLINK link);
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
// A UTF8 String from MathLink that has lexical scope
//
class ScopedMLUTF8String {
private:
    
    MLINK link;
    Buffer buf;
    //
    // used by MLReleaseUTF8String
    //
    // not exposed publicly, to stay consistent with other strings
    //
    int b;
    //
    // unused
    //
    int c;
    
public:
    
    ScopedMLUTF8String(MLINK link);
    
    ~ScopedMLUTF8String();
    
    bool read();
    
    Buffer get() const;
};

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
    
    int read();
    
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
    
    int read();
    
    Buffer get() const;
    
    size_t getByteCount() const;
};
#endif // USE_MATHLINK


#if USE_EXPR_LIB
//
//
//
class ScopedUTF8String {
private:
    
    WolframLibraryData libData;
    Buffer str;
    
public:
    ScopedUTF8String(WolframLibraryData libData, MArgument Arg);
    
    ~ScopedUTF8String();
    
    Buffer data() const;
};


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
