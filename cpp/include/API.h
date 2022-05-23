
#pragma once

#include "Node.h" // for NodePtr, Node, etc.
#include "Source.h" // for BufferAndLength
#include "Parser.h" // for FirstLineBehavior

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
#include <functional> // for function with GCC and MSVC
#include <cstddef> // for size_t


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
//
//
class NodeContainer {
    std::vector<NodePtr> N;
public:
    NodeContainer(std::vector<NodePtr> N) : N(std::move(N)) {}
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    bool check() const;
};

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

//
// A parser session
//
class ParserSession {
private:
    
    IssuePtrSet fatalIssues;
    IssuePtrSet nonFatalIssues;
    
    BufferAndLength bufAndLen;
    
    NodePtr concreteParseLeaf0(int mode);
    
public:
    
#if !NABORT
    std::function<bool ()> currentAbortQ;
#endif // !NABORT
    
    UnsafeCharacterEncodingFlag unsafeCharacterEncodingFlag;
    
    
    ParserSession();
    
    ~ParserSession();
    
    void init(
        BufferAndLength bufAndLen,
        WolframLibraryData libData,
        SourceConvention srcConvention,
        uint32_t tabWidth,
        FirstLineBehavior firstLineBehavior,
        EncodingMode encodingMode
    );
    
    void deinit();
    
    
    NodeContainer *parseExpressions();
    NodeContainer *tokenize();
    NodeContainer *concreteParseLeaf(StringifyMode mode);
    NodeContainer *safeString();
    
    void releaseContainer(NodeContainer *C);
    
#if !NABORT
    bool isAbort() const;
    
    NodePtr handleAbort() const;
#endif // !NABORT
    
    void setUnsafeCharacterEncodingFlag(UnsafeCharacterEncodingFlag flag);
    
#if !NISSUES
    void addIssue(IssuePtr);
#endif // !NISSUES
};

extern ParserSessionPtr TheParserSession;


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

EXTERN_C DLLEXPORT NodeContainer *ParserSessionParseExpressions();
EXTERN_C DLLEXPORT NodeContainer *ParserSessionTokenize();
EXTERN_C DLLEXPORT NodeContainer *ParserSessionConcreteParseLeaf(StringifyMode mode);
EXTERN_C DLLEXPORT void ParserSessionReleaseContainer(NodeContainer *C);

EXTERN_C DLLEXPORT void NodeContainerPrint(NodeContainer *C, std::ostream& stream);
EXTERN_C DLLEXPORT int NodeContainerCheck(NodeContainer *C);

EXTERN_C DLLEXPORT void ByteBufferInit(Buffer buf,
                                       size_t bufLen,
                                       WolframLibraryData libData);
EXTERN_C DLLEXPORT void ByteBufferDeinit();

EXTERN_C DLLEXPORT void ByteDecoderInit(SourceConvention srcConvention, uint32_t TabWidth, EncodingMode encodingMode);
EXTERN_C DLLEXPORT void ByteDecoderDeinit();



EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);


#if USE_MATHLINK

EXTERN_C DLLEXPORT int ConcreteParseBytes_Listable_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeBytes_Listable_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int SafeString_LibraryLink(WolframLibraryData libData, MLINK mlp);

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
