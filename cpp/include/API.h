
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
#include <functional> // for function with GCC and MSVC
#include <cstddef> // for size_t

#if !NABORT
#define HANDLE_ABORT \
do { \
    if (TheParserSession->isAbort()) { \
        auto Abort = TheParserSession->handleAbort(); \
        TheParser->pushNode(std::move(Abort)); \
        return; \
    } \
} while (0)
#else
#define HANDLE_ABORT
#endif // !NABORT

#if !NMUSTTAIL
#define MUSTTAIL [[clang::musttail]]
#else
#define MUSTTAIL
#endif // !NMUSTTAIL


class ParserSession;
#if USE_MATHLINK
class ScopedMLByteArray;
class ScopedMLUTF8String;
class ScopedMLString;
class ScopedMLEnvironmentParameter;
#endif // USE_MATHLINK
class Node;

using ParserSessionPtr = std::unique_ptr<ParserSession>;
using NodePtr = std::unique_ptr<Node>;

#if USE_MATHLINK
using ScopedMLByteArrayPtr = std::unique_ptr<ScopedMLByteArray>;
using ScopedMLUTF8StringPtr = std::unique_ptr<ScopedMLUTF8String>;
using ScopedMLStringPtr = std::unique_ptr<ScopedMLString>;
using ScopedMLEnvironmentParameterPtr = std::unique_ptr<ScopedMLEnvironmentParameter>;
#endif // USE_MATHLINK

#if USE_EXPR_LIB
using expr = void *;
#endif // USE_EXPR_LIB


//
//
//
class NodeContainer {
public:
    
    std::vector<NodePtr> N;
    
    NodeContainer(std::vector<NodePtr> N);
    
#if USE_MATHLINK
    void put(MLINK mlp) const;
#endif // USE_MATHLINK
    
    void print(std::ostream& s) const;
    
    bool check() const;
    
#if USE_EXPR_LIB
    expr toExpr() const;
#endif // USE_EXPR_LIB
};

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


//
// A parser session
//
class ParserSession {
private:
    
    IssuePtrSet fatalIssues;
    IssuePtrSet nonFatalIssues;
    
    NodePtr concreteParseLeaf0(int mode);
    
public:
    
#if !NABORT
    std::function<bool ()> currentAbortQ;
#endif // !NABORT
    
    UnsafeCharacterEncodingFlag unsafeCharacterEncodingFlag;
    
    BufferAndLength bufAndLen;
    WolframLibraryData libData;
    SourceConvention srcConvention;
    uint32_t tabWidth;
    FirstLineBehavior firstLineBehavior;
    EncodingMode encodingMode;
    
    
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
    
    NodeContainerPtr parseExpressions();
    NodeContainerPtr tokenize();
    NodeContainerPtr concreteParseLeaf(StringifyMode mode);
    NodeContainerPtr safeString();
    
    void releaseContainer(NodeContainerPtr C);
    
#if !NABORT
    bool isAbort() const;
    
    NodePtr handleAbort() const;
    
#if USE_EXPR_LIB
    expr handleAbortExpr() const;
#endif // USE_EXPR_LIB
    
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


#if DIAGNOSTICS

EXTERN_C DLLEXPORT int CharacterDecoder_LineContinuationCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LongNameCount;

EXTERN_C DLLEXPORT int CharacterDecoder_4HexCount;

EXTERN_C DLLEXPORT int CharacterDecoder_2HexCount;

EXTERN_C DLLEXPORT int CharacterDecoder_6HexCount;

EXTERN_C DLLEXPORT int CharacterDecoder_OctalCount;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaBackspace;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaFormFeed;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaLineFeedCount;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaCarriageReturn;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaTab;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaDoubleQuoteCount;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaBackslashCount;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaOpenCount;

EXTERN_C DLLEXPORT int CharacterDecoder_StringMetaCloseCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxBangCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxPercentCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxAmpCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxOpenParenCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxCloseParenCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxStarCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxPlusCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxSlashCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxAtCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxCaretCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxUnderscoreCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxBacktickCount;

EXTERN_C DLLEXPORT int CharacterDecoder_LinearSyntaxSpaceCount;

EXTERN_C DLLEXPORT int CharacterDecoder_UnhandledCount;

#endif // DIAGNOSTICS
