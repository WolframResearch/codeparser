
#include "API.h"

#include "ParserSession.h"
#include "Node.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#include "WolframNumericArrayLibrary.h"
#endif // USE_EXPR_LIB

#ifdef WINDOWS_MATHLINK
#else
#include <signal.h> // for SIGINT
#endif // WINDOWS_MATHLINK

#if USE_MATHLINK
#endif // USE_MATHLINK


ParserSessionPtr CreateParserSession() {
    return new ParserSession();
}

void DestroyParserSession(ParserSessionPtr session) {
    delete session;
}

void ParserSessionInit(ParserSessionPtr session, Buffer buf, size_t bufLen, WolframLibraryData libData, SourceConvention srcConvention, uint32_t tabWidth, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode) {
    
    BufferAndLength bufAndLen = BufferAndLength(buf, bufLen);
    
    session->init(bufAndLen, libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
}

void ParserSessionDeinit(ParserSessionPtr session) {
    session->deinit();
}

NodeContainerPtr ParserSessionParseExpressions(ParserSessionPtr session) {
    return session->parseExpressions();
}

NodeContainerPtr ParserSessionTokenize(ParserSessionPtr session) {
    return session->tokenize();
}

NodeContainerPtr ParserSessionConcreteParseLeaf(ParserSessionPtr session, StringifyMode mode) {
    return session->concreteParseLeaf(mode);
}

NodeContainerPtr ParserSessionSafeString(ParserSessionPtr session) {
    return session->safeString();
}

void ParserSessionReleaseContainer(ParserSessionPtr session, NodeContainerPtr C) {
    session->releaseContainer(C);
}


void NodeContainerPrint(NodeContainerPtr C, std::ostream& s) {
    C->print(s);
}

int NodeContainerCheck(NodeContainerPtr C) {
    return C->check();
}


mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

int WolframLibrary_initialize(WolframLibraryData libData) {
    return 0;
}

void WolframLibrary_uninitialize(WolframLibraryData libData) {
    
}


#if USE_EXPR_LIB
DLLEXPORT int CreateParserSession_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 0) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto session = CreateParserSession();
    
    auto e = Expr_FromInteger64(reinterpret_cast<mint>(session));
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int CreateParserSession_LibraryLink(WolframLibraryData libData, MLINK link) {
    
    int mlLen;
        
    if (!MLTestHead(link, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 0) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto session = CreateParserSession();
    
    if (!MLPutLongInteger(link, reinterpret_cast<long>(session))) {
        assert(false);
    }
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
DLLEXPORT int DestroyParserSession_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    DestroyParserSession(session);
    
    //
    // cannot use session after this
    //
    
    auto e = Expr_MEncodedStringToSymbolExpr(SYMBOL_NULL.name());
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int DestroyParserSession_LibraryLink(WolframLibraryData libData, MLINK link) {
    
    int mlLen;
        
    if (!MLTestHead(link, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    long mlSession;
    if (!MLGetLongInteger(link, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    DestroyParserSession(session);
    
    //
    // cannot use session after this
    //
    
    if (!MLPutSymbol(link, SYMBOL_NULL.name())) {
      assert(false);
    }
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    auto arr = ScopedNumericArray(libData, Args[1]);

    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[3]);
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);

    auto numBytes = arr.size();

    auto data = arr.data();

    ParserSessionInit(session, data, numBytes, libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);

    auto C = ParserSessionParseExpressions(session);

    auto e = C->toExpr(session);

    ParserSessionReleaseContainer(session, C);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK link) {
    
    int mlLen;
        
    if (!MLTestHead(link, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    long mlSession;
    if (!MLGetLongInteger(link, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (!MLTestHead(link, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(link);
    if (!arr.read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(link, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(link, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(link, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(link)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionParseExpressions(session);
    
    C->put(session);
    
    ParserSessionReleaseContainer(session, C);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int TokenizeBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    auto arr = ScopedNumericArray(libData, Args[1]);

    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[3]);
    auto tabWidth = static_cast<int>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);

    auto numBytes = arr.size();

    auto data = arr.data();

    ParserSessionInit(session, data, numBytes, libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);

    auto C = ParserSessionTokenize(session);

    auto e = C->toExpr(session);

    ParserSessionReleaseContainer(session, C);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK link) {
    
    int mlLen;
    
    if (!MLTestHead(link, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    long mlSession;
    if (!MLGetLongInteger(link, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (!MLTestHead(link, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(link);
    if (!arr.read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(link, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(link, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(link, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(link) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionTokenize(session);
    
    C->put(session);
    
    ParserSessionReleaseContainer(session, C);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 7) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    auto arr = ScopedNumericArray(libData, Args[1]);

    auto stringifyMode = MArgument_getInteger(Args[2]);

    auto mlSrcConvention = MArgument_getInteger(Args[3]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[4]);
    auto tabWidth = static_cast<int>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[5]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);

    auto mlEncodingMode = MArgument_getInteger(Args[6]);
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);

    auto numBytes = arr.size();

    auto data = arr.data();

    ParserSessionInit(session, data, numBytes, libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);

    auto C = ParserSessionConcreteParseLeaf(session, static_cast<StringifyMode>(stringifyMode));

    auto e = C->toExpr(session);

    ParserSessionReleaseContainer(session, C);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK link) {
    
    int mlLen;
    
    std::string unescaped;
    
    if (!MLTestHead(link, SYMBOL_LIST.name(), &mlLen))  {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 7) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    long mlSession;
    if (!MLGetLongInteger(link, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (!MLTestHead(link, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(link);
    if (!arr.read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int stringifyMode;
    if (!MLGetInteger(link, &stringifyMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(link, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(link, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(link, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    int mlEncodingMode;
    if (!MLGetInteger(link, &mlEncodingMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);

    if (!MLNewPacket(link) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto numBytes = arr.getByteCount();
    
    auto data = arr.get();
    
    ParserSessionInit(session, data, numBytes, libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
    
    auto C = ParserSessionConcreteParseLeaf(session, static_cast<StringifyMode>(stringifyMode));
    
    C->put(session);
    
    ParserSessionReleaseContainer(session, C);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int SafeString_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 2) {
        return LIBRARY_FUNCTION_ERROR;
    }

    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    auto arr = ScopedNumericArray(libData, Args[1]);

    auto numBytes = arr.size();

    auto data = arr.data();

    ParserSessionInit(session, data, numBytes, libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);

    auto C = ParserSessionSafeString(session);

    auto e = C->toExpr(session);

    ParserSessionReleaseContainer(session, C);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int SafeString_LibraryLink(WolframLibraryData libData, MLINK link) {
    
    int mlLen;
    
    if (!MLTestHead(link, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 2) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    long mlSession;
    if (!MLGetLongInteger(link, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (!MLTestHead(link, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(link);
    if (!arr.read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLNewPacket(link) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionSafeString(session);
    
    C->put(session);
    
    ParserSessionReleaseContainer(session, C);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_MATHLINK
ScopedMLUTF8String::ScopedMLUTF8String(MLINK link) : link(link), buf(NULL), b(), c() {}

ScopedMLUTF8String::~ScopedMLUTF8String() {
    
    if (!buf) {
        return;
    }
    
    MLReleaseUTF8String(link, buf, b);
}

bool ScopedMLUTF8String::read() {
    return MLGetUTF8String(link, &buf, &b, &c);
}

Buffer ScopedMLUTF8String::get() const {
    return buf;
}

size_t ScopedMLUTF8String::getByteCount() const {
    return b;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLString::ScopedMLString(MLINK link) : link(link), buf(NULL) {}

ScopedMLString::~ScopedMLString() {
    
    if (!buf) {
        return;
    }
    
    MLReleaseString(link, buf);
}

bool ScopedMLString::read() {
    return MLGetString(link, &buf);
}

const char *ScopedMLString::get() const {
    return buf;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLSymbol::ScopedMLSymbol(MLINK link) : link(link), sym(NULL) {}

ScopedMLSymbol::~ScopedMLSymbol() {
    if (sym != NULL) {
        MLReleaseSymbol(link, sym);
    }
}

bool ScopedMLSymbol::read() {
    return MLGetSymbol(link, &sym);
}

const char *ScopedMLSymbol::get() const {
    return sym;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLFunction::ScopedMLFunction(MLINK link) : link(link), func(NULL), count() {}

ScopedMLFunction::~ScopedMLFunction() {
    
    if (!func) {
        return;
    }
    
    MLReleaseSymbol(link, func);
}

bool ScopedMLFunction::read() {
    return MLGetFunction(link, &func, &count);
}

const char *ScopedMLFunction::getHead() const {
    return func;
}

int ScopedMLFunction::getArgCount() const {
    return count;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLByteArray::ScopedMLByteArray(MLINK link) : link(link), buf(NULL), dims(), heads(), depth() {}

ScopedMLByteArray::~ScopedMLByteArray() {
    
    if (!buf) {
        return;
    }
    
    MLReleaseByteArray(link, buf, dims, heads, depth);
}

bool ScopedMLByteArray::read() {
    return MLGetByteArray(link, &buf, &dims, &heads, &depth);
}

Buffer ScopedMLByteArray::get() const {
    return buf;
}

size_t ScopedMLByteArray::getByteCount() const {
    return dims[0];
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLEnvironmentParameter::ScopedMLEnvironmentParameter() : p(MLNewParameters(MLREVISION, MLAPIREVISION)) {}

ScopedMLEnvironmentParameter::~ScopedMLEnvironmentParameter() {
    MLReleaseParameters(p);
}

MLEnvironmentParameter ScopedMLEnvironmentParameter::get() const {
    return p;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLLoopbackLink::ScopedMLLoopbackLink() : link(NULL), ep(NULL) {
    
    ScopedMLEnvironmentParameter p;
    
    int err;
    
#ifdef WINDOWS_MATHLINK
#else
    //
    // Needed because MathLink intercepts all signals
    //
    MLDoNotHandleSignalParameter(p.get(), SIGINT);
#endif // WINDOWS_MATHLINK
    
    ep = MLInitialize(p.get());
    
    if (ep == (MLENV)0) {
        return;
    }
    
    link = MLLoopbackOpen(ep, &err);
}

ScopedMLLoopbackLink::~ScopedMLLoopbackLink() {
    
    if (!link) {
        return;
    }
    
    MLClose(link);
    
    MLDeinitialize(ep);
}

MLINK ScopedMLLoopbackLink::get() const {
    return link;
}
#endif // USE_MATHLINK


#if USE_EXPR_LIB
ScopedNumericArray::ScopedNumericArray(WolframLibraryData libData, MArgument Arg) : libData(libData) {
    arr = MArgument_getMNumericArray(Arg);
}
    
ScopedNumericArray::~ScopedNumericArray() {
    libData->numericarrayLibraryFunctions->MNumericArray_disown(arr);
}

size_t ScopedNumericArray::size() const {
    return libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(arr);
}

Buffer ScopedNumericArray::data() const {
    return reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(arr));
}
#endif // USE_EXPR_LIB
