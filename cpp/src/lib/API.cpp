
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
using ScopedMLByteArrayPtr = std::unique_ptr<ScopedMLByteArray>;
using ScopedMLUTF8StringPtr = std::unique_ptr<ScopedMLUTF8String>;
using ScopedMLEnvironmentParameterPtr = std::unique_ptr<ScopedMLEnvironmentParameter>;
#endif // USE_MATHLINK

void ParserSessionCreate() {
    TheParserSession = ParserSessionPtr(new ParserSession());
}

void ParserSessionDestroy() {
    TheParserSession.reset(nullptr);
}

void ParserSessionInit(Buffer buf,
                      size_t bufLen,
                      WolframLibraryData libData,
                      SourceConvention srcConvention,
                      uint32_t tabWidth,
                      FirstLineBehavior firstLineBehavior,
                      EncodingMode encodingMode) {
    BufferAndLength bufAndLen = BufferAndLength(buf, bufLen);
    TheParserSession->init(bufAndLen, libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
}

void ParserSessionDeinit() {
    TheParserSession->deinit();
}

NodeContainerPtr ParserSessionParseExpressions() {
    return TheParserSession->parseExpressions();
}

NodeContainerPtr ParserSessionTokenize() {
    return TheParserSession->tokenize();
}

NodeContainerPtr ParserSessionConcreteParseLeaf(StringifyMode mode) {
    return TheParserSession->concreteParseLeaf(mode);
}

NodeContainerPtr ParserSessionSafeString() {
    return TheParserSession->safeString();
}

void ParserSessionReleaseContainer(NodeContainerPtr C) {
    TheParserSession->releaseContainer(C);
}

ParserSessionPtr TheParserSession = nullptr;



mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

int WolframLibrary_initialize(WolframLibraryData libData) {
    
    ParserSessionCreate();
    
    return 0;
}

void WolframLibrary_uninitialize(WolframLibraryData libData) {
    ParserSessionDestroy();
}

#if USE_EXPR_LIB
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bytes = MArgument_getMNumericArray(Args[0]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[1]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    auto tabWidth = MArgument_getInteger(Args[2]);
    
    auto mlFirstLineBehavior = MArgument_getInteger(Args[3]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(bytes);
    
    auto data = reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(bytes));
    
    auto bufAndLen = BufferAndLength(data, numBytes);
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionParseExpressions();
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->numericarrayLibraryFunctions->MNumericArray_disown(bytes);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
        
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(mlp, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArrayPtr(new ScopedMLByteArray(mlp));
    if (!arr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(mlp, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(mlp, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(mlp, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
        
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionParseExpressions();
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int TokenizeBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bytes = MArgument_getMNumericArray(Args[0]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[1]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    auto tabWidth = MArgument_getInteger(Args[2]);
    
    auto mlFirstLineBehavior = MArgument_getInteger(Args[3]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    
    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(bytes);
    
    auto data = reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(bytes));
    
    auto bufAndLen = BufferAndLength(data, numBytes);
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionTokenize();
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->numericarrayLibraryFunctions->MNumericArray_disown(bytes);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
        
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(mlp, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArrayPtr(new ScopedMLByteArray(mlp));
    if (!arr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(mlp, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(mlp, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(mlp, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
        
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionTokenize();
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 6) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto inStrRaw = MArgument_getUTF8String(Args[0]);
    auto inStr = std::string(inStrRaw);
    
    auto stringifyMode = MArgument_getInteger(Args[1]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    auto tabWidth = MArgument_getInteger(Args[3]);
    
    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    auto mlEncodingMode = MArgument_getInteger(Args[5]);
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);
    
    
    auto bufAndLen = BufferAndLength(reinterpret_cast<Buffer>(inStr.c_str()), inStr.size());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
    
    auto C = ParserSessionConcreteParseLeaf(static_cast<StringifyMode>(stringifyMode));
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->UTF8String_disown(inStrRaw);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    std::string unescaped;
    
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen))  {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 6) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto inStr = ScopedMLUTF8StringPtr(new ScopedMLUTF8String(mlp));
    if (!inStr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int stringifyMode;
    if (!MLGetInteger(mlp, &stringifyMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(mlp, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(mlp, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(mlp, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    int mlEncodingMode;
    if (!MLGetInteger(mlp, &mlEncodingMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);

    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bufAndLen = BufferAndLength(inStr->get(), inStr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
    
    auto C = ParserSessionConcreteParseLeaf(static_cast<StringifyMode>(stringifyMode));
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int SafeString_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bytes = MArgument_getMNumericArray(Args[0]);
    
    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(bytes);
    
    auto data = reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(bytes));
    
    auto bufAndLen = BufferAndLength(data, numBytes);
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionSafeString();
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->numericarrayLibraryFunctions->MNumericArray_disown(bytes);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int SafeString_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(mlp, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArrayPtr(new ScopedMLByteArray(mlp));
    if (!arr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionSafeString();
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_MATHLINK
ScopedMLUTF8String::ScopedMLUTF8String(MLINK mlp) : mlp(mlp), buf(NULL), b(), c() {}

ScopedMLUTF8String::~ScopedMLUTF8String() {
    
    if (buf == NULL) {
        return;
    }
    
    MLReleaseUTF8String(mlp, buf, b);
}

bool ScopedMLUTF8String::read() {
    return MLGetUTF8String(mlp, &buf, &b, &c);
}

Buffer ScopedMLUTF8String::get() const {
    return buf;
}

size_t ScopedMLUTF8String::getByteCount() const {
    return b;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLString::ScopedMLString(MLINK mlp) : mlp(mlp), buf(NULL) {}

ScopedMLString::~ScopedMLString() {
    
    if (buf == NULL) {
        return;
    }
    
    MLReleaseString(mlp, buf);
}

bool ScopedMLString::read() {
    return MLGetString(mlp, &buf);
}

const char *ScopedMLString::get() const {
    return buf;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLSymbol::ScopedMLSymbol(MLINK mlp) : mlp(mlp), sym(NULL) {}

ScopedMLSymbol::~ScopedMLSymbol() {
    if (sym != NULL) {
        MLReleaseSymbol(mlp, sym);
    }
}

bool ScopedMLSymbol::read() {
    return MLGetSymbol(mlp, &sym);
}

const char *ScopedMLSymbol::get() const {
    return sym;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLFunction::ScopedMLFunction(MLINK mlp) : mlp(mlp), func(NULL), count() {}

ScopedMLFunction::~ScopedMLFunction() {
    
    if (func == NULL) {
        return;
    }
    
    MLReleaseSymbol(mlp, func);
}

bool ScopedMLFunction::read() {
    return MLGetFunction(mlp, &func, &count);
}

const char *ScopedMLFunction::getHead() const {
    return func;
}

int ScopedMLFunction::getArgCount() const {
    return count;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLByteArray::ScopedMLByteArray(MLINK mlp) : mlp(mlp), buf(NULL), dims(), heads(), depth() {}

ScopedMLByteArray::~ScopedMLByteArray() {
    
    if (buf == NULL) {
        return;
    }
    
    MLReleaseByteArray(mlp, buf, dims, heads, depth);
}

bool ScopedMLByteArray::read() {
    return MLGetByteArray(mlp, &buf, &dims, &heads, &depth);
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

MLEnvironmentParameter ScopedMLEnvironmentParameter::get() {
    return p;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLLoopbackLink::ScopedMLLoopbackLink() : mlp(NULL), ep(NULL) {
    
    ScopedMLEnvironmentParameterPtr p;
    
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
    
    mlp = MLLoopbackOpen(ep, &err);
}

ScopedMLLoopbackLink::~ScopedMLLoopbackLink() {
    
    if (mlp == NULL) {
        return;
    }
    
    MLClose(mlp);
    
    MLDeinitialize(ep);
}

MLINK ScopedMLLoopbackLink::get() {
    return mlp;
}
#endif // USE_MATHLINK
