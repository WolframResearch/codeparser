
#include "API.h"

#include "ParserSession.h"
#include "Node.h"
#include "SymbolRegistration.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#include "WolframNumericArrayLibrary.h"
#endif // USE_EXPR_LIB

#include <cstdio> // for fopen
#include <cstring> // for strlen


int CreateParserSession(ParserSessionPtr *sessionOut) {
    
    *sessionOut = new ParserSession();
    
    return 0;
}

void DestroyParserSession(ParserSessionPtr session) {
    delete session;
}

int ParserSessionInit(ParserSessionPtr session, Buffer Buf, uint64_t Len, WolframLibraryData libData, ParserSessionOptions opts) {
    return session->init(Buf, Len, libData, opts);
}

int ParserSessionInitSimple(ParserSessionPtr session, Buffer Buf, uint64_t Len, int AlreadyHasEOFSentinel) {
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = AlreadyHasEOFSentinel;
    
    return session->init(Buf, Len, nullptr, opts);
}

void ParserSessionDeinit(ParserSessionPtr session) {
    session->deinit();
}

int ParserSessionConcreteParse(ParserSessionPtr session, NodePtr *nOut) {
    
    *nOut = session->concreteParse();
    
    return 0;
}

int ParserSessionTokenize(ParserSessionPtr session, NodePtr *nOut) {
    
    *nOut = session->tokenize();
    
    return 0;
}

int ParserSessionConcreteParseLeaf(ParserSessionPtr session, StringifyMode mode, NodePtr *nOut) {
    
    *nOut = session->concreteParseLeaf(mode);
    
    return 0;
}

int ParserSessionSafeString(ParserSessionPtr session, NodePtr *nOut) {
    
    *nOut = session->safeString();
    
    return 0;
}

void ParserSessionReleaseNode(ParserSessionPtr session, NodePtr N) {
    session->releaseNode(N);
}


void NodePrint(ParserSessionPtr session, NodePtr N, std::ostream& s) {
    N->print(session, s);
}

int NodeSyntaxQ(NodePtr N) {
    return N->syntaxQ();
}


#if USE_EXPR_LIB
int NodeToExpr(ParserSessionPtr session, NodePtr N, expr *eOut) {
    
    *eOut = N->toExpr(session);
    
    return 0;
}
#endif // USE_EXPR_LIB

#if USE_MATHLINK
int NodePut(ParserSessionPtr session, NodePtr N, MLINK callLink) {
    
    N->put(session, callLink);
    
    return 0;
}
#endif // USE_MATHLINK


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
    
    ParserSessionPtr session;
    
    if (CreateParserSession(&session)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto e = Expr_FromInteger64(reinterpret_cast<mint>(session));
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int CreateParserSession_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 0) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionPtr session;
    
    if (CreateParserSession(&session)) {
        return LIBRARY_FUNCTION_ERROR;
    }

#if SIZEOF_VOID_P == 8
    if (!MLPutInteger64(callLink, reinterpret_cast<mlint64>(session))) {
        assert(false);
    }
#elif SIZEOF_VOID_P == 4
    if (!MLPutInteger32(callLink, reinterpret_cast<int>(session))) {
        assert(false);
    }
#endif // SIZEOF_VOID_P == 8
    
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
    
    auto e = Expr_MEncodedStringToSymbolExpr(SYMBOL_NULL.Name);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int DestroyParserSession_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    DestroyParserSession(session);
    
    //
    // cannot use session after this
    //
    
    if (!MLPutSymbol(callLink, SYMBOL_NULL.Name)) {
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
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.data(), arr.size(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        assert(false);
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        assert(false);
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY.Name, &mlLen)) {
        assert(false);
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        assert(false);
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        assert(false);
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        assert(false);
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        assert(false);
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(callLink)) {
        assert(false);
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    auto full = ScopedUTF8String(libData, Args[1]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[3]);
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    auto fb = ScopedFileBuffer(full.data(), strlen(reinterpret_cast<const char *>(full.data())));
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        assert(false);
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        assert(false);
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (MLGetType(callLink) != MLTKSTR) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto full = ScopedMLUTF8String(callLink);
    if (!full.read()) {
        assert(false);
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        assert(false);
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        assert(false);
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        assert(false);
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(callLink)) {
        assert(false);
    }
    
    auto fb = ScopedFileBuffer(full.get(), strlen(reinterpret_cast<const char *>(full.get())));
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
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
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.data(), arr.size(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        assert(false);
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        assert(false);
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(callLink) ) {
        assert(false);
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int TokenizeFile_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    auto full = ScopedUTF8String(libData, Args[1]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[3]);
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    auto fb = ScopedFileBuffer(full.data(), strlen(reinterpret_cast<const char *>(full.data())));
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int TokenizeFile_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (MLGetType(callLink) != MLTKSTR) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto full = ScopedMLUTF8String(callLink);
    if (!full.read()) {
        assert(false);
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        assert(false);
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(callLink) ) {
        assert(false);
    }
    
    auto fb = ScopedFileBuffer(full.get(), strlen(reinterpret_cast<const char *>(full.get())));
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
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

    auto mlStringifyMode = MArgument_getInteger(Args[2]);
    auto stringifyMode = static_cast<StringifyMode>(mlStringifyMode);
    
    auto mlSrcConvention = MArgument_getInteger(Args[3]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[4]);
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[5]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);

    auto mlEncodingMode = MArgument_getInteger(Args[6]);
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = encodingMode;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.data(), arr.size(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParseLeaf(session, stringifyMode, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    std::string unescaped;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen))  {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 7) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        assert(false);
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlStringifyMode;
    if (!MLGetInteger(callLink, &mlStringifyMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto stringifyMode = static_cast<StringifyMode>(mlStringifyMode);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlEncodingMode;
    if (!MLGetInteger(callLink, &mlEncodingMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);

    if (!MLNewPacket(callLink) ) {
        assert(false);
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = encodingMode;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParseLeaf(session, stringifyMode, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
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
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.data(), arr.size(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionSafeString(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int SafeString_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int mlLen;
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_LIST.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 2) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (MLGetType(callLink) != MLTKINT) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    if (MLGetType(callLink) != MLTKFUNC) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY.Name, &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        assert(false);
    }
    
    if (!MLNewPacket(callLink) ) {
        assert(false);
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = SOURCECONVENTION_LINECOLUMN;
    opts.tabWidth = DEFAULT_TAB_WIDTH;
    opts.firstLineBehavior = FIRSTLINEBEHAVIOR_NOTSCRIPT;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = false;
    
    if (ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionSafeString(session, &N)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


ScopedFileBuffer::ScopedFileBuffer(Buffer inStrIn, size_t inLen) : buf(), len(), inited(false) {
    
    auto inStr = reinterpret_cast<const char *>(inStrIn);
    
    FILE *file = fopen(inStr, "rb");
    
    if (!file) {
        return;
    }
    
    if (fseek(file, 0, SEEK_END)) {
        return;
    }
    
    auto res = ftell(file);
    
    if (res < 0) {
        return;
    }
    len = res;
    
    rewind(file);
    
    //
    // add 1 byte at end for EOF sentinel
    //
    buf = new unsigned char[len + 1];
    
    inited = true;
    
    auto r = fread(buf, sizeof(unsigned char), len, file);
    
    if (r != len) {
        
        inited = false;
        
        delete[] buf;
    }
    
    //
    // EOF sentinel
    //
    buf[len] = 0xff;
    
    fclose(file);
}

ScopedFileBuffer::~ScopedFileBuffer() {

    if (!inited) {
        return;
    }

    delete[] buf;
}

Buffer ScopedFileBuffer::getBuf() const {
    return buf;
}

size_t ScopedFileBuffer::getLen() const {
    //
    // add 1 byte at end for EOF sentinel
    //
    return len + 1;
}

bool ScopedFileBuffer::fail() const {
    return !inited;
}


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


ScopedMLString::ScopedMLString(MLINK link) : link(link), buf(NULL) {}

ScopedMLString::~ScopedMLString() {
    
    if (!buf) {
        return;
    }
    
    MLReleaseString(link, buf);
}

int ScopedMLString::read() {
    return MLGetString(link, &buf);
}

const char *ScopedMLString::get() const {
    return buf;
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

int ScopedMLByteArray::read() {
    return MLGetByteArray(link, &buf, &dims, &heads, &depth);
}

Buffer ScopedMLByteArray::get() const {
    return buf;
}

size_t ScopedMLByteArray::getByteCount() const {
    return *dims;
}
#endif // USE_MATHLINK


#if USE_EXPR_LIB
ScopedUTF8String::ScopedUTF8String(WolframLibraryData libData, MArgument Arg) : libData(libData), str() {
    str = reinterpret_cast<MBuffer>(MArgument_getUTF8String(Arg));
}
    
ScopedUTF8String::~ScopedUTF8String() {
    libData->UTF8String_disown(const_cast<char *>(reinterpret_cast<const char *>(str)));
}

Buffer ScopedUTF8String::data() const {
    return str;
}


ScopedNumericArray::ScopedNumericArray(WolframLibraryData libData, MArgument Arg) : libData(libData), arr() {
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
