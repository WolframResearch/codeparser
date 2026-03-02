
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
#include <cassert>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h> // for MultiByteToWideChar
#endif // ifdef _WIN32


bool validatePath(WolframLibraryData libData, Buffer fullIn);

//
// MathLink sends System`List as List,
// So need to make sure that any testing of System symbols is sans context.
// bug 283291
// bug 284492
// bug 429034
//
const char *SYMBOL_LIST_SHORTNAME = "List";
const char *SYMBOL_BYTEARRAY_SHORTNAME = "ByteArray";


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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionPtr session;
    
    if (CreateParserSession(&session)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto e = Expr_FromInteger64(reinterpret_cast<mint>(session));
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int CreateParserSession_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 0) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionPtr session;
    
    if (CreateParserSession(&session)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
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
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
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
    
    if (Argc != 6) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
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
    
    auto mlAlreadyHasEOFSentinel = MArgument_getBoolean(Args[5]);
    auto alreadyHasEOFSentinel = static_cast<bool>(mlAlreadyHasEOFSentinel);

    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = alreadyHasEOFSentinel;
    
    if (ParserSessionInit(session, arr.data(), arr.size(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 6) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        assert(false);
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    auto mlAlreadyHasEOFSentinel = ScopedMLSymbol(callLink);
    if (!mlAlreadyHasEOFSentinel.read()) {
        assert(false);
    }
    
    auto alreadyHasEOFSentinel = (strcmp(mlAlreadyHasEOFSentinel.get(), "True") == 0);
    
    if (!MLNewPacket(callLink)) {
        assert(false);
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = alreadyHasEOFSentinel;
    
    if (ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
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
    
    if (!validatePath(libData, full.data())) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto fb = ScopedFileBuffer(full.data(), strlen(reinterpret_cast<const char *>(full.data())));
    
    if (fb.fail()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    type = MLGetType(callLink);
    if (type != MLTKSTR) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto full = ScopedMLUTF8String(callLink);
    if (!full.read()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
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
    
    if (!validatePath(libData, full.get())) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto fb = ScopedFileBuffer(full.get(), strlen(reinterpret_cast<const char *>(full.get())));
    
    if (fb.fail()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParse(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int TokenizeBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 6) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
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
    
    auto mlAlreadyHasEOFSentinel = MArgument_getBoolean(Args[5]);
    auto alreadyHasEOFSentinel = static_cast<bool>(mlAlreadyHasEOFSentinel);
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = alreadyHasEOFSentinel;
    
    if (ParserSessionInit(session, arr.data(), arr.size(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 6) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    auto mlAlreadyHasEOFSentinel = ScopedMLSymbol(callLink);
    if (!mlAlreadyHasEOFSentinel.read()) {
        assert(false);
    }
    
    auto alreadyHasEOFSentinel = (strcmp(mlAlreadyHasEOFSentinel.get(), "True") == 0);
    
    if (!MLNewPacket(callLink) ) {
        assert(false);
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = alreadyHasEOFSentinel;
    
    if (ParserSessionInit(session, arr.get(), arr.getByteCount(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
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
    
    if (!validatePath(libData, full.data())) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto fb = ScopedFileBuffer(full.data(), strlen(reinterpret_cast<const char *>(full.data())));
    
    if (fb.fail()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int TokenizeFile_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 5) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    type = MLGetType(callLink);
    if (type != MLTKSTR) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto full = ScopedMLUTF8String(callLink);
    if (!full.read()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(callLink) ) {
        assert(false);
    }
    
    if (!validatePath(libData, full.get())) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto fb = ScopedFileBuffer(full.get(), strlen(reinterpret_cast<const char *>(full.get())));
    
    if (fb.fail()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;
    
    if (ParserSessionInit(session, fb.getBuf(), fb.getLen(), libData, opts)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionTokenize(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParseLeaf(session, stringifyMode, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen))  {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 7) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlStringifyMode;
    if (!MLGetInteger(callLink, &mlStringifyMode)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto stringifyMode = static_cast<StringifyMode>(mlStringifyMode);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(callLink, &mlSrcConvention)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlTabWidth;
    if (!MLGetInteger(callLink, &mlTabWidth)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(callLink, &mlFirstLineBehavior)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlEncodingMode;
    if (!MLGetInteger(callLink, &mlEncodingMode)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionConcreteParseLeaf(session, stringifyMode, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "Argc: %d\n", static_cast<int>(Argc));
        
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionSafeString(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    expr e;
    
    if (NodeToExpr(session, N, &e)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }

    ParserSessionReleaseNode(session, N);

    ParserSessionDeinit(session);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int SafeString_LibraryLink(WolframLibraryData libData, MLINK callLink) {
    
    int type;
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlLen;
    if (!MLTestHead(callLink, SYMBOL_LIST_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 2) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    type = MLGetType(callLink);
    if (type != MLTKINT) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
#if SIZEOF_VOID_P == 8
    mlint64 mlSession;
    if (!MLGetInteger64(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#elif SIZEOF_VOID_P == 4
    int mlSession;
    if (!MLGetInteger32(callLink, &mlSession)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
#endif // SIZEOF_VOID_P == 8
    
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);
    
    type = MLGetType(callLink);
    if (type != MLTKFUNC) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "type: %d\n", type);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(callLink, SYMBOL_BYTEARRAY_SHORTNAME, &mlLen)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "len: %lu\n", len);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArray(callLink);
    if (!arr.read()) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        fprintf(stderr, "MLError: %d\n", MLError(callLink));
        
        return LIBRARY_FUNCTION_ERROR;
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
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    NodePtr N;
    
    if (ParserSessionSafeString(session, &N)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (NodePut(session, N, callLink)) {
        
        fprintf(stderr, "returning LIBRARY_FUNCTION_ERROR: %s:%d\n", __FILE__, __LINE__);
        
        return LIBRARY_FUNCTION_ERROR;
    }
    
    ParserSessionReleaseNode(session, N);
    
    ParserSessionDeinit(session);
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


ScopedFileBuffer::ScopedFileBuffer(Buffer inStrIn, size_t inLen) : buf(), len(), inited(false) {
    
    auto inStr = reinterpret_cast<const char *>(inStrIn);
    
#ifdef _WIN32
    
    //
    // fopen on Windows:
    // By default, a narrow filename string is interpreted using the ANSI codepage (CP_ACP)
    //
    // So convert UTF-8 string inStr to wide-character string and use _wfopen
    //
    // And while we are at it, use the secure version _wfopen_s
    //
    
    auto wLen = MultiByteToWideChar(CP_UTF8, 0, inStr, -1, 0, 0);
    
    if (wLen == 0) {
        return;
    }
    
    auto winStr = new wchar_t[wLen];
    
    wLen = MultiByteToWideChar(CP_UTF8, 0, inStr, -1, winStr, wLen);
    
    if (wLen == 0) {
        
        delete[] winStr;
        
        return;
    }
    
    FILE *file;
    auto err = _wfopen_s(&file, winStr, L"rb");
    
    if (err) {
        
        delete[] winStr;
        
        return;
    }
    
    delete[] winStr;
    
#else
    
    FILE *file = fopen(inStr, "r");
    
#endif // ifdef _WIN32
    
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
    
    auto r = fread(buf, sizeof(unsigned char), len, file);
    
    if (r != len) {
        
        delete[] buf;
        
        return;
    }
    
    //
    // EOF sentinel
    //
    buf[len] = 0xff;
    
    if (fclose(file)) {
        
        delete[] buf;
        
        return;
    }
    
    inited = true;
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


ScopedMLSymbol::ScopedMLSymbol(MLINK link) : link(link), buf(NULL) {}

ScopedMLSymbol::~ScopedMLSymbol() {
    
    if (!buf) {
        return;
    }
    
    MLReleaseSymbol(link, buf);
}

int ScopedMLSymbol::read() {
    return MLGetSymbol(link, &buf);
}

const char *ScopedMLSymbol::get() const {
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

//
// Does the file currently have permission to be read?
//
bool validatePath(WolframLibraryData libData, Buffer fullIn) {
    
    if (!libData) {
        //
        // If running as a stand-alone executable, then always valid
        //
        return true;
    }
    
    auto inStr1 = reinterpret_cast<const char *>(fullIn);
        
    auto inStr2 = const_cast<char *>(inStr1);
    
    return libData->validatePath(inStr2, 'R');
}
