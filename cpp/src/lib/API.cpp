
#include "API.h"

#include "Parser.h"
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "ByteBuffer.h"
#include "Utils.h"
#include "Symbol.h"

#include <memory> // for unique_ptr
#ifdef WINDOWS_MATHLINK
#else
#include <signal.h>
#endif
#include <vector>

bool validatePath(WolframLibraryData libData, const unsigned char *inStr, size_t len);


ParserSession::ParserSession() : bufAndLen(),
#if !NABORT
currentAbortQ(),
#endif // NABORT
policy() {
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    TheCharacterDecoder = CharacterDecoderPtr(new CharacterDecoder());
    TheTokenizer = TokenizerPtr(new Tokenizer());
    TheParser = ParserPtr(new Parser());
}

ParserSession::~ParserSession() {

    TheParser.reset(nullptr);
    TheTokenizer.reset(nullptr);
    TheCharacterDecoder.reset(nullptr);
    TheByteDecoder.reset(nullptr);
    TheByteBuffer.reset(nullptr);
}

void ParserSession::init(BufferAndLength bufAndLenIn, WolframLibraryData libData, ParserSessionPolicy policyIn) {
    
    bufAndLen = bufAndLenIn;
    
    policy = policyIn;
    
    TheByteBuffer->init(bufAndLen, libData);
    TheByteDecoder->init();
    TheCharacterDecoder->init(libData);
    TheTokenizer->init();
    TheParser->init();
    
#if !NABORT
    if (libData) {
        currentAbortQ = [libData]() {
            //
            // For some reason, AbortQ() returns a mint
            //
            bool res = libData->AbortQ();
            return res;
        };
    } else {
        currentAbortQ = nullptr;
    }
#endif // NABORT
}

void ParserSession::deinit() {
    
    TheParser->deinit();
    TheTokenizer->deinit();
    TheCharacterDecoder->deinit();
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

Node *ParserSession::parseExpressions() {
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
        ParserContext Ctxt;
        
        while (true) {
            
#if !NABORT
            if (TheParserSession->isAbort()) {
                
                break;
            }
#endif // !NABORT
            
            auto peek = TheParser->currentToken();
            
            if (peek.Tok == TOKEN_ENDOFFILE) {
                break;
            }
            
            if (peek.Tok.isTrivia()) {
                
                exprs.push_back(LeafNodePtr(new LeafNode(std::move(peek))));
                
                TheParser->nextToken();
                
                continue;
            }
            
            if (!peek.Tok.isPossibleBeginningOfExpression()) {
                
                auto NotPossible = TheParser->handleNotPossible(peek, peek, Ctxt, nullptr);
                
                exprs.push_back(std::move(NotPossible));
                
                continue;
            }
            
            auto Expr = TheParser->parse(peek, Ctxt);
            
            exprs.push_back(std::move(Expr));
            
        } // while (true)
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    //
    // Now handle the out-of-band expressions, i.e., issues and metadata
    //
    
    //
    // Collect all issues from the various components
    //
    {
        std::vector<IssuePtr> issues;
        
#if !NISSUES
        auto& ParserIssues = TheParser->getIssues();
        for (auto& I : ParserIssues) {
            issues.push_back(std::move(I));
        }
        
        auto& TokenizerIssues = TheTokenizer->getIssues();
        for (auto& I : TokenizerIssues) {
            issues.push_back(std::move(I));
        }
        
        auto& CharacterDecoderIssues = TheCharacterDecoder->getIssues();
        for (auto& I : CharacterDecoderIssues) {
            issues.push_back(std::move(I));
        }
        
        auto& ByteDecoderIssues = TheByteDecoder->getIssues();
        for (auto& I : ByteDecoderIssues) {
            issues.push_back(std::move(I));
        }
#endif // !NISSUES
        
        nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(issues))));
    }
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

Node *ParserSession::tokenize() {
    
    std::vector<NodePtr> nodes;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        auto Tok = TheTokenizer->currentToken(TOPLEVEL);
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            break;
        }
        
        auto N = NodePtr(new LeafNode(Tok));
        
        nodes.push_back(std::move(N));
        
        TheTokenizer->nextToken(TOPLEVEL);
        
    } // while (true)
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

Node *ParserSession::listSourceCharacters() {
    
    std::vector<NodePtr> nodes;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        //        TheByteDecoder->nextSourceCharacter(TOPLEVEL);
        
        auto Char = TheByteDecoder->nextSourceCharacter0(TOPLEVEL);
        
        if (Char.isEndOfFile()) {
            break;
        }
        
        auto N = NodePtr(new SourceCharacterNode(Char));
        
        nodes.push_back(std::move(N));
        
    } // while (true)
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

NodePtr ParserSession::parseLeaf0(int mode) {
    
    switch (mode) {
        case 0: {
            auto Tok = TheTokenizer->currentToken(TOPLEVEL);
            auto N = LeafNodePtr(new LeafNode(Tok));
            return N;
        }
        case 1: {
            auto Tok = TheTokenizer->currentToken_stringifySymbol();
            auto N = LeafNodePtr(new LeafNode(Tok));
            return N;
        }
        case 2: {
            auto Tok = TheTokenizer->currentToken_stringifyFile();
            auto N = LeafNodePtr(new LeafNode(Tok));
            return N;
        }
        default: {
            assert(false);
            return nullptr;
        }
    }
}

Node *ParserSession::parseLeaf(int mode) {
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
        auto node = parseLeaf0(mode);
        
        exprs.push_back(std::move(node));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    //
    // Collect all issues from the various components
    //
    {
        std::vector<IssuePtr> issues;
        
#if !NISSUES
        auto& ParserIssues = TheParser->getIssues();
        for (auto& I : ParserIssues) {
            issues.push_back(std::move(I));
        }
        
        auto& TokenizerIssues = TheTokenizer->getIssues();
        for (auto& I : TokenizerIssues) {
            issues.push_back(std::move(I));
        }
        
        auto& CharacterDecoderIssues = TheCharacterDecoder->getIssues();
        for (auto& I : CharacterDecoderIssues) {
            issues.push_back(std::move(I));
        }
        
        auto& ByteDecoderIssues = TheByteDecoder->getIssues();
        for (auto& I : ByteDecoderIssues) {
            issues.push_back(std::move(I));
        }
#endif // !NISSUES
        
        nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(issues))));
    }
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

//
// Does the file currently have permission to be read?
//
bool validatePath(WolframLibraryData libData, BufferAndLength bufAndLen) {
    
    if (!libData) {
        //
        // If running as a stand-alone executable, then always valid
        //
        return true;
    }
    
    auto inStr1 = reinterpret_cast<const char *>(bufAndLen.buffer);
    
    auto inStr2 = const_cast<char *>(inStr1);
    
    auto valid = libData->validatePath(inStr2, 'R');
    return valid;
}

void ParserSession::releaseNode(Node *N) {
    delete N;
}

#if !NABORT
bool ParserSession::isAbort() const {
    if (!currentAbortQ) {
        return false;
    }
    
    return currentAbortQ();
}

NodePtr ParserSession::handleAbort() const {
    
    auto buf = TheByteBuffer->buffer;
    auto loc = TheByteDecoder->SrcLoc;
    
    auto A = Token(TOKEN_ERROR_ABORTED, BufferAndLength(buf), Source(loc));
    
    auto Aborted = NodePtr(new LeafNode(A));
    
    return Aborted;
}
#endif // !NABORT

ParserSessionPtr TheParserSession = nullptr;




DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    
    TheParserSession = ParserSessionPtr(new ParserSession);
    
    return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    
    TheParserSession.reset(nullptr);
}

#if USE_MATHLINK
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
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
    
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount(), false);
    
    TheParserSession->init(bufAndLen, libData, INCLUDE_SOURCE);
    
    auto N = TheParserSession->parseExpressions();
    
    N->put(mlp);
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
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
    
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount(), false);
    
    TheParserSession->init(bufAndLen, libData, INCLUDE_SOURCE);
    
    auto N = TheParserSession->tokenize();
    
    N->put(mlp);
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int TokenizeBytes_Listable_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    len = static_cast<size_t>(mlLen);
    
    auto arrs = std::vector<ScopedMLByteArrayPtr>();
    arrs.reserve(len);
    
    for (size_t i = 0; i < len; i++) {
        
        auto arr = ScopedMLByteArrayPtr(new ScopedMLByteArray(mlp));
        if (!arr->read()) {
            return LIBRARY_FUNCTION_ERROR;
        }
        
        arrs.push_back(std::move(arr));
    }
    
    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), mlLen)) {
        assert(false);
    }
    for (size_t i = 0; i < len; i++) {
        
        const auto& arr = arrs[i];
        
        auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount(), false);
        
        TheParserSession->init(bufAndLen, libData, INCLUDE_SOURCE);
        
        auto N = TheParserSession->tokenize();
        
        N->put(mlp);
        
        TheParserSession->releaseNode(N);
        
        TheParserSession->deinit();
    }
    
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int ParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    std::string unescaped;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &mlLen))  {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 2) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto inStr = ScopedMLUTF8StringPtr(new ScopedMLUTF8String(mlp));
    if (!inStr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mode;
    if (!MLGetInteger(mlp, &mode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bufAndLen = BufferAndLength(inStr->get(), inStr->getByteCount(), false);
    
    TheParserSession->init(bufAndLen, libData, INCLUDE_SOURCE);
    
    auto N = TheParserSession->parseLeaf(mode);
    
    N->put(mlp);
    
    TheParserSession->releaseNode(N);
    
    TheParserSession->deinit();
    
    return LIBRARY_NO_ERROR;
}

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


ScopedMLEnvironmentParameter::ScopedMLEnvironmentParameter() : p(MLNewParameters(MLREVISION, MLAPIREVISION)) {}

ScopedMLEnvironmentParameter::~ScopedMLEnvironmentParameter() {
    MLReleaseParameters(p);
}

MLEnvironmentParameter ScopedMLEnvironmentParameter::get() {
    return p;
}

ScopedMLLoopbackLink::ScopedMLLoopbackLink() : mlp(NULL) {
    
    MLENV ep;
    ScopedMLEnvironmentParameterPtr p;
    int err;
    
#ifdef WINDOWS_MATHLINK
    
#else
    //
    // Needed because MathLink intercepts all signals
    //
    MLDoNotHandleSignalParameter(p.get(), SIGINT);
#endif
    
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
}

MLINK ScopedMLLoopbackLink::get() {
    return mlp;
}

#endif // USE_MATHLINK

