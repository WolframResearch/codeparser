
#include "API.h"

#include "Parser.h"
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "SourceManager.h"
#include "Utils.h"
#include "Symbol.h"

#include <functional> // for function with GCC and MSVC
#include <memory> // for unique_ptr
#ifdef WINDOWS_MATHLINK
#else
#include <signal.h>
#endif

class Node;

// MSVC: error C2338: The C++ Standard forbids containers of const elements because allocator<const T> is ill-formed.
using NodePtr = std::unique_ptr<Node>;

Node *parseExpressions();
Node *tokenize();
Node *parseLeaf();

bool validatePath(WolframLibraryData libData, const unsigned char *inStr, size_t len);

DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    
    TheParserSession = std::unique_ptr<ParserSession>(new ParserSession);
    
    return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    
    TheParserSession.reset(nullptr);
}

DLLEXPORT Node *ConcreteParseFile(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr, const char *skipFirstLineSym) {
    
    auto valid = validatePath(libData, input, len);
    if (!valid) {
        return nullptr;
    }
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    auto skipFirstLine = Utils::parseBooleanSymbol(skipFirstLineSym);
    
    ScopedFileBuffer fb(input, len);
    
    if (fb.fail()) {
        return nullptr;
    }
    
    TheParserSession->init(libData, fb.getBuf(), fb.getLen(), style, false, false, skipFirstLine);
    
    auto N = parseExpressions();
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len))  {
        return res;
    }
    if (len != 3) {
        return res;
    }
    
    ScopedMLUTF8String inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    ScopedMLString styleStr(mlp);
    if (!styleStr.read()) {
        return res;
    }
    
    ScopedMLSymbol skipFirstLineSym(mlp);
    if (!skipFirstLineSym.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp)) {
        return res;
    }
    
    auto N = ConcreteParseFile(libData, inStr.get(), inStr.getByteCount(), styleStr.get(), skipFirstLineSym.get());
    
    N->put(mlp);
    
    ReleaseNode(N);
    
    TheParserSession->deinit();
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT Node *ConcreteParseString(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr) {
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    TheParserSession->init(libData, input, len, style, false, false, false);
    
    auto N = parseExpressions();
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT int ConcreteParseString_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 2) {
        return res;
    }
    
    ScopedMLUTF8String inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    ScopedMLString styleStr(mlp);
    if (!styleStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto N = ConcreteParseString(libData, inStr.get(), inStr.getByteCount(), styleStr.get());
    
    N->put(mlp);
    
    ReleaseNode(N);
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT Node *TokenizeString(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr) {
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    TheParserSession->init(libData, input, len, style, false, false, false);
    
    auto N = tokenize();
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT int TokenizeString_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 2) {
        return res;
    }
    
    ScopedMLUTF8String inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    ScopedMLString styleStr(mlp);
    if (!styleStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto N = TokenizeString(libData, inStr.get(), inStr.getByteCount(), styleStr.get());
    
    N->put(mlp);
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT Node *TokenizeFile(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr, const char *skipFirstLineSym) {
    
    auto valid = validatePath(libData, input, len);
    if (!valid) {
        return nullptr;
    }
    
    ScopedFileBuffer fb(input, len);
    
    if (fb.fail()) {
        return nullptr;
    }
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    TheParserSession->init(libData, fb.getBuf(), fb.getLen(), style, false, false, false);
    
    auto N = tokenize();
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT int TokenizeFile_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead( mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 2) {
        return res;
    }
    
    ScopedMLUTF8String inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    ScopedMLString styleStr(mlp);
    if (!styleStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto N = TokenizeFile(libData, inStr.get(), inStr.getByteCount(), styleStr.get(), "False");
    
    N->put(mlp);
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT Node *ParseLeaf(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr, const char *stringifyNextTokenSymbolSym, const char *stringifyNextTokenFileSym) {
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    auto stringifyNextTokenSymbol = Utils::parseBooleanSymbol(stringifyNextTokenSymbolSym);
    
    auto stringifyNextTokenFile = Utils::parseBooleanSymbol(stringifyNextTokenFileSym);
    
    TheParserSession->init(libData, input, len, style, stringifyNextTokenSymbol, stringifyNextTokenFile, false);
    
    auto N = parseLeaf();
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT int ParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    std::string unescaped;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len))  {
        return res;
    }
    if (len != 4) {
        return res;
    }
    
    ScopedMLUTF8String inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    ScopedMLString styleStr(mlp);
    if (!styleStr.read()) {
        return res;
    }
    
    ScopedMLSymbol stringifyNextTokenSymbolSym(mlp);
    if (!stringifyNextTokenSymbolSym.read()) {
        return res;
    }
    
    ScopedMLSymbol stringifyNextTokenFileSym(mlp);
    if (!stringifyNextTokenFileSym.read()) {
        return res;
    }
    
    auto N = ParseLeaf(libData, inStr.get(), inStr.getByteCount(), styleStr.get(), stringifyNextTokenSymbolSym.get(), stringifyNextTokenFileSym.get());
    
    N->put(mlp);
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT void ReleaseNode(Node *node) {

    delete node;
}

Node *parseExpressions() {
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
        ParserContext Ctxt;
        
        while (true) {
            
            if (TheParser->isAbort()) {
                
                break;
            }
            
            auto peek = TheParser->currentToken();
            
            if (peek.Tok() == TOKEN_ENDOFFILE) {
                break;
            }
            
            if (peek.isTrivia()) {
                
                exprs.push_back(LeafNodePtr(new LeafNode(std::move(peek))));
                
                TheParser->nextToken(Ctxt);
                
                continue;
            }
            
            if (!TheParser->isPossibleBeginningOfExpression(Ctxt)) {
                
                auto NotPossible = TheParser->handleNotPossible(peek, Ctxt, nullptr);
                
                exprs.push_back(std::move(NotPossible));
                
                continue;
            }
            
            auto Expr = TheParser->parse(Ctxt);
            
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
        std::vector<std::unique_ptr<Issue>> issues;
        
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
        
        auto SourceManagerIssues = TheSourceManager->getIssues();
        for (auto& I : SourceManagerIssues) {
            issues.push_back(std::move(I));
        }
        
        nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(issues))));
    }
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

Node *tokenize() {
    
    std::vector<NodePtr> nodes;
    
    TokenizerContext Ctxt;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        auto Tok = TheTokenizer->currentToken();
        
        if (Tok.Tok() == TOKEN_ENDOFFILE) {
            break;
        }
        
        auto N = NodePtr(new LeafNode(Tok));
        
        nodes.push_back(std::move(N));
        
        TheTokenizer->nextToken(Ctxt);
        
    } // while (true)
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

NodePtr parseLeaf0() {
    
    ParserContext PCtxt;
    
    auto Tok = TheTokenizer->currentToken();
    
    auto N = LeafNodePtr(new LeafNode(Tok));
    
    TheParser->nextToken(PCtxt);
    
    Tok = TheParser->currentToken();
    
    //
    // There may be more input
    // For example, parsing f.m would first return f as a Symbol
    // Still need to grab . and m to finish the parse
    //
    // Also handle TOKEN_ERROR_EMPTYSTRING here because we want << to return TOKEN_LESSLESS, not TOKEN_OTHER
    //
    if (!(Tok.Tok() == TOKEN_ENDOFFILE ||
          Tok.Tok() == TOKEN_ERROR_EMPTYSTRING)) {
        
        auto AccumTok = N->getToken();
        auto AccumStr = AccumTok.Str;
        
        auto allWhitespace = (AccumTok.T == TOKEN_WHITESPACE);
        
        auto Str = Tok.Str;
        AccumStr = AccumStr + Str;
        
        TheParser->nextToken(PCtxt);
        
        Tok = TheParser->currentToken();
        
        while (!(Tok.Tok() == TOKEN_ENDOFFILE ||
                 Tok.Tok() == TOKEN_ERROR_EMPTYSTRING)) {
            
            auto Str = Tok.Str;
            AccumStr = AccumStr + Str;
            
            allWhitespace = allWhitespace && (Tok.T == TOKEN_WHITESPACE);
            
            TheParser->nextToken(PCtxt);
            
            Tok = TheParser->currentToken();
        }
        
        //
        // Other is invented, so also invent source
        //
        
        auto NSrc = N->getSource();
        
        auto Start = NSrc.start();
        auto End = Start + AccumStr.size() - 1;
        
        if (allWhitespace) {
            //
            // Convenience here, any amount of whitespace will be treated as a single token
            //
            auto WhiteSpaceTok = Token(TOKEN_WHITESPACE, std::move(AccumStr), Source(Start, End));
            auto WhiteSpaceLeaf = LeafNodePtr(new LeafNode(WhiteSpaceTok));
            return WhiteSpaceLeaf;
        }
        
        auto OtherTok = Token(TOKEN_OTHER, std::move(AccumStr), Source(Start, End));
        auto OtherLeaf = LeafNodePtr(new LeafNode(OtherTok));
        return OtherLeaf;
    }
    
    //
    // Simple leaf
    //
    return N;
}

Node *parseLeaf() {
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
        auto node = parseLeaf0();
        
        exprs.push_back(std::move(node));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    //
    // Collect all issues from the various components
    //
    {
        std::vector<std::unique_ptr<Issue>> issues;
        
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
        
        auto SourceManagerIssues = TheSourceManager->getIssues();
        for (auto& I : SourceManagerIssues) {
            issues.push_back(std::move(I));
        }
        
        nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(issues))));
    }
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

//
// Does the file currently have permission to be read?
//
bool validatePath(WolframLibraryData libData, const unsigned char *inStrIn, size_t len) {
    
    if (!libData) {
        //
        // If running as a stand-alone executable, then always valid
        //
        return true;
    }
    
    auto inStr1 = reinterpret_cast<const char *>(inStrIn);
    
    auto inStr2 = const_cast<char *>(inStr1);
    
    auto valid = libData->validatePath(inStr2, 'R');
    return valid;
}

ParserSession::ParserSession() {
    
    allocSymbols();
    
    TheByteDecoder = std::unique_ptr<ByteDecoder>(new ByteDecoder());
    TheSourceManager = std::unique_ptr<SourceManager>(new SourceManager());
    TheCharacterDecoder = std::unique_ptr<CharacterDecoder>(new CharacterDecoder());
    TheTokenizer = std::unique_ptr<Tokenizer>(new Tokenizer());
    TheParser = std::unique_ptr<Parser>(new Parser());
}

ParserSession::~ParserSession() {
    
    TheParser.reset(nullptr);
    TheTokenizer.reset(nullptr);
    TheSourceManager.reset(nullptr);
    TheCharacterDecoder.reset(nullptr);
    TheByteDecoder.reset(nullptr);
    
    freeSymbols();
}

void ParserSession::init(WolframLibraryData libData, const unsigned char *data, size_t dataLen, SourceStyle sourceStyle, bool stringifyNextTokenSymbol, bool stringifyNextTokenFile, bool skipFirstLine) {
    
    TheSourceManager->init(data, dataLen, sourceStyle, libData);
    TheByteDecoder->init();
    TheCharacterDecoder->init(libData);
    TheTokenizer->init(sourceStyle, stringifyNextTokenSymbol, stringifyNextTokenFile, skipFirstLine);
    TheParser->init( [libData]() {
        if (!libData) {
            return false;
        }
        //
        // For some reason, AbortQ() returns a mint
        //
        bool res = libData->AbortQ();
        return res;
    }, { } );
}

void ParserSession::deinit() {
    
    TheParser->deinit();
    TheTokenizer->deinit();
    TheCharacterDecoder->deinit();
    TheByteDecoder->deinit();
    TheSourceManager->deinit();
}


MLSession::MLSession() : inited(false), ep(), mlp() {
    
    ScopedMLEnvironmentParameter p;
    
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
    
    int err;
    mlp = MLLoopbackOpen(ep, &err);
    
    inited = true;
}

MLSession::~MLSession() {
    if (!inited) {
        return;
    }
    
    MLClose(mlp);
    
    MLDeinitialize(ep);
}

MLINK MLSession::getMLINK() const {
    return mlp;
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

const unsigned char *ScopedMLUTF8String::get() const {
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
    if (func != NULL) {
        MLReleaseSymbol(mlp, func);
    }
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


ScopedMLEnvironmentParameter::ScopedMLEnvironmentParameter() : p(MLNewParameters(MLREVISION, MLAPIREVISION)) {}

ScopedMLEnvironmentParameter::~ScopedMLEnvironmentParameter() {
    MLReleaseParameters(p);
}

MLEnvironmentParameter ScopedMLEnvironmentParameter::get() {
    return p;
}


ScopedFileBuffer::ScopedFileBuffer(const unsigned char *inStrIn, size_t inLen) : buf(), len(), inited(false) {
    
    auto inStr = reinterpret_cast<const char *>(inStrIn);
    
    FILE * file = fopen(inStr, "rb");
    
    if (file == NULL) {
        return;
    }
    
    fseek(file, 0, SEEK_END);
    
    len = ftell(file);
    
    fclose(file);
    
    file = fopen(inStr, "rb");
    
    buf = new unsigned char[len];
    
    auto read = fread(buf, sizeof(unsigned char), len, file);
    
    fclose(file);
    
    if (read != len) {
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

unsigned char *ScopedFileBuffer::getBuf() const {
    return buf;
}

size_t ScopedFileBuffer::getLen() const {
    return len;
}

bool ScopedFileBuffer::fail() const {
    return !inited;
}


std::unique_ptr<ParserSession> TheParserSession = nullptr;
