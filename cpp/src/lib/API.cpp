
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
#include <cstring> // for strcmp with GCC and MSVC
#ifdef WINDOWS_MATHLINK
#else
#include <signal.h>
#endif

class Node;

// MSVC: error C2338: The C++ Standard forbids containers of const elements because allocator<const T> is ill-formed.
using NodePtr = std::unique_ptr<Node>;

void putExpressions(std::vector<NodePtr>, MLINK mlp);

std::vector<NodePtr> parseExpressions();
std::vector<NodePtr> tokenize();
LeafNodePtr parseLeaf();

bool validatePath(WolframLibraryData libData, const unsigned char *inStr);

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

DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len))  {
        return res;
    }
    if (len != 2) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    ScopedMLSymbol skipFirstLineSym(mlp);
    if (!skipFirstLineSym.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp)) {
        return res;
    }
    
    auto valid = validatePath(libData, inStr.get());
    if (!valid) {
        return res;
    }
    
    auto skipFirstLine = (strcmp(skipFirstLineSym.get(), SYMBOL_TRUE->name()) == 0);
    
    ScopedIFS ifs(inStr.get());
    
    if (ifs.fail()) {
        return res;
    }
    
    TheParserSession->init(libData, ifs, skipFirstLine);
    
    auto nodes = parseExpressions();
    
    putExpressions(std::move(nodes), mlp);
    
    TheParserSession->deinit();
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT int ConcreteParseString_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 1) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto skipFirstLine = false;
    auto iss = std::stringstream(reinterpret_cast<const char *>(inStr.get()));
    
    TheParserSession->init(libData, iss, skipFirstLine);
    
    auto nodes = parseExpressions();
    
    putExpressions(std::move(nodes), mlp);
    
    TheParserSession->deinit();
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT int TokenizeString_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 1) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto skipFirstLine = false;
    auto iss = std::stringstream(reinterpret_cast<const char *>(inStr.get()));
    
    TheParserSession->init(libData, iss, skipFirstLine);
    
    std::vector<NodePtr> nodes = tokenize();
    
    putExpressions(std::move(nodes), mlp);
    
    TheParserSession->deinit();
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT int TokenizeFile_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead( mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 1) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto valid = validatePath(libData, inStr.get());
    if (!valid) {
        return res;
    }
    
    auto skipFirstLine = false;
    ScopedIFS ifs(inStr.get());
    
    if (ifs.fail()) {
        return res;
    }
    
    TheParserSession->init(libData, ifs, skipFirstLine);
    
    std::vector<NodePtr> nodes = tokenize();
    
    putExpressions(std::move(nodes), mlp);
    
    TheParserSession->deinit();
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT int ParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    std::string unescaped;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len))  {
        return res;
    }
    if (len != 1) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
    if (!inStr.read()) {
        return res;
    }
    
    auto iss = std::stringstream(reinterpret_cast<const char *>(inStr.get()));
    
    TheParserSession->init(libData, iss, false);
    
    auto node = parseLeaf();
    
    node->put(mlp);
    
    TheParserSession->deinit();
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

void putExpressions(std::vector<NodePtr> nodes, MLINK mlp) {
    
    //
    // Check if isAbort() before calling MathLink
    //
    if (TheParser->isAbort()) {
        
        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 0)) {
            return;
        }
        
        return;
    }
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(nodes.size()))) {
        return;
    }
    
    for (auto& node : nodes) {
        node->put(mlp);
    }
}

std::vector<NodePtr> parseExpressions() {
    
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
            
            if (peek.Tok == TOKEN_ENDOFFILE) {
                break;
            }
            
            if (peek.Tok == TOKEN_WHITESPACE ||
                peek.Tok == TOKEN_NEWLINE ||
                peek.Tok == TOKEN_COMMENT ||
                peek.Tok == TOKEN_LINECONTINUATION) {
                
                exprs.push_back(std::unique_ptr<LeafNode>(new LeafNode(peek)));
                
                TheParser->nextToken(Ctxt);
                
                continue;
            }
            
            auto Expr = TheParser->parse(Ctxt);
            
            exprs.push_back(std::move(Expr));
            
        } // while (true)
        
        NodePtr Collected = std::unique_ptr<Node>(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    //
    // Now handle the out-of-band expressions, i.e., issues and metadata
    //
    
    //
    // Collect all issues from the various components
    //
    {
        std::vector<SyntaxIssue> issues;
        
        auto ParserIssues = TheParser->getIssues();
        std::copy(ParserIssues.begin(), ParserIssues.end(), std::back_inserter(issues));
        
        auto TokenizerIssues = TheTokenizer->getIssues();
        std::copy(TokenizerIssues.begin(), TokenizerIssues.end(), std::back_inserter(issues));
        
        auto CharacterDecoderIssues = TheCharacterDecoder->getIssues();
        std::copy(CharacterDecoderIssues.begin(), CharacterDecoderIssues.end(), std::back_inserter(issues));
        
        auto ByteDecoderIssues = TheByteDecoder->getIssues();
        std::copy(ByteDecoderIssues.begin(), ByteDecoderIssues.end(), std::back_inserter(issues));
        
        auto SourceManagerIssues = TheSourceManager->getIssues();
        std::copy(SourceManagerIssues.begin(), SourceManagerIssues.end(), std::back_inserter(issues));
        
        nodes.push_back(std::unique_ptr<Node>(new CollectedSyntaxIssuesNode(issues)));
    }
    
    return nodes;
}


std::vector<NodePtr> tokenize() {
    
    std::vector<NodePtr> nodes;
    
    TokenizerContext Ctxt;
    
    while (true) {
        
        //
        // No need to check isAbort() inside tokenizer loops
        //
        
        
        auto Tok = TheTokenizer->currentToken();
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            break;
        }
        
        auto N = std::unique_ptr<Node>(new LeafNode(Tok));
        
        nodes.push_back(std::move(N));
        
        TheTokenizer->nextToken(Ctxt);
        
    } // while (true)
    
    return nodes;
}

LeafNodePtr parseLeaf() {
    
    TokenizerContext Ctxt;
    
    auto Tok = TheTokenizer->currentToken();
    
    auto N = std::unique_ptr<LeafNode>(new LeafNode(Tok));
    
    Tok = TheTokenizer->nextToken(Ctxt);
    
    //
    // There may be more input
    // For example, parsing f.m would first return f as a Symbol
    // Still need to grab . and m to finish the parse
    //
    // Also handle TOKEN_ERROR_EMPTYSTRING here because we want << to return TOKEN_LESSLESS, not TOKEN_OTHER
    //
    if (!(Tok.Tok == TOKEN_ENDOFFILE ||
          Tok.Tok == TOKEN_ERROR_EMPTYSTRING)) {
        
        auto AccumTok = N->getToken();
        auto AccumStr = AccumTok.Str;
        
        auto Str = Tok.Str;
        AccumStr = AccumStr + Str;
        
        Tok = TheTokenizer->nextToken(Ctxt);
        
        while (!(Tok.Tok == TOKEN_ENDOFFILE ||
                 Tok.Tok == TOKEN_ERROR_EMPTYSTRING)) {
            
            auto Str = Tok.Str;
            AccumStr = AccumStr + Str;
            
            Tok = TheTokenizer->nextToken(Ctxt);
        }
        
        //
        // Other is invented, so invent source
        //
        auto StartSpan = N->getSourceSpan().lines.start;
        auto EndSpan = StartSpan + AccumStr.size() - 1;
        
        auto OtherTok = Token(TOKEN_OTHER, AccumStr, Source(StartSpan, EndSpan));
        auto OtherLeaf = std::unique_ptr<LeafNode>(new LeafNode(OtherTok));
        return OtherLeaf;
    }
    
    //
    // Simple leaf
    //
    return N;
}

//
// Does the file currently have permission to be read?
//
bool validatePath(WolframLibraryData libData, const unsigned char *inStr) {
    if (!libData) {
        //
        // If running as a stand-alone executable, then always valid
        //
        return true;
    }
    
    auto valid = libData->validatePath(const_cast<char *>(reinterpret_cast<const char *>(inStr)), 'R');
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

void ParserSession::init(WolframLibraryData libData, std::istream& is, bool skipFirstLine) {
    
    TheSourceManager->init(is, libData);
    TheByteDecoder->init();
    TheCharacterDecoder->init();
    TheTokenizer->init(skipFirstLine);
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

std::unique_ptr<ParserSession> TheParserSession = nullptr;




MLSession::MLSession() {
    
    inited = false;
    
    MLEnvironmentParameter p;
    int err;
    
    p = MLNewParameters(MLREVISION, MLAPIREVISION);
#ifdef WINDOWS_MATHLINK
    
#else
    //
    // Needed because MathLink intercepts all signals
    //
    MLDoNotHandleSignalParameter(p, SIGINT);
#endif
    ep = MLInitialize(p);
    if (ep == (MLENV)0) {
        return;
    }
    
    mlp = MLLoopbackOpen(ep, &err);
    
    inited = true;
}

MLSession::~MLSession() {
    if (!inited) {
        return;
    }
    if (mlp != nullptr) {
        MLClose(mlp);
    }
    if (ep != 0) {
        MLDeinitialize(ep);
    }
}


