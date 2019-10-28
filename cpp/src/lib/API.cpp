
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

void putExpressions(std::vector<NodePtr>, MLINK mlp);

std::vector<NodePtr> parseExpressions();
std::vector<NodePtr> tokenize();
NodePtr parseLeaf();

bool validatePath(WolframLibraryData libData, const char *inStr);

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
    if (len != 3) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
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
    
    auto valid = validatePath(libData, inStr.get());
    if (!valid) {
        return res;
    }
    
    auto style = Utils::parseSourceStyle(reinterpret_cast<const char *>(styleStr.get()));
    
    auto skipFirstLine = Utils::parseBooleanSymbol(skipFirstLineSym.get());
    
    ScopedIFS ifs(inStr.get());
    
    if (ifs.fail()) {
        return res;
    }
    
    TheParserSession->init(libData, ifs.getData(), ifs.getDataLength(), style, false, false, skipFirstLine);
    
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
    if (len != 2) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
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
    
    auto style = Utils::parseSourceStyle(reinterpret_cast<const char *>(styleStr.get()));
    
    TheParserSession->init(libData, inStr.get(), inStr.size(), style, false, false, false);
    
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
    if (len != 2) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
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
    
    auto style = Utils::parseSourceStyle(reinterpret_cast<const char *>(styleStr.get()));
    
    TheParserSession->init(libData, inStr.get(), inStr.size(), style, false, false, false);
    
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
    if (len != 2) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
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
    
    auto valid = validatePath(libData, inStr.get());
    if (!valid) {
        return res;
    }
    
    ScopedIFS ifs(inStr.get());
    
    if (ifs.fail()) {
        return res;
    }
    
    auto style = Utils::parseSourceStyle(reinterpret_cast<const char *>(styleStr.get()));
    
    TheParserSession->init(libData, ifs.getData(), ifs.getDataLength(), style, false, false, false);
    
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
    if (len != 4) {
        return res;
    }
    
    ScopedMLString inStr(mlp);
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
    
    auto style = Utils::parseSourceStyle(reinterpret_cast<const char *>(styleStr.get()));
    
    auto stringifyNextTokenSymbol = Utils::parseBooleanSymbol(stringifyNextTokenSymbolSym.get());
    
    auto stringifyNextTokenFile = Utils::parseBooleanSymbol(stringifyNextTokenFileSym.get());
    
    TheParserSession->init(libData, inStr.get(), inStr.size(), style, stringifyNextTokenSymbol, stringifyNextTokenFile, false);
    
    auto node = parseLeaf();
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
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
        
        auto& SourceManagerIssues = TheSourceManager->getIssues();
        for (auto& I : SourceManagerIssues) {
            issues.push_back(std::move(I));
        }
        
        nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(issues))));
    }
    
    putExpressions(std::move(nodes), mlp);
    
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
        
        auto& SourceManagerIssues = TheSourceManager->getIssues();
        for (auto& I : SourceManagerIssues) {
            issues.push_back(std::move(I));
        }
        
        nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(issues))));
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
        
        if (Tok.Tok() == TOKEN_ENDOFFILE) {
            break;
        }
        
        auto N = NodePtr(new LeafNode(Tok));
        
        nodes.push_back(std::move(N));
        
        TheTokenizer->nextToken(Ctxt);
        
    } // while (true)
    
    return nodes;
}

NodePtr parseLeaf() {
    
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

//
// Does the file currently have permission to be read?
//
bool validatePath(WolframLibraryData libData, const char *inStr) {
    if (!libData) {
        //
        // If running as a stand-alone executable, then always valid
        //
        return true;
    }
    
    auto valid = libData->validatePath(const_cast<char *>(inStr), 'R');
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

void ParserSession::init(WolframLibraryData libData, const char *data, size_t dataLen, SourceStyle sourceStyle, bool stringifyNextTokenSymbol, bool stringifyNextTokenFile, bool skipFirstLine) {
    
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

std::unique_ptr<ParserSession> TheParserSession = nullptr;




MLSession::MLSession() : inited(false), ep(), mlp() {
    
    MLEnvironmentParameter p = MLNewParameters(MLREVISION, MLAPIREVISION);
#ifdef WINDOWS_MATHLINK
    
#else
    //
    // Needed because MathLink intercepts all signals
    //
    MLDoNotHandleSignalParameter(p, SIGINT);
#endif
    ep = MLInitialize(p);
    if (ep == (MLENV)0) {
        
        MLReleaseParameters(p);
        
        return;
    }
    
    int err;
    mlp = MLLoopbackOpen(ep, &err);
    
    MLReleaseParameters(p);
    
    inited = true;
}

MLSession::~MLSession() {
    if (!inited) {
        return;
    }
    
    MLClose(mlp);
    
    MLDeinitialize(ep);
}


