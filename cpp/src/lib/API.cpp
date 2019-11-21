
#include "API.h"

#include "Parser.h"
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "ByteBuffer.h"
#include "Utils.h"
#include "Symbol.h"

#include <functional> // for function with GCC and MSVC
#include <memory> // for unique_ptr
#ifdef WINDOWS_MATHLINK
#else
#include <signal.h>
#endif
#include <vector>

bool validatePath(WolframLibraryData libData, const unsigned char *inStr, size_t len);


DLLEXPORT Node *ConcreteParseBytes(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr) {
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    TheParserSession->init(libData, input, len, style, 0);
    
    auto N = TheParserSession->parseExpressions();
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT Node *TokenizeBytes(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr) {
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    TheParserSession->init(libData, input, len, style, 0);
    
    auto N = TheParserSession->tokenize();
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT Node *ParseLeaf(WolframLibraryData libData, const unsigned char *input, size_t len, const char *styleStr, int mode) {
    
    auto style = Utils::parseSourceStyle(styleStr);
    
    TheParserSession->init(libData, input, len, style, mode);
    
    auto N = TheParserSession->parseLeaf(mode);
    
    TheParserSession->deinit();
    
    return N;
}

DLLEXPORT void ReleaseNode(Node *node) {
    
    delete node;
}


//
// in: an array of bytes that contain \r and \n
// out: a vector of the indices where each newline is
//
// Example: given the list { 'a', 'b', '\n', 'c', 'd', 'e', '\n', 'f'}
// the result would be < 2, 6 >
//
std::vector<size_t> ParserSession::offsetLineMap() {
    
    AdvancementState state;
    SourceLocation Loc(LineCol(0, 0));
    
    std::vector<size_t> V;
    
    for (size_t i = 0; i < dataLen; i++) {
        auto b = data[i];

        Loc = state.advance(SourceCharacter(b), Loc);
        
        //
        // Col == 0 means that this is a new line
        //
        if (Loc.lineCol.Col == 0) {
            V.push_back(i);
        }
    }
    
    return V;
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
            if (TheParser->isAbort()) {
                
                break;
            }
#endif
            auto peek = TheParser->currentToken();
            
            if (peek.getTokenEnum() == TOKEN_ENDOFFILE) {
                break;
            }
            
            if (peek.getTokenEnum().isTrivia()) {
                
                exprs.push_back(LeafNodePtr(new LeafNode(std::move(peek))));
                
                TheParser->nextToken(Ctxt);
                
                continue;
            }
            
            if (!peek.getTokenEnum().isPossibleBeginningOfExpression()) {
                
                auto NotPossible = TheParser->handleNotPossible(peek, peek, Ctxt, nullptr);
                
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
#endif
        
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
        
        auto Tok = TheTokenizer->currentToken();
        
        if (Tok.getTokenEnum() == TOKEN_ENDOFFILE) {
            break;
        }
        
        auto N = NodePtr(new LeafNode(Tok));
        
        nodes.push_back(std::move(N));
        
        TheTokenizer->nextToken();
        
    } // while (true)
    
    auto N = new ListNode(std::move(nodes));
    
    return N;
}

NodePtr ParserSession::parseLeaf0(int mode) {
    
    ParserContext PCtxt;
    
    auto Tok = TheTokenizer->currentToken();
    
    auto N = LeafNodePtr(new LeafNode(Tok));
    
    switch (mode) {
        case 0:
            TheParser->nextToken(PCtxt);
            break;
        case 1:
            TheParser->nextToken_stringifyNextToken_symbol(PCtxt);
            break;
        case 2:
            TheParser->nextToken_stringifyNextToken_file(PCtxt);
            break;
    }
    
    Tok = TheParser->currentToken();
    
    //
    // There may be more input
    // For example, parsing <space>f.m would first return <space>
    // Still need to grab f.m
    //
    // Also handle TOKEN_ERROR_EMPTYSTRING here because we want << to return TOKEN_LESSLESS, not TOKEN_OTHER
    //
//    if (!(Tok.getTokenEnum() == TOKEN_ENDOFFILE ||
//          Tok.getTokenEnum() == TOKEN_ERROR_EMPTYSTRING)) {
//        
//        auto AccumTok = N->getToken();
//        auto AccumStr = AccumTok.Str;
//        
//        auto allWhitespace = (AccumTok.T == TOKEN_WHITESPACE);
//        
//        auto Str = Tok.Str;
//        AccumStr = AccumStr + Str;
//        
//        if (stringifyNextToken_symbol) {
//            TheParser->nextToken_stringifyNextToken_symbol(PCtxt);
//        } else if (stringifyNextToken_file) {
//            TheParser->nextToken_stringifyNextToken_file(PCtxt);
//        } else {
//            TheParser->nextToken(PCtxt);
//        }
//        
//        Tok = TheParser->currentToken();
//        
//        while (!(Tok.getTokenEnum() == TOKEN_ENDOFFILE ||
//                 Tok.getTokenEnum() == TOKEN_ERROR_EMPTYSTRING)) {
//            
//            auto Str = Tok.Str;
//            AccumStr = AccumStr + Str;
//            
//            allWhitespace = allWhitespace && (Tok.T == TOKEN_WHITESPACE);
//            
//            if (stringifyNextToken_symbol) {
//                TheParser->nextToken_stringifyNextToken_symbol(PCtxt);
//            } else if (stringifyNextToken_file) {
//                TheParser->nextToken_stringifyNextToken_file(PCtxt);
//            } else {
//                TheParser->nextToken(PCtxt);
//            }
//            
//            Tok = TheParser->currentToken();
//        }
//        
//        //
//        // Other is invented, so also invent source
//        //
//        
//        auto NSrc = N->getSource();
//        
//        auto Start = NSrc.start();
//        auto End = Start + AccumStr.size() - 1;
//        
//        if (allWhitespace) {
//            //
//            // Convenience here, any amount of whitespace will be treated as a single token
//            //
//            auto WhiteSpaceTok = Token(TOKEN_WHITESPACE, std::move(AccumStr), Source(Start, End));
//            auto WhiteSpaceLeaf = LeafNodePtr(new LeafNode(WhiteSpaceTok));
//            return WhiteSpaceLeaf;
//        }
//        
//        auto OtherTok = Token(TOKEN_OTHER, std::move(AccumStr), Source(Start, End));
//        auto OtherLeaf = LeafNodePtr(new LeafNode(OtherTok));
//        return OtherLeaf;
//    }
    
    //
    // Simple leaf
    //
    return N;
}

Node *ParserSession::parseLeaf(int mode) {
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
        auto node = parseLeaf0(0);
        
        exprs.push_back(std::move(node));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    //
    // Collect all issues from the various components
    //
    {
        std::vector<std::unique_ptr<Issue>> issues;
        
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
#endif
        
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
    
    TheByteBuffer = std::unique_ptr<ByteBuffer>(new ByteBuffer());
    TheByteDecoder = std::unique_ptr<ByteDecoder>(new ByteDecoder());
    TheCharacterDecoder = std::unique_ptr<CharacterDecoder>(new CharacterDecoder());
    TheTokenizer = std::unique_ptr<Tokenizer>(new Tokenizer());
    TheParser = std::unique_ptr<Parser>(new Parser());
}

ParserSession::~ParserSession() {

    TheParser.reset(nullptr);
    TheTokenizer.reset(nullptr);
    TheCharacterDecoder.reset(nullptr);
    TheByteDecoder.reset(nullptr);
    TheByteBuffer.reset(nullptr);
}

void ParserSession::init(WolframLibraryData libData, const unsigned char *dataIn, size_t dataLenIn, SourceStyle sourceStyle, int mode) {
    
    data = dataIn;
    dataLen = dataLenIn;
    
    TheByteBuffer->init(dataIn, dataLenIn, libData);
    TheByteDecoder->init(sourceStyle);
    TheCharacterDecoder->init(libData, sourceStyle);
    TheTokenizer->init(sourceStyle, mode);
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
    TheByteBuffer->deinit();
}

std::unique_ptr<ParserSession> TheParserSession = nullptr;


#if USE_MATHLINK

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

DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 2) {
        return res;
    }
    
    ScopedMLByteArray arr(mlp);
    if (!arr.read()) {
        return res;
    }
    
    ScopedMLString styleStr(mlp);
    if (!styleStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto N = ConcreteParseBytes(libData, arr.get(), arr.getByteCount(), styleStr.get());
    
    N->put(mlp);
    
    ReleaseNode(N);
    
    res = LIBRARY_NO_ERROR;
    
    return res;
}

DLLEXPORT int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len)) {
        return res;
    }
    if (len != 2) {
        return res;
    }
    
    ScopedMLByteArray arr(mlp);
    if (!arr.read()) {
        return res;
    }
    
    ScopedMLString styleStr(mlp);
    if (!styleStr.read()) {
        return res;
    }
    
    if (!MLNewPacket(mlp) ) {
        return res;
    }
    
    auto N = TokenizeBytes(libData, arr.get(), arr.getByteCount(), styleStr.get());
    
    N->put(mlp);
    
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
    
    int mode;
    if (!MLGetInteger(mlp, &mode)) {
        return res;
    }
    
    auto N = ParseLeaf(libData, inStr.get(), inStr.getByteCount(), styleStr.get(), mode);
    
    N->put(mlp);
    
    res = LIBRARY_NO_ERROR;
    
    return res;
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

const unsigned char *ScopedMLByteArray::get() const {
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

#endif // USE_MATHLINK

