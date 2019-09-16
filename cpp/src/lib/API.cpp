
#include "API.h"

#include "Parser.h"
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "ByteDecoder.h"
#include "SourceManager.h"
#include "Utils.h"
#include "Symbol.h"

#include <fstream>
#include <memory>

class Node;

// MSVC: error C2338: The C++ Standard forbids containers of const elements because allocator<const T> is ill-formed.
using NodePtr = const std::shared_ptr<Node>;

void putExpressions(std::vector<NodePtr>, MLINK mlp);

std::vector<NodePtr> parseExpressions();
std::vector<NodePtr> tokenize();
NodePtr parseLeaf();

bool validatePath(WolframLibraryData libData, const unsigned char *inStr);

DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    
    allocSymbols();
    
    TheByteDecoder = new ByteDecoder();
    TheSourceManager = new SourceManager();
    TheCharacterDecoder = new CharacterDecoder();
    TheTokenizer = new Tokenizer();
    TheParser = new Parser();
    
    return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    
    delete TheParser;
    delete TheTokenizer;
    delete TheSourceManager;
    delete TheCharacterDecoder;
    delete TheByteDecoder;
    
    freeSymbols();
}

DLLEXPORT int ConcreteParseFile(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    const unsigned char *inStr = NULL;
    const char *skipFirstLineSym = NULL;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len))  {
        goto retPt;
    }
    if (len != 2) {
        goto retPt;
    }
    
    int b;
    int c;
    if (!MLGetUTF8String(mlp, &inStr, &b, &c)) {
        goto retPt;
    }
    
    if (!MLGetSymbol(mlp, &skipFirstLineSym)) {
        goto retPt;
    }
    
    if (!MLNewPacket(mlp)) {
        goto retPt;
    }
    
    {
        auto valid = validatePath(libData, inStr);
        if (!valid) {
            goto retPt;
        }
    }
    
    {
        auto skipFirstLine = (strcmp(skipFirstLineSym, SYMBOL_TRUE->name()) == 0);
        
        std::ifstream ifs(reinterpret_cast<const char *>(inStr), std::ifstream::in);
        
        if (ifs.fail()) {
            goto retPt;
        }
        
        TheSourceManager->init(ifs, libData);
        TheByteDecoder->init();
        TheCharacterDecoder->init();
        TheTokenizer->init(skipFirstLine);
        TheParser->init( [&libData]() {
            if (!libData) {
                return false;
            }
            //
            // For some reason, AbortQ() returns a mint
            //
            bool res = libData->AbortQ();
            return res;
        }, { } );
        
        auto nodes = parseExpressions();
        
        putExpressions(nodes, mlp);
        
        TheParser->deinit();
        TheTokenizer->deinit();
        TheCharacterDecoder->deinit();
        TheByteDecoder->deinit();
        TheSourceManager->deinit();
        
        ifs.close();
        
        res = LIBRARY_NO_ERROR;
    }
    
retPt:
    if (inStr != NULL) {
        MLReleaseUTF8String(mlp, inStr, b);
    }
    if (skipFirstLineSym != NULL) {
        MLReleaseSymbol(mlp, skipFirstLineSym);
    }
    return res;
}

DLLEXPORT int ConcreteParseString(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    const unsigned char *inStr = NULL;
    
    if (!MLTestHead( mlp, SYMBOL_LIST->name(), &len)) {
        goto retPt;
    }
    if (len != 1) {
        goto retPt;
    }
    
    int b;
    int c;
    if (!MLGetUTF8String(mlp, &inStr, &b, &c)) {
        goto retPt;
    }
    
    if (!MLNewPacket(mlp) ) {
        goto retPt;
    }
    
    {
        auto skipFirstLine = false;
        auto iss = std::stringstream(reinterpret_cast<const char *>(inStr));
        
        TheSourceManager->init(iss, libData);
        TheByteDecoder->init();
        TheCharacterDecoder->init();
        TheTokenizer->init(skipFirstLine);
        TheParser->init( [&libData]() {
            if (!libData) {
                return false;
            }
            //
            // For some reason, AbortQ() returns a mint
            //
            bool res = libData->AbortQ();
            return res;
        }, { } );
        
        auto nodes = parseExpressions();
        
        putExpressions(nodes, mlp);
        
        TheParser->deinit();
        TheTokenizer->deinit();
        TheCharacterDecoder->deinit();
        TheByteDecoder->deinit();
        TheSourceManager->deinit();
        
        res = LIBRARY_NO_ERROR;
    }
    
retPt:
    if (inStr != NULL) {
        MLReleaseUTF8String(mlp, inStr, b);
    }
    return res;
}

DLLEXPORT int TokenizeString(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    const unsigned char *inStr = NULL;
    
    if (!MLTestHead( mlp, SYMBOL_LIST->name(), &len)) {
        goto retPt;
    }
    if (len != 1) {
        goto retPt;
    }
    
    int b;
    int c;
    if (!MLGetUTF8String(mlp, &inStr, &b, &c)) {
        goto retPt;
    }
    
    if (!MLNewPacket(mlp) ) {
        goto retPt;
    }
    
    {
        auto skipFirstLine = false;
        auto iss = std::stringstream(reinterpret_cast<const char *>(inStr));
        
        TheSourceManager->init(iss, libData);
        TheByteDecoder->init();
        TheCharacterDecoder->init();
        TheTokenizer->init(skipFirstLine);
        TheParser->init( [&libData]() {
            if (!libData) {
                return false;
            }
            //
            // For some reason, AbortQ() returns a mint
            //
            bool res = libData->AbortQ();
            return res;
        }, { } );
        
        std::vector<NodePtr> nodes = tokenize();
        
        putExpressions(nodes, mlp);
        
        TheParser->deinit();
        TheTokenizer->deinit();
        TheCharacterDecoder->deinit();
        TheByteDecoder->deinit();
        TheSourceManager->deinit();
        
        res = LIBRARY_NO_ERROR;
    }
    
retPt:
    if (inStr != NULL) {
        MLReleaseUTF8String(mlp, inStr, b);
    }
    return res;
}

DLLEXPORT int TokenizeFile(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    const unsigned char *inStr = NULL;
    
    if (!MLTestHead( mlp, SYMBOL_LIST->name(), &len)) {
        goto retPt;
    }
    if (len != 1) {
        goto retPt;
    }
    
    int b;
    int c;
    if (!MLGetUTF8String(mlp, &inStr, &b, &c)) {
        goto retPt;
    }
    
    if (!MLNewPacket(mlp) ) {
        goto retPt;
    }
    
    {
        auto valid = validatePath(libData, inStr);
        if (!valid) {
            goto retPt;
        }
    }
    
    {
        auto skipFirstLine = false;
        std::ifstream ifs(reinterpret_cast<const char *>(inStr), std::ifstream::in);
        
        if (ifs.fail()) {
            goto retPt;
        }
        
        TheSourceManager->init(ifs, libData);
        TheByteDecoder->init();
        TheCharacterDecoder->init();
        TheTokenizer->init(skipFirstLine);
        TheParser->init( [&libData]() {
            if (!libData) {
                return false;
            }
            //
            // For some reason, AbortQ() returns a mint
            //
            bool res = libData->AbortQ();
            return res;
        }, { } );
        
        std::vector<NodePtr> nodes = tokenize();
        
        putExpressions(nodes, mlp);
        
        TheParser->deinit();
        TheTokenizer->deinit();
        TheCharacterDecoder->deinit();
        TheByteDecoder->deinit();
        TheSourceManager->deinit();
        
        ifs.close();
        
        res = LIBRARY_NO_ERROR;
    }
    
retPt:
    if (inStr != NULL) {
        MLReleaseUTF8String(mlp, inStr, b);
    }
    return res;
}

DLLEXPORT int ParseLeaf(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;
    int len;
    const unsigned char *inStr = NULL;
    
    std::string unescaped;
    
    if (!MLTestHead(mlp, SYMBOL_LIST->name(), &len))  {
        goto retPt;
    }
    if (len != 1) {
        goto retPt;
    }
    
    int b;
    int c;
    if (!MLGetUTF8String(mlp, &inStr, &b, &c)) {
        goto retPt;
    }
    
    {
        auto iss = std::stringstream(reinterpret_cast<const char *>(inStr));
        
        TheSourceManager->init(iss, libData);
        TheByteDecoder->init();
        TheCharacterDecoder->init();
        TheTokenizer->init(false);
        TheParser->init( [&libData]() {
            if (!libData) {
                return false;
            }
            //
            // For some reason, AbortQ() returns a mint
            //
            bool res = libData->AbortQ();
            return res;
        }, { } );
        
        auto node = parseLeaf();
        
        node->put(mlp);
        
        TheParser->deinit();
        TheTokenizer->deinit();
        TheCharacterDecoder->deinit();
        TheByteDecoder->deinit();
        TheSourceManager->deinit();
        
        res = LIBRARY_NO_ERROR;
    }
    
retPt:
    if (inStr != NULL) {
        MLReleaseUTF8String(mlp, inStr, b);
    }
    return res;
}

void putExpressions(std::vector<NodePtr> nodes, MLINK mlp) {
    
    //
    // Check if isAbort() before calling MathLink
    //
    if (TheParser->isAbort()) {
        
        if (!MLPutFunction(mlp, SYMBOL_LIST->name(), 0)) {
            goto retPt;
        }
        
        return;
    }
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(nodes.size()))) {
        goto retPt;
    }
    
    for (NodePtr node : nodes) {
        node->put(mlp);
    }
    
retPt:
    return;
}

std::vector<NodePtr> parseExpressions() {
    
    std::vector<NodePtr> nodes;
    
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
                
                exprs.push_back(std::make_shared<LeafNode>(peek));
                
                TheParser->nextToken(Ctxt);
                
                continue;
            }
            
            auto Expr = TheParser->parse(Ctxt);
            
            exprs.push_back(Expr);
            
        } // while (true)
        
        nodes.push_back(std::make_shared<CollectedExpressionsNode>(exprs));
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
        
        nodes.push_back(std::make_shared<CollectedSyntaxIssuesNode>(issues));
    }
    
    {
        std::vector<Metadata> metas;
        
        //
        // Skip metadatas for now
        //
        
        //        auto ParserMetas = TheParser->getMetadatas();
        //        std::copy(ParserMetas.begin(), ParserMetas.end(), std::back_inserter(metas));
        //
        //        auto TokenizerMetas = TheTokenizer->getMetadatas();
        //        std::copy(TokenizerMetas.begin(), TokenizerMetas.end(), std::back_inserter(metas));
        //
        //        auto CharacterDecoderMetas = TheCharacterDecoder->getMetadatas();
        //        std::copy(CharacterDecoderMetas.begin(), CharacterDecoderMetas.end(), std::back_inserter(metas));
        //
        //        auto ByteDecoderMetas = TheByteDecoder->getMetadatas();
        //        std::copy(ByteDecoderMetas.begin(), ByteDecoderMetas.end(), std::back_inserter(metas));
        
        nodes.push_back(std::make_shared<CollectedMetadatasNode>(metas));
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
        
        auto N = std::make_shared<LeafNode>(Tok);
        nodes.push_back(N);
        
        TheTokenizer->nextToken(Ctxt);
        
    } // while (true)
    
    return nodes;
}

NodePtr parseLeaf() {
    
    TokenizerContext Ctxt;
    
    auto Tok = TheTokenizer->currentToken();
    
    auto N = std::make_shared<LeafNode>(Tok);
    
    Tok = TheTokenizer->nextToken(Ctxt);
    
    //
    // There may be more import
    // For example, parsing f.m would first return f as a Symbol
    // Still need to grab . and m to finish the parse
    //
    if (Tok.Tok != TOKEN_ENDOFFILE) {
        
        auto AccumTok = N->getToken();
        auto AccumStr = AccumTok.Str;
        
        auto Str = Tok.Str;
        AccumStr = AccumStr + Str;
        
        Tok = TheTokenizer->nextToken(Ctxt);
        
        while (Tok.Tok != TOKEN_ENDOFFILE) {
            
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
        auto OtherLeaf = std::make_shared<LeafNode>(OtherTok);
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

