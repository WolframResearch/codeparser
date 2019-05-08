
#include "API.h"

#include "Parser.h"
#include "Tokenizer.h"
#include "CharacterDecoder.h"
#include "Node.h"
#include "ByteDecoder.h"
#include "ByteEncoder.h"
#include "Precedence.h"

#include <fstream>
#include <cassert>
#include <vector>
#include <memory>
#include <cstring>

void putExpressions(std::vector<std::shared_ptr<Node>>, MLINK mlp);
std::vector<std::shared_ptr<Node>> parseExpressions();
std::vector<std::shared_ptr<Node>> tokenize();

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
    
    TheInputStream = &ifs;
        
    TheSourceManager->init();
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
    }, {} );
    
    auto nodes = parseExpressions();

    putExpressions(nodes, mlp);
    
    TheParser->deinit();
    TheTokenizer->deinit();
    TheCharacterDecoder->deinit();
    TheByteDecoder->deinit();
    TheSourceManager->deinit();

    ifs.close();
    TheInputStream = nullptr;
        
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
    
    TheInputStream = &iss;
    
    TheSourceManager->init();
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
    TheByteDecoder->deinit();
    TheCharacterDecoder->deinit();
    TheSourceManager->deinit();

    TheInputStream = nullptr;
        
	res = LIBRARY_NO_ERROR;
    }
    
retPt: 
    if (inStr != NULL) {
		MLReleaseUTF8String(mlp, inStr, b);
    }
	return res;
}

DLLEXPORT int ConcreteParseBoxes(WolframLibraryData libData, MLINK mlp) {
    
    int res = LIBRARY_FUNCTION_ERROR;

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

    TheInputStream = &iss;
        
    TheByteDecoder->init();
    TheCharacterDecoder->init();
    TheSourceManager->init();
    TheTokenizer->init(skipFirstLine);
    
    std::vector<std::shared_ptr<Node>> nodes = tokenize();

    putExpressions(nodes, mlp);
    
    TheTokenizer->deinit();
    TheSourceManager->deinit();
    TheCharacterDecoder->deinit();
    TheByteDecoder->deinit();

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

    TheInputStream = &ifs;
        
    TheByteDecoder->init();
    TheCharacterDecoder->init();
    TheSourceManager->init();
    TheTokenizer->init(skipFirstLine);
    
    std::vector<std::shared_ptr<Node>> nodes = tokenize();

    putExpressions(nodes, mlp);
    
    TheTokenizer->deinit();
    TheSourceManager->deinit();
    TheCharacterDecoder->deinit();
    TheByteDecoder->deinit();

    ifs.close();
        
    res = LIBRARY_NO_ERROR;
    }
    
retPt: 
    if (inStr != NULL) {
        MLReleaseUTF8String(mlp, inStr, b);
    }
    return res;
}


void putExpressions(std::vector<std::shared_ptr<Node>> nodes, MLINK mlp) {
   
    //
    // Check if isAbort() before calling MathLink
    // 
    if (TheParser->isAbort()) {
        return;
    }
    
    if (!MLPutFunction(mlp, SYMBOL_LIST->name(), static_cast<int>(nodes.size()))) {
		goto retPt;
    }
    
	for (std::shared_ptr<Node> node : nodes) {
		node->put(mlp);
	}

retPt:
	return;
}

std::vector<std::shared_ptr<Node>> parseExpressions() {

    std::vector<std::shared_ptr<Node>> nodes;

    
    std::vector<std::shared_ptr<Node>> exprs;
    
    ParserContext Ctxt;

    while (true) {
        
        if (TheParser->isAbort()) {
            
            break;
        }
        
        auto peek = TheParser->tryNextToken(Ctxt, DISCARD_EVERYTHING);
        
        if (peek.Tok == TOKEN_ENDOFFILE) {
            break;
        }
        
        auto Expr = TheParser->parse(Ctxt);
        
        exprs.push_back(Expr);
        
    } // while (true)
    
    nodes.push_back(std::make_shared<CollectedExpressionsNode>(exprs));
    
    
    //
    // Now handle the out-of-band expressions, i.e., comments and issues
    //
    
    
    auto Comments = TheParser->getComments();
    
    nodes.push_back(std::make_shared<CollectedCommentsNode>(Comments));
    
    
    
    //
    // Collect all issues from the various components
    //
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
    
    
    return nodes;
}


std::vector<std::shared_ptr<Node>> tokenize() {

    std::vector<std::shared_ptr<Node>> nodes;
    
    TokenizerContext Ctxt;
    
    while (true) {
        
        auto Tok = TheTokenizer->currentToken();
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            break;
        }

        auto N = std::make_shared<TokenNode>(Tok);
        nodes.push_back(N);
        
        TheTokenizer->nextToken(Ctxt);
        
    } // while (true)

    return nodes;
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
