
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

void putExpressions(MLINK mlp);
std::vector<std::shared_ptr<Node>> parseExpressions();


DLLEXPORT mint WolframLibrary_getVersion() {
	return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    
    TheCharacterDecoder = new CharacterDecoder();
    TheSourceManager = new SourceManager();
    TheByteEncoder = new ByteEncoder();
    TheTokenizer = new Tokenizer();
    TheParser = new Parser();
    
	return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    
    delete TheCharacterDecoder;
    delete TheSourceManager;
    delete TheByteEncoder;
    delete TheParser;
    delete TheTokenizer;
}

DLLEXPORT int ConcreteParseFile(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const unsigned char *inStr = NULL;
	const char *skipFirstLineSym = NULL;
	
	if ( !MLTestHead( mlp, SYMBOL_LIST.name(), &len)) 
		goto retPt;
	if ( len != 2) 
		goto retPt;

	int b;
	int c;
	if(! MLGetUTF8String(mlp, &inStr, &b, &c))
		goto retPt;

	if(! MLGetSymbol(mlp, &skipFirstLineSym))
		goto retPt;


	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	// blah;
	{
	auto skipFirstLine = (strcmp(skipFirstLineSym, SYMBOL_TRUE.name()) == 0);

	std::ifstream ifs(reinterpret_cast<const char *>(inStr), std::ifstream::in);
        
	if (ifs.fail()) {
	   goto retPt;
	}
        
        TheSourceManager->init();
        TheByteDecoder = new ByteDecoder(ifs);
        TheCharacterDecoder->init();
        TheTokenizer->init(skipFirstLine);
        TheParser->init();
        
        putExpressions(mlp);
       	
        TheParser->deinit();
        TheTokenizer->deinit();
        TheCharacterDecoder->deinit();
        delete TheByteDecoder;
        TheSourceManager->deinit();

	}
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseUTF8String(mlp, inStr, b);
	if ( skipFirstLineSym != NULL)
		MLReleaseSymbol(mlp, skipFirstLineSym);
	return res;
}

DLLEXPORT int ConcreteParseString(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const unsigned char *inStr = NULL;

    if ( !MLTestHead( mlp, SYMBOL_LIST.name(), &len)) {
		goto retPt;
    }
    if ( len != 1) {
		goto retPt;
    }
    
	int b;
	int c;
	if(! MLGetUTF8String(mlp, &inStr, &b, &c))
		goto retPt;


	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	// blah;
	{
	auto skipFirstLine = false;
	auto iss = std::stringstream(reinterpret_cast<const char *>(inStr));

			TheSourceManager->init();
        TheByteDecoder = new ByteDecoder(iss);
        TheTokenizer->init(skipFirstLine);
        TheParser->init();
        
        putExpressions(mlp);

        TheParser->deinit();
        TheTokenizer->deinit();
        delete TheByteDecoder;
        TheSourceManager->deinit();

	}
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseUTF8String(mlp, inStr, b);
	return res;
}



void putExpressions(MLINK mlp) {

	auto nodes = parseExpressions();
   
	if(!MLPutFunction(mlp, SYMBOL_LIST.name(), nodes.size()))
		goto retPt;

	for (std::shared_ptr<Node> node : nodes) {
		node->put(mlp);
	}

retPt:
	return;
}

std::vector<std::shared_ptr<Node>> parseExpressions() {

    std::vector<std::shared_ptr<Node>> nodes;

    ParserContext Ctxt{0, 0, PRECEDENCE_LOWEST, true};

    while (true) {
        
        auto peek = TheParser->currentToken();
        
        while (peek == TOKEN_NEWLINE) {
            
            // Clear String
            TheParser->getString();
            
            peek = TheParser->nextToken(Ctxt, NEXTTOKEN_DISCARD_TOPLEVEL_NEWLINES);
        }
        
        if (peek != TOKEN_EOF) {
            
            auto Expr = TheParser->parseTopLevel();
            
            //
            // Do not check:
            // assert(TheParser->getString().empty());
            // assert(TheParser->getIssues().empty());
            // here.
            // There may be multiple expressions to parse, and after parsing the first expression, the first token of the second expression has been queued.
            // There may be a message from this token which will be handled when the second expression is parsed.
            
            nodes.push_back(Expr);
        }

        peek = TheParser->currentToken();
        
        if (peek == TOKEN_EOF) {
            break;
        }
        
    } // while (true)

    assert(TheParser->getString().empty());
    assert(TheParser->getIssues().empty());

    return nodes;
}



