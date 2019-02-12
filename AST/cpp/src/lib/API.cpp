
#include "API.h"

#include "Parser.h"
#include "Precedence.h"
#include "ByteDecoder.h"
#include "ByteEncoder.h"
#include "CharacterDecoder.h"
#include "Tokenizer.h"

#include <stdlib.h>
#include <string>
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <cassert>

DLLEXPORT mint WolframLibrary_getVersion() {
	return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
	return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
	return;
}

DLLEXPORT int ConcreteParseFile(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const unsigned char *inStr = NULL;
	const char *interactiveSym = NULL;
	const char *skipFirstLineSym = NULL;
	
	if ( !MLTestHead( mlp, "List", &len)) 
		goto retPt;
	if ( len != 3) 
		goto retPt;

	int b;
	int c;
	if(! MLGetUTF8String(mlp, &inStr, &b, &c))
		goto retPt;

	if(! MLGetSymbol(mlp, &interactiveSym))
		goto retPt;

	


	if(! MLGetSymbol(mlp, &skipFirstLineSym))
		goto retPt;


	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	// blah;
	{
	auto interactive = (strcmp(interactiveSym, "True") == 0);

	auto skipFirstLine = (strcmp(skipFirstLineSym, "True") == 0);

	std::ifstream ifs(reinterpret_cast<const char *>(inStr), std::ifstream::in);
        
	if (ifs.fail()) {
	   return 1;
	}

	TheSourceManager = new SourceManager();
	TheByteDecoder = new ByteDecoder(ifs, interactive);
	TheByteEncoder = new ByteEncoder();
	TheCharacterDecoder = new CharacterDecoder();

	TheTokenizer = new Tokenizer();
   TheTokenizer->init(skipFirstLine);
   TheParser = new Parser();
   TheParser->init();
   putExpressions(mlp, interactive);

	delete TheSourceManager;
	delete TheByteDecoder;
	delete TheByteEncoder;
	delete TheCharacterDecoder;
	delete TheTokenizer;
	delete TheParser;

	}
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseUTF8String(mlp, inStr, b);
	if ( interactiveSym != NULL)
		MLReleaseSymbol(mlp, interactiveSym);
	if ( skipFirstLineSym != NULL)
		MLReleaseSymbol(mlp, skipFirstLineSym);
	return res;
}

DLLEXPORT int ConcreteParseString(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const unsigned char *inStr = NULL;
	const char *interactiveSym = NULL;
	const char *skipFirstLineSym = NULL;
	
	if ( !MLTestHead( mlp, "List", &len)) 
		goto retPt;
	if ( len != 3) 
		goto retPt;

	int b;
	int c;
	if(! MLGetUTF8String(mlp, &inStr, &b, &c))
		goto retPt;

	if(! MLGetSymbol(mlp, &interactiveSym))
		goto retPt;

	


	if(! MLGetSymbol(mlp, &skipFirstLineSym))
		goto retPt;


	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	// blah;
	{
	auto interactive = (strcmp(interactiveSym, "True") == 0);

	auto skipFirstLine = (strcmp(skipFirstLineSym, "True") == 0);

	auto iss = std::stringstream(reinterpret_cast<const char *>(inStr));

	TheSourceManager = new SourceManager();
	TheByteDecoder = new ByteDecoder(iss, interactive);
	TheByteEncoder = new ByteEncoder();
	TheCharacterDecoder = new CharacterDecoder();

	TheTokenizer = new Tokenizer();
   TheTokenizer->init(skipFirstLine);
   TheParser = new Parser();
   TheParser->init();
   putExpressions(mlp, interactive);

	delete TheSourceManager;
	delete TheByteDecoder;
	delete TheByteEncoder;
	delete TheCharacterDecoder;
	delete TheTokenizer;
	delete TheParser;

	}
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseUTF8String(mlp, inStr, b);
	if ( interactiveSym != NULL)
		MLReleaseSymbol(mlp, interactiveSym);
	if ( skipFirstLineSym != NULL)
		MLReleaseSymbol(mlp, skipFirstLineSym);
	return res;
}

DLLEXPORT int TokenizeFile(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const unsigned char *inStr = NULL;
	const char *interactiveSym = NULL;
	const char *skipFirstLineSym = NULL;
	
	if ( !MLTestHead( mlp, "List", &len)) 
		goto retPt;
	if ( len != 3) 
		goto retPt;

	int b;
	int c;
	if(! MLGetUTF8String(mlp, &inStr, &b, &c))
		goto retPt;

	if(! MLGetSymbol(mlp, &interactiveSym))
		goto retPt;

	


	if(! MLGetSymbol(mlp, &skipFirstLineSym))
		goto retPt;


	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	// blah;
	{
	auto interactive = (strcmp(interactiveSym, "True") == 0);

	auto skipFirstLine = (strcmp(skipFirstLineSym, "True") == 0);

	std::ifstream ifs(reinterpret_cast<const char *>(inStr), std::ifstream::in);
        
	if (ifs.fail()) {
	   return 1;
	}

	TheSourceManager = new SourceManager();
	TheByteDecoder = new ByteDecoder(ifs, interactive);
	TheByteEncoder = new ByteEncoder();
	TheCharacterDecoder = new CharacterDecoder();

	TheTokenizer = new Tokenizer();
   TheTokenizer->init(skipFirstLine);
   putTokens(mlp, interactive);

	delete TheSourceManager;
	delete TheByteDecoder;
	delete TheByteEncoder;
	delete TheCharacterDecoder;
	delete TheTokenizer;
	delete TheParser;

	}
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseUTF8String(mlp, inStr, b);
	if ( interactiveSym != NULL)
		MLReleaseSymbol(mlp, interactiveSym);
	if ( skipFirstLineSym != NULL)
		MLReleaseSymbol(mlp, skipFirstLineSym);
	return res;
}

DLLEXPORT int TokenizeString(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const unsigned char *inStr = NULL;
	const char *interactiveSym = NULL;
	const char *skipFirstLineSym = NULL;
	
	if ( !MLTestHead( mlp, "List", &len)) 
		goto retPt;
	if ( len != 3) 
		goto retPt;

	int b;
	int c;
	if(! MLGetUTF8String(mlp, &inStr, &b, &c))
		goto retPt;

	if(! MLGetSymbol(mlp, &interactiveSym))
		goto retPt;

	


	if(! MLGetSymbol(mlp, &skipFirstLineSym))
		goto retPt;


	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	// blah;
	{
	auto interactive = (strcmp(interactiveSym, "True") == 0);

	auto skipFirstLine = (strcmp(skipFirstLineSym, "True") == 0);

	auto iss = std::stringstream(reinterpret_cast<const char *>(inStr));

	TheSourceManager = new SourceManager();
	TheByteDecoder = new ByteDecoder(iss, interactive);
	TheByteEncoder = new ByteEncoder();
	TheCharacterDecoder = new CharacterDecoder();

	TheTokenizer = new Tokenizer();
   TheTokenizer->init(skipFirstLine);
   putTokens(mlp, interactive);

	delete TheSourceManager;
	delete TheByteDecoder;
	delete TheByteEncoder;
	delete TheCharacterDecoder;
	delete TheTokenizer;
	delete TheParser;

	}
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseUTF8String(mlp, inStr, b);
	if ( interactiveSym != NULL)
		MLReleaseSymbol(mlp, interactiveSym);
	if ( skipFirstLineSym != NULL)
		MLReleaseSymbol(mlp, skipFirstLineSym);
	return res;
}



void putExpressions(MLINK mlp, bool interactive) {

	auto nodes = parseExpressions(interactive);
   
	if(!MLPutFunction(mlp, "List", nodes.size()))
		goto retPt;

	for (std::shared_ptr<Node> node : nodes) {
		node->put(mlp);
	}

retPt:
	return;
}


struct TokenExpr {
	Token Tok;
	std::string Str;
	SourceSpan Span;

	void put(MLINK mlp) {
		MLPutFunction(mlp, SYMBOL_TOKEN.name().c_str(), 3);

		MLPutSymbol(mlp, TokenToString(Tok).c_str());

		auto escaped = stringEscape(Str);
    	MLPutUTF8String(mlp, reinterpret_cast<unsigned const char *>(escaped.c_str()), escaped.size());

		MLPutSymbol(mlp, "Source");

		Span.put(mlp);
	}
};

void putTokens(MLINK mlp, bool interactive) {

	TheTokenizer->nextToken();

	std::vector<TokenExpr> TokenExprs;

    while (true) {
        
        auto Tok = TheTokenizer->currentToken();

        auto Str = TheTokenizer->getString();

        auto Span = TheSourceManager->getTokenSpan();

        // std::cout << SYMBOL_TOKEN.name();
        // std::cout << "[";
        // std::cout << TokenToString(Tok);
        // std::cout << ", ";
        // std::cout << stringEscape(Str);
        // std::cout << ", <|";
        // std::cout << ASTSourceString(Span);
        // std::cout << "|>";
        // std::cout << "]";

        // std::cout << ",\n";
        // std::cout << "\n";

        if (Tok == TOKEN_EOF) {
            break;
        }

        TokenExpr E{Tok, Str, Span};
        TokenExprs.push_back(E);

        TheTokenizer->nextToken();
    }

    // std::cout << SYMBOL_NOTHING.name();
    // std::cout << "\n";
    // std::cout << "}\n";

    if(!MLPutFunction(mlp, "List", TokenExprs.size()))
		goto retPt;

	for (auto E : TokenExprs) {
		E.put(mlp);
	}

retPt:
	return;
}

std::vector<std::shared_ptr<Node>> parseExpressions(bool interactive) {

    std::vector<std::shared_ptr<Node>> nodes;
            
    while (true) {
        
        auto peek = TheParser->currentToken();
        
        while (peek == TOKEN_NEWLINE) {
            peek = TheParser->nextToken();
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

        //
        // This is running on command-line, so only parse first expression
        //
        if (interactive) {
            break;
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



