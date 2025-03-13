
#pragma once

#include "Token.h" // for Token
#include "Source.h"

class ParserSession;

using ParserSessionPtr = ParserSession *;


//
// Tokenizer takes a stream of WL characters and tokenizes them
//

Token Tokenizer_nextToken(ParserSessionPtr session, NextPolicy policy);
Token Tokenizer_nextToken_stringifyAsTag(ParserSessionPtr session);
Token Tokenizer_nextToken_stringifyAsFile(ParserSessionPtr session);

Token Tokenizer_currentToken(ParserSessionPtr session, NextPolicy policy);
Token Tokenizer_currentToken_stringifyAsTag(ParserSessionPtr session);
Token Tokenizer_currentToken_stringifyAsFile(ParserSessionPtr session);
