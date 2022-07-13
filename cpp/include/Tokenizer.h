
#pragma once

#include "WLCharacter.h" // for WLCharacter
#include "Token.h" // for Token

#include <set>
#include <cstddef> // for size_t

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
