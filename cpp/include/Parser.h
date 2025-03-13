
#pragma once

#include "Node.h" // for LeafSeq
#include "Token.h" // for Token
#include "Precedence.h" // for Precedence

#include <cstddef> // for size_t
#include <cstdint> // for uint8_t

class Parselet;
class ParserSession;

using ParseletPtr = Parselet *;
using ParserSessionPtr = ParserSession *;
using ParseFunction = void(ParserSessionPtr parser, ParseletPtr parselet, Token firstTok);
using ParseFunctionPtr = ParseFunction *;


struct Context {
    
    ParseFunctionPtr F;

    ParseletPtr P;

    const size_t Index;
    
    Precedence Prec;
    
    Context(size_t Index, Precedence Prec);
};

enum ColonLHS : uint8_t {
    COLONLHS_NONE,
    COLONLHS_PATTERN,
    COLONLHS_OPTIONAL,
    COLONLHS_ERROR
};

void Parser_handleFirstLine(ParserSessionPtr session);

void Parser_parseClimb(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2);
void Parser_tryContinue(ParserSessionPtr session, ParseletPtr Ignored, Token Ignored2);
void Parser_identity(ParserSessionPtr session, ParseletPtr P, Token firstTok);

void Parser_eatTrivia(ParserSessionPtr session, Token& firstTok, NextPolicy policy);
void Parser_eatTrivia(ParserSessionPtr session, Token& firstTok, NextPolicy policy, TriviaSeq& Args);
void Parser_eatTrivia_stringifyAsFile(ParserSessionPtr session, Token& firstTok);
void Parser_eatTriviaButNotToplevelNewlines(ParserSessionPtr session, Token& firstTok, NextPolicy policy);
void Parser_eatTriviaButNotToplevelNewlines(ParserSessionPtr session, Token& firstTok, NextPolicy policy, TriviaSeq& Args);

Context& Parser_pushContext(ParserSessionPtr session, Precedence Prec);
NodeSeq Parser_popContext(ParserSessionPtr session);
Context& Parser_topContext(ParserSessionPtr session);
Precedence Parser_topPrecedence(ParserSessionPtr session);
void Parser_setPrecedence(ParserSessionPtr session, Precedence Prec);

void Parser_pushLeaf(ParserSessionPtr session, Token T);
void Parser_pushLeafAndNext(ParserSessionPtr session, Token Tok);
void Parser_pushTriviaSeq(ParserSessionPtr session, TriviaSeq& T);
void Parser_pushNode(ParserSessionPtr session, Node *N);
NodeVariant Parser_popNode(ParserSessionPtr session);
NodeVariant& Parser_topNode(ParserSessionPtr session);

void Parser_pushGroup(ParserSessionPtr session, Closer Closr);
void Parser_popGroup(ParserSessionPtr session);
bool Parser_checkGroup(ParserSessionPtr session, Closer Closr);

bool Parser_isQuiescent(ParserSessionPtr session);

bool Parser_checkPatternPrecedence(ParserSessionPtr session);
ColonLHS Parser_checkColonLHS(ParserSessionPtr session);
bool Parser_checkTilde(ParserSessionPtr session);
bool Parser_checkSpan(ParserSessionPtr session);
