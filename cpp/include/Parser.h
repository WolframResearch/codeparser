
#pragma once

#include "Node.h" // for LeafNodePtr, etc.
#include "Source.h" // for IssuePtr
#include "Token.h" // for Token
#include "Precedence.h" // for Precedence
#include "TokenEnum.h" // for TokenEnum

#include <set>
#include <deque>
#include <memory> // for unique_ptr

class Parser;

using ParserPtr = std::unique_ptr<Parser>;

enum FirstLineBehavior {
    //
    // Source is a string or something, so if #! is on first line, then do not treat special
    //
    FIRSTLINEBEHAVIOR_NOTSCRIPT = 0,
    
    //
    // Source is something like .wl file that is being treated as a script
    // Or source is .wl file that is NOT being treated as a script
    // #! may be present, or it might not
    //
    FIRSTLINEBEHAVIOR_CHECK = 1,
    
    //
    // Source is a .wls file and there is definitely a #! on first line
    //
    FIRSTLINEBEHAVIOR_SCRIPT = 2,
};

//
//
//
enum Associativity {
    ASSOCIATIVITY_NONRIGHT,
    ASSOCIATIVITY_RIGHT,
};

//
// The ParserContextFlagBits tend to contain context-sensitive bits for parsing
//
// Generally the parser is a Pratt parser with 1 token of look-ahead, except in these few cases.
//
enum ParserContextFlagBits : uint8_t {
    //
    // when parsing a in a:b  then PARSER_INSIDE_COLON bit is 0
    // when parsing b in a:b  then PARSER_INSIDE_COLON bit is 1
    //
    PARSER_INSIDE_COLON = 0x01,
    
    //
    // Needs to detect \[Differential] while parsing
    //
    PARSER_INSIDE_INTEGRAL = 0x02,
    
    //
    // Needs to detect the = or := or =. while parsing
    //
    PARSER_INSIDE_SLASHCOLON = 0x04,
    
    //
    // Needs to detect the second ~ while parsing
    //
    PARSER_INSIDE_TILDE = 0x08,
};

using ParserContextFlag = uint8_t;

//
//
//
struct ParserContext {
    
    //
    // Precedence of the current operator being parsed
    //
    Precedence Prec;
    
    //
    // The Closer of the innermost Group being parsed
    //
    Closer Closr : 4;
    
    ParserContextFlag Flag : 4;
    
    ParserContext() : Prec(PRECEDENCE_LOWEST), Closr(), Flag() {}
};

//
// Sizes of structs with bit-fields are implementation-dependent
//
#ifdef __clang__
static_assert(sizeof(ParserContext) == 2, "Check your assumptions");
#endif // __clang__

//
//
//
class Parser {
private:
    
    void handleFirstLine(FirstLineBehavior firstLineBehavior);
    
public:
    Parser();
    
    void init(FirstLineBehavior firstLineBehavior);
    
    void deinit();
    
    void nextToken(Token Tok);
    
    Token nextToken0(ParserContext Ctxt, NextPolicy policy);
    
    Token currentToken(ParserContext Ctxt, NextPolicy policy) const;
    
    Token currentToken_stringifyAsTag() const;
    Token currentToken_stringifyAsFile() const;
    
    NodePtr infixLoop(NodePtr Left, ParserContext Ctxt);
    
    ~Parser();

    Token eatTrivia(Token firstTok, ParserContext Ctxt, NextPolicy policy, LeafSeq& Args);
    Token eatTrivia_stringifyAsFile(Token firstTok, ParserContext Ctxt, LeafSeq& Args);
    Token eatTriviaButNotToplevelNewlines(Token firstTok, ParserContext Ctxt, NextPolicy policy, LeafSeq& Args);
    Token eatTriviaButNotToplevelNewlines_stringifyAsFile(Token firstTok, ParserContext Ctxt, LeafSeq& Args);
};

extern ParserPtr TheParser;

