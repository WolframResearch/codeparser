
// #include "Lexer.h"
#include "Parser.h"
#include "Precedence.h"
#include "ByteDecoder.h"
#include "CharacterDecoder.h"
#include "Tokenizer.h"

#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <cassert>


enum Format {
    FORMAT_INPUTFORM,
    FORMAT_AST,
    FORMAT_TOKENS,
    FORMAT_CHARACTERS
};

void printCharacters();
void printTokens();

int main(int argc, char *argv[]) {
    
    Format format = FORMAT_AST;
    bool prompt = true;
    bool skipFirstLine = false;
    bool singleLine = true;
    std::string file;
    
    for (int i = 1; i < argc; i++) {
        auto arg = std::string(argv[i]);
        if (arg == "-format") {
            i++;
            auto formatStr = std::string(argv[i]);
            if (formatStr == "inputform") {
                format = FORMAT_INPUTFORM;
            } else if (formatStr == "ast") {
                format = FORMAT_AST;
            } else if (formatStr == "tokens") {
                format = FORMAT_TOKENS;
            } else if (formatStr == "characters") {
                format = FORMAT_CHARACTERS;
            } else {
                return 1;
            }
            
        } else if (arg == "-file") {
            i++;
            file = std::string(argv[i]);
            prompt = false;
            singleLine = false;
        } else if (arg == "-noprompt" || arg == "-noPrompt") {
            prompt = false;
        } else if (arg == "-skipfirstline" || arg == "-skipFirstLine") {
            skipFirstLine = true;
        } else if (arg == "-multiline" || arg == "-multiLine") {
            singleLine = false;
        } else {
            return 1;
        }
    }
    
    if (!file.empty()) {
        
        std::ifstream ifs(file, std::ifstream::in);
        
        TheSourceManager = new SourceManager();
        TheByteDecoder = new ByteDecoder(ifs, singleLine);
        TheCharacterDecoder = new CharacterDecoder();
        
        if (format == FORMAT_CHARACTERS) {

            printCharacters();

        } else if (format == FORMAT_TOKENS) {
            
            TheTokenizer = new Tokenizer();
            TheTokenizer->init(skipFirstLine);
            printTokens();

        } else {
            
            TheTokenizer = new Tokenizer();
            TheTokenizer->init(skipFirstLine);
            TheParser = new Parser();
            TheParser->init();
            
            std::vector<std::shared_ptr<Node>> nodes;
            
            while (true) {
                
                auto peek = TheParser->currentToken();
                
                while (peek == TOKEN_NEWLINE) {
                    peek = TheParser->nextToken();
                }
                
                if (peek != TOKEN_EOF) {
                    
                    auto Expr = TheParser->parseTopLevel();
                    
                    nodes.push_back(Expr);
                }

                peek = TheParser->currentToken();
                
                if (peek == TOKEN_EOF) {
                    break;
                }
                
            } // while (true)
            
            auto FN = std::make_shared<FileNode>(nodes);
            
            switch (format) {
                case FORMAT_INPUTFORM:
                    std::cout << FN->inputform();
                    break;
                case FORMAT_AST:
                    std::cout << FN->string();
                    break;
                case FORMAT_TOKENS:
                    // handled elsewhere
                    ;
                    break;
                case FORMAT_CHARACTERS:
                    // handled elsewhere
                    ;
                    break;
            }
            std::cout << "\n";
        }
        
    } else {
            
        if (prompt) {
            std::cout << ">>> ";
        }
        
        TheSourceManager = new SourceManager();
        TheByteDecoder = new ByteDecoder(std::cin, singleLine);
        TheCharacterDecoder = new CharacterDecoder();
        
        if (format == FORMAT_CHARACTERS) {

            printCharacters();

        } else if (format == FORMAT_TOKENS) {
            
            TheTokenizer = new Tokenizer();
            TheTokenizer->init(skipFirstLine);
            printTokens();
            
        } else {
            
            TheTokenizer = new Tokenizer();
            TheTokenizer->init(skipFirstLine);
            TheParser = new Parser();
            TheParser->init();
            
            auto peek = TheParser->currentToken();
            
            while (peek == TOKEN_NEWLINE) {
                peek = TheParser->nextToken();
            }
            
            if (peek != TOKEN_EOF) {
                
                auto Expr = TheParser->parseTopLevel();
                
                peek = TheParser->currentToken();
                
                assert(peek == TOKEN_EOF);
                
                assert(TheParser->getString().empty());
                assert(TheParser->getIssues().empty());
                
                switch (format) {
                    case FORMAT_INPUTFORM:
                        std::cout << Expr->inputform();
                        break;
                    case FORMAT_AST:
                        std::cout << Expr->string();
                        break;
                    case FORMAT_TOKENS:
                        // handled elsewhere
                        ;
                        break;
                    case FORMAT_CHARACTERS:
                        // handled elsewhere
                        ;
                        break;
                }
            }
            
            if (prompt) {
                std::cout << "\n\n";
            }
        }
    }
    
	return 0;
}

void printCharacters() {

    TheCharacterDecoder->nextWLCharacter();

    std::cout << "{\n";

    while (true) {
        
        auto c = TheCharacterDecoder->currentWLCharacter();

        auto String = WLCharacterToString(c);

        auto Span = TheSourceManager->getWLCharacterSpan();

        std::cout << SYMBOL_CHARACTER->name();
        std::cout << "[";
        std::cout << c;
        std::cout << ", ";
        std::cout << stringEscape(String);
        std::cout << ", <|";
        std::cout << ASTSourceString(Span);
        std::cout << "|>";
        std::cout << "]";

        std::cout << ",\n";

        if (c == EOF) {
            break;
        }
        
        TheCharacterDecoder->nextWLCharacter();
    }

    std::cout << "Nothing\n";
    std::cout << "}\n";
}


void printTokens() {

    TheTokenizer->nextToken();

    std::cout << "{\n";

    while (true) {
        
        auto Tok = TheTokenizer->currentToken();

        auto Str = TheTokenizer->getString();

        auto Span = TheSourceManager->getTokenSpan();

        std::cout << SYMBOL_TOKEN->name();
        std::cout << "[";
        std::cout << stringEscape(TokenToString(Tok));
        std::cout << ", ";
        std::cout << stringEscape(Str);
        std::cout << ", <|";
        std::cout << ASTSourceString(Span);
        std::cout << "|>";
        std::cout << "]";

        std::cout << ",\n";

        if (Tok == TOKEN_EOF) {
            break;
        }
        
        TheTokenizer->nextToken();
    }

    std::cout << "Nothing\n";
    std::cout << "}\n";
}


