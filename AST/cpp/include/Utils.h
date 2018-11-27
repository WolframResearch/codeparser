
#pragma once

#include "Token.h"
#include "SourceManager.h"

#include <string>

bool isDigit(int c);

bool isAlpha(int c);

bool isDigitOrAlpha(int c);

bool isDigitOrAlphaOrDollar(int c);

bool isAlphaOrDollar(int c);

bool isHex(int c);

bool isOctal(int c);

bool isSpace(int c);

bool isControl(int c);

int toBaseDigit(int c);

int parseInteger(std::string s, int base);

std::string stringEscape(std::string s);

std::string ASTSourceString(SourceSpan span);
