
#pragma once

#include "Token.h"
#include "SourceManager.h"

#include <string>

int parseInteger(std::string s, int base);

std::string stringEscape(std::string s);

std::string ASTSourceString(SourceSpan span);
