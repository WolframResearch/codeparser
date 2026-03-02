
#include "TokenEnum.h"

#include "TokenEnumRegistration.h"

#include <cassert>


bool operator==(TokenEnum a, TokenEnum b) {
	return a.valBits == b.valBits;
}

bool operator!=(TokenEnum a, TokenEnum b) {
	return a.valBits != b.valBits;
}

Closer GroupOpenerToCloser(TokenEnum T) {

	switch (T.value()) {
        case TOKEN_COLONCOLONOPENSQUARE.value(): {
            return CLOSER_CLOSESQUARE;
        }
        case TOKEN_LONGNAME_LEFTANGLEBRACKET.value(): {
            return CLOSER_LONGNAME_RIGHTANGLEBRACKET;
        }
        case TOKEN_LONGNAME_LEFTASSOCIATION.value(): {
            return CLOSER_LONGNAME_RIGHTASSOCIATION;
        }
        case TOKEN_LONGNAME_LEFTBRACKETINGBAR.value(): {
            return CLOSER_LONGNAME_RIGHTBRACKETINGBAR;
        }
        case TOKEN_LONGNAME_LEFTCEILING.value(): {
            return CLOSER_LONGNAME_RIGHTCEILING;
        }
        case TOKEN_LONGNAME_LEFTDOUBLEBRACKET.value(): {
            return CLOSER_LONGNAME_RIGHTDOUBLEBRACKET;
        }
        case TOKEN_LONGNAME_LEFTDOUBLEBRACKETINGBAR.value(): {
            return CLOSER_LONGNAME_RIGHTDOUBLEBRACKETINGBAR;
        }
        case TOKEN_LONGNAME_LEFTFLOOR.value(): {
            return CLOSER_LONGNAME_RIGHTFLOOR;
        }
        case TOKEN_LESSBAR.value(): {
            return CLOSER_BARGREATER;
        }
        case TOKEN_OPENCURLY.value(): {
            return CLOSER_CLOSECURLY;
        }
        case TOKEN_LONGNAME_OPENCURLYDOUBLEQUOTE.value(): {
            return CLOSER_LONGNAME_CLOSECURLYDOUBLEQUOTE;
        }
        case TOKEN_LONGNAME_OPENCURLYQUOTE.value(): {
            return CLOSER_LONGNAME_CLOSECURLYQUOTE;
        }
        case TOKEN_OPENPAREN.value(): {
            return CLOSER_CLOSEPAREN;
        }
        case TOKEN_OPENSQUARE.value(): {
            return CLOSER_CLOSESQUARE;
        }
	}
	
	assert(false && "Unhandled token");
	
	return CLOSER_ASSERTFALSE;
}

Closer TokenToCloser(TokenEnum T) {
	switch (T.value()) {
        case TOKEN_BARGREATER.value(): {
            return CLOSER_BARGREATER;
        }
        case TOKEN_CLOSECURLY.value(): {
            return CLOSER_CLOSECURLY;
        }
        case TOKEN_LONGNAME_CLOSECURLYDOUBLEQUOTE.value(): {
            return CLOSER_LONGNAME_CLOSECURLYDOUBLEQUOTE;
        }
        case TOKEN_LONGNAME_CLOSECURLYQUOTE.value(): {
            return CLOSER_LONGNAME_CLOSECURLYQUOTE;
        }
        case TOKEN_CLOSEPAREN.value(): {
            return CLOSER_CLOSEPAREN;
        }
        case TOKEN_CLOSESQUARE.value(): {
            return CLOSER_CLOSESQUARE;
        }
        case TOKEN_LONGNAME_RIGHTANGLEBRACKET.value(): {
            return CLOSER_LONGNAME_RIGHTANGLEBRACKET;
        }
        case TOKEN_LONGNAME_RIGHTASSOCIATION.value(): {
            return CLOSER_LONGNAME_RIGHTASSOCIATION;
        }
        case TOKEN_LONGNAME_RIGHTBRACKETINGBAR.value(): {
            return CLOSER_LONGNAME_RIGHTBRACKETINGBAR;
        }
        case TOKEN_LONGNAME_RIGHTCEILING.value(): {
            return CLOSER_LONGNAME_RIGHTCEILING;
        }
        case TOKEN_LONGNAME_RIGHTDOUBLEBRACKET.value(): {
            return CLOSER_LONGNAME_RIGHTDOUBLEBRACKET;
        }
        case TOKEN_LONGNAME_RIGHTDOUBLEBRACKETINGBAR.value(): {
            return CLOSER_LONGNAME_RIGHTDOUBLEBRACKETINGBAR;
        }
        case TOKEN_LONGNAME_RIGHTFLOOR.value(): {
            return CLOSER_LONGNAME_RIGHTFLOOR;
        }
	}
    
    return CLOSER_ASSERTFALSE;
}
