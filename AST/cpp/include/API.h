//
// Despite being mentioned here:
// language/LibraryLink/tutorial/LibraryStructure.html
//
// It is not actually possible to include "wstp.h" for use with 
// WolframLibrary.h
//
// Using wstp.h results in errors like:
// error: unknown type name 'MLINK'
//
// This is a bug in WSTP
//
// Also be a good citizen and cleanup the leftover defines
//
#include "mathlink.h"

#undef P

#include "WolframLibrary.h"

#undef True

#undef False

EXTERN_C DLLEXPORT int concreteParseFile_characters(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int concreteParseFile_tokens(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int concreteParseFile(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int concreteParseString_characters(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int concreteParseString_tokens(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int concreteParseString(WolframLibraryData libData, MLINK mlp);
