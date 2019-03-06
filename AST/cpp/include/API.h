
#pragma once

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
// The closest related bug is 357133
//
// This bug was fixed in v12.0
//
// When we no longer support any version < 12.0, then we can switch to using WSTP
//
// Also be a good citizen and cleanup the leftover defines
//
#include "mathlink.h"

#undef P

#include "WolframLibrary.h"

#undef True

#undef False

EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT int ConcreteParseFile(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ConcreteParseString(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeString(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeFile(WolframLibraryData libData, MLINK mlp);
