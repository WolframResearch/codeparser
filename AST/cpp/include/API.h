
#include "Node.h"

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
// Also be a good citizen and cleanup the leftover defines
//
//#include "wstp.h"
#include "mathlink.h"

#undef P

#include "WolframLibrary.h"

#undef True

#undef False



EXTERN_C DLLEXPORT int ConcreteParseFile(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int ConcreteParseString(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeFile(WolframLibraryData libData, MLINK mlp);

EXTERN_C DLLEXPORT int TokenizeString(WolframLibraryData libData, MLINK mlp);
