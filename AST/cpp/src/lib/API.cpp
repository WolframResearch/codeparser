
#include "API.h"

#include <stdlib.h>
#include <string>

DLLEXPORT mint WolframLibrary_getVersion() {
	return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
	return 0;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
	return;
}

char *reverseStringImpl(const char *);

DLLEXPORT int concreteParseFile(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const char *inStr = NULL;
	char* outStr = NULL;
	
	if ( !MLTestHead( mlp, "List", &len)) 
		goto retPt;
	if ( len != 1) 
		goto retPt;

	if(! MLGetString(mlp, &inStr))
		goto retPt;

	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	outStr = reverseStringImpl(inStr);
	if ( !MLPutString( mlp,outStr))
		goto retPt;
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseString(mlp, inStr);
	if ( outStr != NULL)
		free((void*)outStr);
	return res;
}

DLLEXPORT int concreteParseString(WolframLibraryData libData, MLINK mlp) {
	int res = LIBRARY_FUNCTION_ERROR;
	int len;
	const char *inStr = NULL;
	char* outStr = NULL;
	
	if ( !MLTestHead( mlp, "List", &len)) 
		goto retPt;
	if ( len != 1) 
		goto retPt;

	if(! MLGetString(mlp, &inStr))
		goto retPt;

	if ( ! MLNewPacket(mlp) ) 
		goto retPt;

	outStr = reverseStringImpl(inStr);
	if ( !MLPutString( mlp,outStr))
		goto retPt;
	res = LIBRARY_NO_ERROR;
retPt: 
	if ( inStr != NULL)
		MLReleaseString(mlp, inStr);
	if ( outStr != NULL)
		free((void*)outStr);
	return res;
}


char *reverseStringImpl(const char *in) {
	std::string inStr(in);
	
	reverse(inStr.begin(), inStr.end());

	char *s2 = strdup(inStr.c_str());
	return s2;
} 
