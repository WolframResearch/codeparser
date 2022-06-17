
#include "WolframLibrary.h"
#undef True
#undef False

#include <ostream>


EXTERN_C DLLEXPORT void DiagnosticsPrint(std::ostream& s);

extern int CharacterDecoder_LineContinuationCount;

extern int CharacterDecoder_LongNameCount;

extern int CharacterDecoder_4HexCount;

extern int CharacterDecoder_2HexCount;

extern int CharacterDecoder_6HexCount;

extern int CharacterDecoder_OctalCount;

extern int CharacterDecoder_StringMetaBackspace;

extern int CharacterDecoder_StringMetaFormFeed;

extern int CharacterDecoder_StringMetaLineFeedCount;

extern int CharacterDecoder_StringMetaCarriageReturn;

extern int CharacterDecoder_StringMetaTab;

extern int CharacterDecoder_StringMetaDoubleQuoteCount;

extern int CharacterDecoder_StringMetaBackslashCount;

extern int CharacterDecoder_StringMetaOpenCount;

extern int CharacterDecoder_StringMetaCloseCount;

extern int CharacterDecoder_LinearSyntaxBangCount;

extern int CharacterDecoder_LinearSyntaxPercentCount;

extern int CharacterDecoder_LinearSyntaxAmpCount;

extern int CharacterDecoder_LinearSyntaxOpenParenCount;

extern int CharacterDecoder_LinearSyntaxCloseParenCount;

extern int CharacterDecoder_LinearSyntaxStarCount;

extern int CharacterDecoder_LinearSyntaxPlusCount;

extern int CharacterDecoder_LinearSyntaxSlashCount;

extern int CharacterDecoder_LinearSyntaxAtCount;

extern int CharacterDecoder_LinearSyntaxCaretCount;

extern int CharacterDecoder_LinearSyntaxUnderscoreCount;

extern int CharacterDecoder_LinearSyntaxBacktickCount;

extern int CharacterDecoder_LinearSyntaxSpaceCount;

extern int CharacterDecoder_UnhandledCount;

