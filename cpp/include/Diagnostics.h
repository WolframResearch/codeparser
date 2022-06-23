
#include "WolframLibrary.h"
#undef True
#undef False

#include <ostream>
#include <string>


EXTERN_C DLLEXPORT void DiagnosticsLog(std::string s);

EXTERN_C DLLEXPORT void DiagnosticsMarkTime();

EXTERN_C DLLEXPORT void DiagnosticsLogTime();

EXTERN_C DLLEXPORT void DiagnosticsPrint();

extern int ByteBuffer_size;

extern int ByteDecoder_PrintableCount;

extern int ByteDecoder_LineFeedCount;

extern int ByteDecoder_TabCount;

extern int ByteDecoder_CarriageReturnCount;

extern int ByteDecoder_1ByteCount;

extern int ByteDecoder_2ByteCount;

extern int ByteDecoder_3ByteCount;

extern int ByteDecoder_4ByteCount;

extern int ByteDecoder_FFCount;

extern int ByteDecoder_Incomplete1ByteCount;

extern int CharacterDecoder_UnescapedCount;

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

extern int Tokenizer_StringFastCount;

extern int Tokenizer_StringSlowCount;

extern int Tokenizer_CommentCount;

extern int Tokenizer_NewlineCount;

extern int Tokenizer_SymbolCount;

extern int Tokenizer_OpenSquareCount;

extern int Tokenizer_OpenCurlyCount;

extern int Tokenizer_WhitespaceCount;

extern int Tokenizer_CommaCount;

extern int Tokenizer_CloseSquareCount;

extern int Tokenizer_CloseCurlyCount;

extern int Tokenizer_CloseParenCount;

extern int Tokenizer_MinusGreaterCount;

extern int Tokenizer_NumberCount;

extern int Tokenizer_ColonGreaterCount;

extern int Tokenizer_MinusCount;

extern int Tokenizer_OpenParenCount;

extern int Tokenizer_HashCount;

extern int Tokenizer_AmpCount;

extern int Tokenizer_PlusCount;

extern int Node_LeafNodeCount;

extern int Node_ErrorNodeCount;

extern int Node_UnterminatedTokenErrorNeedsReparseNodeCount;

extern int Node_SyntaxErrorNodeCount;

//extern int Node_OperatorNodeCount;

extern int Node_AbortNodeCount;

extern int Node_PrefixNodeCount;

extern int Node_BinaryNodeCount;

extern int Node_InfixNodeCount;

extern int Node_TernaryNodeCount;

extern int Node_PostfixNodeCount;

extern int Node_PrefixBinaryNodeCount;

extern int Node_GroupNodeCount;

extern int Node_CompoundNodeCount;

extern int Node_GroupMissingCloserNodeCount;

extern int Node_UnterminatedGroupNeedsReparseNodeCount;

extern int Node_CallNodeCount;
