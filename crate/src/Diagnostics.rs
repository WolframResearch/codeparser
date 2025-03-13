
#include "Diagnostics.h"

#include <iostream>
#include <chrono>


std::chrono::time_point<std::chrono::system_clock> last;


void DiagnosticsLog(std::string s) {
    std::cout << s << "\n";
}

void DiagnosticsMarkTime() {
    last = std::chrono::system_clock::now();
}

void DiagnosticsLogTime() {
    
    auto now = std::chrono::system_clock::now();
    
    auto diff = std::chrono::duration_cast<std::chrono::milliseconds>(now - last);
    
    std::cout << "time: " << diff.count() << "ms\n";
}

void DiagnosticsPrint() {
    
    std::cout << "ByteBuffer_size: " << ByteBuffer_size << "\n";
    
    std::cout << "ByteDecoder_PrintableCount: " << ByteDecoder_PrintableCount << "\n";
    
    std::cout << "ByteDecoder_LineFeedCount: " << ByteDecoder_LineFeedCount << "\n";
    
    std::cout << "ByteDecoder_TabCount: " << ByteDecoder_TabCount << "\n";
    
    std::cout << "ByteDecoder_CarriageReturnCount: " << ByteDecoder_CarriageReturnCount << "\n";

    std::cout << "ByteDecoder_1ByteCount: " << ByteDecoder_1ByteCount << "\n";

    std::cout << "ByteDecoder_2ByteCount: " << ByteDecoder_2ByteCount << "\n";

    std::cout << "ByteDecoder_3ByteCount: " << ByteDecoder_3ByteCount << "\n";

    std::cout << "ByteDecoder_4ByteCount: " << ByteDecoder_4ByteCount << "\n";

    std::cout << "ByteDecoder_FFCount: " << ByteDecoder_FFCount << "\n";

    std::cout << "ByteDecoder_Incomplete1ByteCount: " << ByteDecoder_Incomplete1ByteCount << "\n";
    
    std::cout << "CharacterDecoder_UnescapedCount: " << CharacterDecoder_UnescapedCount << "\n";
    
    std::cout << "CharacterDecoder_LineContinuationCount: " << CharacterDecoder_LineContinuationCount << "\n";

    std::cout << "CharacterDecoder_LongNameCount: " << CharacterDecoder_LongNameCount << "\n";

    std::cout << "CharacterDecoder_4HexCount: " << CharacterDecoder_4HexCount << "\n";

    std::cout << "CharacterDecoder_2HexCount: " << CharacterDecoder_2HexCount << "\n";

    std::cout << "CharacterDecoder_6HexCount: " << CharacterDecoder_6HexCount << "\n";
    
    std::cout << "CharacterDecoder_OctalCount: " << CharacterDecoder_OctalCount << "\n";

    std::cout << "CharacterDecoder_StringMetaBackspaceCount: " << CharacterDecoder_StringMetaBackspaceCount << "\n";

    std::cout << "CharacterDecoder_StringMetaFormFeedCount: " << CharacterDecoder_StringMetaFormFeedCount << "\n";

    std::cout << "CharacterDecoder_StringMetaLineFeedCount: " << CharacterDecoder_StringMetaLineFeedCount << "\n";

    std::cout << "CharacterDecoder_StringMetaCarriageReturnCount: " << CharacterDecoder_StringMetaCarriageReturnCount << "\n";

    std::cout << "CharacterDecoder_StringMetaTabCount: " << CharacterDecoder_StringMetaTabCount << "\n";

    std::cout << "CharacterDecoder_StringMetaDoubleQuoteCount: " << CharacterDecoder_StringMetaDoubleQuoteCount << "\n";

    std::cout << "CharacterDecoder_StringMetaBackslashCount: " << CharacterDecoder_StringMetaBackslashCount << "\n";

    std::cout << "CharacterDecoder_StringMetaOpenCount: " << CharacterDecoder_StringMetaOpenCount << "\n";

    std::cout << "CharacterDecoder_StringMetaCloseCount: " << CharacterDecoder_StringMetaCloseCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxBangCount: " << CharacterDecoder_LinearSyntaxBangCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxPercentCount: " << CharacterDecoder_LinearSyntaxPercentCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxAmpCount: " << CharacterDecoder_LinearSyntaxAmpCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxOpenParenCount: " << CharacterDecoder_LinearSyntaxOpenParenCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxCloseParenCount: " << CharacterDecoder_LinearSyntaxCloseParenCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxStarCount: " << CharacterDecoder_LinearSyntaxStarCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxPlusCount: " << CharacterDecoder_LinearSyntaxPlusCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxSlashCount: " << CharacterDecoder_LinearSyntaxSlashCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxAtCount: " << CharacterDecoder_LinearSyntaxAtCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxCaretCount: " << CharacterDecoder_LinearSyntaxCaretCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxUnderscoreCount: " << CharacterDecoder_LinearSyntaxUnderscoreCount << "\n";

    std::cout << "CharacterDecoder_LinearSyntaxBacktickCount: " << CharacterDecoder_LinearSyntaxBacktickCount << "\n";
    
    std::cout << "CharacterDecoder_LinearSyntaxSpaceCount: " << CharacterDecoder_LinearSyntaxSpaceCount << "\n";

    std::cout << "CharacterDecoder_UnhandledCount: " << CharacterDecoder_UnhandledCount << "\n";
    
    std::cout << "Tokenizer_StringFastCount: " << Tokenizer_StringFastCount << "\n";

    std::cout << "Tokenizer_StringSlowCount: " << Tokenizer_StringSlowCount << "\n";
    
    std::cout << "Tokenizer_NewlineCount: " << Tokenizer_NewlineCount << "\n";
    
    std::cout << "Tokenizer_CommentCount: " << Tokenizer_CommentCount << "\n";
    
    std::cout << "Tokenizer_SymbolCount: " << Tokenizer_SymbolCount << "\n";
    
    std::cout << "Tokenizer_OpenSquareCount: " << Tokenizer_OpenSquareCount << "\n";
    
    std::cout << "Tokenizer_OpenCurlyCount: " << Tokenizer_OpenCurlyCount << "\n";

    std::cout << "Tokenizer_WhitespaceCount: " << Tokenizer_WhitespaceCount << "\n";

    std::cout << "Tokenizer_CommaCount: " << Tokenizer_CommaCount << "\n";

    std::cout << "Tokenizer_CloseSquareCount: " << Tokenizer_CloseSquareCount << "\n";

    std::cout << "Tokenizer_CloseCurlyCount: " << Tokenizer_CloseCurlyCount << "\n";

    std::cout << "Tokenizer_CloseParenCount: " << Tokenizer_CloseParenCount << "\n";
    
    std::cout << "Tokenizer_MinusGreaterCount: " << Tokenizer_MinusGreaterCount << "\n";
    
    std::cout << "Tokenizer_NumberCount: " << Tokenizer_NumberCount << "\n";
    
    std::cout << "Tokenizer_ColonGreaterCount: " << Tokenizer_ColonGreaterCount << "\n";
    
    std::cout << "Tokenizer_MinusCount: " << Tokenizer_MinusCount << "\n";
    
    std::cout << "Tokenizer_OpenParenCount: " << Tokenizer_OpenParenCount << "\n";
    
    std::cout << "Tokenizer_HashCount: " << Tokenizer_HashCount << "\n";
    
    std::cout << "Tokenizer_AmpCount: " << Tokenizer_AmpCount << "\n";
    
    std::cout << "Tokenizer_PlusCount: " << Tokenizer_PlusCount << "\n";
    
    std::cout << "Node_LeafNodeCount: " << Node_LeafNodeCount << "\n";
    
    std::cout << "Node_ErrorNodeCount: " << Node_ErrorNodeCount << "\n";
    
    std::cout << "Node_UnterminatedTokenErrorNeedsReparseNodeCount: " << Node_UnterminatedTokenErrorNeedsReparseNodeCount << "\n";
    
    std::cout << "Node_SyntaxErrorNodeCount: " << Node_SyntaxErrorNodeCount << "\n";
    
    std::cout << "Node_AbortNodeCount: " << Node_AbortNodeCount << "\n";
    
    std::cout << "Node_BinaryNodeCount: " << Node_BinaryNodeCount << "\n";
    
    std::cout << "Node_CallNodeCount: " << Node_CallNodeCount << "\n";
    
    std::cout << "Node_CompoundNodeCount: " << Node_CompoundNodeCount << "\n";
    
    std::cout << "Node_GroupMissingCloserNodeCount: " << Node_GroupMissingCloserNodeCount << "\n";
    
    std::cout << "Node_GroupNodeCount: " << Node_GroupNodeCount << "\n";
    
    std::cout << "Node_InfixNodeCount: " << Node_InfixNodeCount << "\n";
    
    std::cout << "Node_PostfixNodeCount: " << Node_PostfixNodeCount << "\n";
    
    std::cout << "Node_PrefixBinaryNodeCount: " << Node_PrefixBinaryNodeCount << "\n";
    
    std::cout << "Node_PrefixNodeCount: " << Node_PrefixNodeCount << "\n";
    
    std::cout << "Node_TernaryNodeCount: " << Node_TernaryNodeCount << "\n";
    
    std::cout << "Node_UnterminatedGroupNeedsReparseNodeCount: " << Node_UnterminatedGroupNeedsReparseNodeCount << "\n";
    
    auto totalNodeCount = Node_LeafNodeCount + Node_ErrorNodeCount + Node_UnterminatedTokenErrorNeedsReparseNodeCount +
        Node_SyntaxErrorNodeCount + Node_AbortNodeCount + Node_BinaryNodeCount + Node_CallNodeCount + Node_CompoundNodeCount +
        Node_GroupMissingCloserNodeCount + Node_GroupNodeCount + Node_InfixNodeCount + Node_PostfixNodeCount +
        Node_PrefixBinaryNodeCount + Node_PrefixNodeCount + Node_TernaryNodeCount + Node_UnterminatedGroupNeedsReparseNodeCount;
    
    std::cout << "bytes per leaf node: " << (1.0 * ByteBuffer_size / Node_LeafNodeCount) << "\n";
    
    std::cout << "bytes per node: " << (1.0 * ByteBuffer_size / totalNodeCount) << "\n";
}


int ByteBuffer_size = 0;

int ByteDecoder_PrintableCount = 0;

int ByteDecoder_LineFeedCount = 0;

int ByteDecoder_TabCount = 0;

int ByteDecoder_CarriageReturnCount = 0;

int ByteDecoder_1ByteCount = 0;

int ByteDecoder_2ByteCount = 0;

int ByteDecoder_3ByteCount = 0;

int ByteDecoder_4ByteCount = 0;

int ByteDecoder_FFCount = 0;

int ByteDecoder_Incomplete1ByteCount = 0;

int CharacterDecoder_UnescapedCount = 0;

int CharacterDecoder_LineContinuationCount = 0;

int CharacterDecoder_LongNameCount = 0;

int CharacterDecoder_4HexCount = 0;

int CharacterDecoder_2HexCount = 0;

int CharacterDecoder_6HexCount = 0;

int CharacterDecoder_OctalCount = 0;

int CharacterDecoder_StringMetaBackspaceCount = 0;

int CharacterDecoder_StringMetaFormFeedCount = 0;

int CharacterDecoder_StringMetaLineFeedCount = 0;

int CharacterDecoder_StringMetaCarriageReturnCount = 0;

int CharacterDecoder_StringMetaTabCount = 0;

int CharacterDecoder_StringMetaDoubleQuoteCount = 0;

int CharacterDecoder_StringMetaBackslashCount = 0;

int CharacterDecoder_StringMetaOpenCount = 0;

int CharacterDecoder_StringMetaCloseCount = 0;

int CharacterDecoder_LinearSyntaxBangCount = 0;

int CharacterDecoder_LinearSyntaxPercentCount = 0;

int CharacterDecoder_LinearSyntaxAmpCount = 0;

int CharacterDecoder_LinearSyntaxOpenParenCount = 0;

int CharacterDecoder_LinearSyntaxCloseParenCount = 0;

int CharacterDecoder_LinearSyntaxStarCount = 0;

int CharacterDecoder_LinearSyntaxPlusCount = 0;

int CharacterDecoder_LinearSyntaxSlashCount = 0;

int CharacterDecoder_LinearSyntaxAtCount = 0;

int CharacterDecoder_LinearSyntaxCaretCount = 0;

int CharacterDecoder_LinearSyntaxUnderscoreCount = 0;

int CharacterDecoder_LinearSyntaxBacktickCount = 0;

int CharacterDecoder_LinearSyntaxSpaceCount = 0;

int CharacterDecoder_UnhandledCount = 0;

int Tokenizer_StringFastCount = 0;

int Tokenizer_StringSlowCount = 0;

int Tokenizer_CommentCount = 0;

int Tokenizer_NewlineCount = 0;

int Tokenizer_SymbolCount = 0;

int Tokenizer_OpenSquareCount = 0;

int Tokenizer_OpenCurlyCount = 0;

int Tokenizer_WhitespaceCount = 0;

int Tokenizer_CommaCount = 0;

int Tokenizer_CloseSquareCount = 0;

int Tokenizer_CloseCurlyCount = 0;

int Tokenizer_CloseParenCount = 0;

int Tokenizer_MinusGreaterCount = 0;

int Tokenizer_NumberCount = 0;

int Tokenizer_ColonGreaterCount = 0;

int Tokenizer_MinusCount = 0;

int Tokenizer_OpenParenCount = 0;

int Tokenizer_HashCount = 0;

int Tokenizer_AmpCount = 0;

int Tokenizer_PlusCount = 0;

int Node_LeafNodeCount = 0;

int Node_ErrorNodeCount = 0;

int Node_UnterminatedTokenErrorNeedsReparseNodeCount = 0;

int Node_SyntaxErrorNodeCount = 0;

int Node_AbortNodeCount = 0;

int Node_BinaryNodeCount = 0;

int Node_CallNodeCount = 0;

int Node_CompoundNodeCount = 0;

int Node_GroupMissingCloserNodeCount = 0;

int Node_GroupNodeCount = 0;

int Node_InfixNodeCount = 0;

int Node_PostfixNodeCount = 0;

int Node_PrefixBinaryNodeCount = 0;

int Node_PrefixNodeCount = 0;

int Node_TernaryNodeCount = 0;

int Node_UnterminatedGroupNeedsReparseNodeCount = 0;
