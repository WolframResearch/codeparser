
#include "API.h"

#include "Parser.h" // for Parser
#include "ParseletRegistration.h" // for prefixParselets
#include "Parselet.h" // for Parselet impls
#include "Tokenizer.h" // for Tokenizer
#include "CharacterDecoder.h" // for CharacterDecoder
#include "ByteDecoder.h" // for ByteDecoder
#include "ByteBuffer.h" // for ByteBuffer
#include "ByteEncoder.h" // for ByteEncoder
#include "Utils.h" // for parseSourceConvention
#include "MyString.h"
#include "Symbol.h"

#if USE_EXPR_LIB
#include "ExprLibrary.h"
#include "WolframNumericArrayLibrary.h"
#endif // USE_EXPR_LIB

#include <memory> // for unique_ptr
#ifdef WINDOWS_MATHLINK
#else
#include <signal.h> // for SIGINT
#endif // WINDOWS_MATHLINK
#include <vector>
#include <set>
#include <numeric> // for accumulate

bool validatePath(WolframLibraryData libData, const unsigned char *inStr, size_t len);


NodeContainer::NodeContainer(std::vector<NodePtr> N) : N(std::move(N)) {}

void NodeContainer::print(std::ostream& s) const {
    
    SYMBOL_LIST.print(s);
    s << "[";
    
    for (auto& NN : N) {
        NN->print(s);
        s << ", ";
    }
    
    s << "]";
}

bool NodeContainer::check() const {
    
    auto accum = std::accumulate(N.begin(), N.end(), true, [](bool a, const NodePtr& b){ return a && b->check(); });
    
    return accum;
}

void NodeContainerPrint(NodeContainerPtr C, std::ostream& s) {
    C->print(s);
}

int NodeContainerCheck(NodeContainerPtr C) {
    return C->check();
}


ParserSession::ParserSession() : fatalIssues(), nonFatalIssues(),
#if !NABORT
currentAbortQ(),
#endif // !NABORT
unsafeCharacterEncodingFlag(),
bufAndLen(),
libData(),
srcConvention(),
tabWidth(),
firstLineBehavior(),
encodingMode() {
    
    TheByteBuffer = ByteBufferPtr(new ByteBuffer());
    TheByteDecoder = ByteDecoderPtr(new ByteDecoder());
    TheCharacterDecoder = CharacterDecoderPtr(new CharacterDecoder());
    TheTokenizer = TokenizerPtr(new Tokenizer());
    TheParser = ParserPtr(new Parser());
}

ParserSession::~ParserSession() {

    TheParser.reset(nullptr);
    TheTokenizer.reset(nullptr);
    TheCharacterDecoder.reset(nullptr);
    TheByteDecoder.reset(nullptr);
    TheByteBuffer.reset(nullptr);
}

void ParserSession::init(
    BufferAndLength bufAndLenIn,
    WolframLibraryData libDataIn,
    SourceConvention srcConventionIn,
    uint32_t tabWidthIn,
    FirstLineBehavior firstLineBehaviorIn,
    EncodingMode encodingModeIn) {
    
    fatalIssues.clear();
    nonFatalIssues.clear();
    
    bufAndLen = bufAndLenIn;
    libData = libDataIn;
    srcConvention = srcConventionIn;
    tabWidth = tabWidthIn;
    firstLineBehavior = firstLineBehaviorIn;
    encodingMode = encodingModeIn;
    
    unsafeCharacterEncodingFlag = UNSAFECHARACTERENCODING_OK;
    
    TheByteBuffer->init();
    TheByteDecoder->init();
    TheCharacterDecoder->init();
    TheTokenizer->init();
    TheParser->init();
    
    if (libDataIn) {
        
#if !NABORT
        currentAbortQ = [libDataIn]() {
            //
            // AbortQ() returns a mint
            //
            bool res = libDataIn->AbortQ();
            return res;
        };
#endif // !NABORT
    
    } else {
#if !NABORT
        currentAbortQ = nullptr;
#endif // !NABORT
    }
}

void ParserSession::deinit() {
    
    fatalIssues.clear();
    nonFatalIssues.clear();
    
    TheParser->deinit();
    TheTokenizer->deinit();
    TheCharacterDecoder->deinit();
    TheByteDecoder->deinit();
    TheByteBuffer->deinit();
}

NodeContainerPtr ParserSession::parseExpressions() {
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
        while (true) {
            
#if !NABORT
            if (TheParserSession->isAbort()) {
                
                break;
            }
#endif // !NABORT
            
            auto peek = TheParser->currentToken(TOPLEVEL);
            
            if (peek.Tok == TOKEN_ENDOFFILE) {
                break;
            }
            
            if (peek.Tok.isTrivia()) {
                
                exprs.emplace_back(new LeafNode(peek));
                
                TheParser->nextToken(peek);
                
                continue;
            }
            
            //
            // special top-level handling of stray closers
            //
            if (peek.Tok.isCloser()) {
                
                PrefixToplevelCloserParselet_parsePrefix(prefixToplevelCloserParselet, peek);
                
            } else {
                
                auto P = prefixParselets[peek.Tok.value()];
                
                (P->parsePrefix())(P, peek);
            }
            
            auto Expr = TheParser->popNode();
            
            assert(TheParser->getArgsStackSize() == 0);
            assert(TheParser->getContextStackSize() == 0);
            assert(TheParser->getNodeStackSize() == 0);
            assert(TheParser->getGroupDepth() == 0);
            assert(TheParser->getTrivia1().empty());
            assert(TheParser->getTrivia2().empty());
            
            exprs.push_back(std::move(Expr));
            
        } // while (true)
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        std::vector<NodePtr> exprs;
        
        exprs.push_back(NodePtr(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag)));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    //
    // Now handle the out-of-band expressions, i.e., issues and metadata
    //
    {
#if !NISSUES
        //
        // if there are fatal issues, then only send fatal issues
        //
        if (!fatalIssues.empty()) {
            
            nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(fatalIssues))));
            
        } else {
            
            nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(nonFatalIssues))));
        }
#else
        
        nodes.push_back(NodePtr(new CollectedIssuesNode({})));
        
#endif // !NISSUES
    }
    
    {
        auto& SimpleLineContinuations = TheCharacterDecoder->getSimpleLineContinuations();

        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(SimpleLineContinuations))));
    }
    
    {
        auto& ComplexLineContinuations = TheCharacterDecoder->getComplexLineContinuations();
        
        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(ComplexLineContinuations))));
    }
    
    {
        auto& EmbeddedNewlines = TheTokenizer->getEmbeddedNewlines();

        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(EmbeddedNewlines))));
    }
    
    {
        std::set<SourceLocation> tabs;
        
        auto& TokenizerEmbeddedTabs = TheTokenizer->getEmbeddedTabs();
        for (auto& T : TokenizerEmbeddedTabs) {
            tabs.insert(T);
        }
        
        auto& CharacterDecoderEmbeddedTabs = TheCharacterDecoder->getEmbeddedTabs();
        for (auto& T : CharacterDecoderEmbeddedTabs) {
            tabs.insert(T);
        }
        
        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(tabs))));
    }
    
    auto C = new NodeContainer(std::move(nodes));
    
    return C;
}

NodeContainerPtr ParserSession::tokenize() {
    
    std::vector<NodePtr> nodes;
    
    while (true) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            break;
        }
#endif // !NABORT
        
        auto Tok = TheTokenizer->currentToken(TOPLEVEL);
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            break;
        }
        
        if (Tok.Tok.isError()) {
            
            if (Tok.Tok.isUnterminated()) {
                
                nodes.emplace_back(new UnterminatedTokenErrorNeedsReparseNode(Tok));
                
                TheTokenizer->nextToken(Tok);
                
            } else {
                
                nodes.emplace_back(new ErrorNode(Tok));
                
                TheTokenizer->nextToken(Tok);
            }
            
        } else {
            
            nodes.emplace_back(new LeafNode(Tok));
            
            TheTokenizer->nextToken(Tok);
        }
        
    } // while (true)
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        auto N = NodePtr(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag));

        nodes.push_back(std::move(N));
    }
    
    auto C = new NodeContainer(std::move(nodes));
    
    return C;
}


Node *ParserSession::concreteParseLeaf0(int mode) {
    
    switch (mode) {
        case STRINGIFYMODE_NORMAL: {
            
            auto Tok = TheTokenizer->nextToken0(TOPLEVEL);
            
            if (Tok.Tok.isError()) {
                
                if (Tok.Tok.isUnterminated()) {
                    
                    return new UnterminatedTokenErrorNeedsReparseNode(Tok);
                    
                } else {
                    
                    return new ErrorNode(Tok);
                }
                
            } else {
                
                return new LeafNode(Tok);
            }
        }
        case STRINGIFYMODE_TAG: {
            
            auto Tok = TheTokenizer->nextToken0_stringifyAsTag();
            
            if (Tok.Tok.isError()) {
                
                if (Tok.Tok.isUnterminated()) {
                    
                    return new UnterminatedTokenErrorNeedsReparseNode(Tok);
                    
                } else {
                    
                    return new ErrorNode(Tok);
                }
                
            } else {
                
                return new LeafNode(Tok);
            }
        }
        case STRINGIFYMODE_FILE: {
            
            auto Tok = TheTokenizer->nextToken0_stringifyAsFile();
            
            if (Tok.Tok.isError()) {
                
                if (Tok.Tok.isUnterminated()) {
                    
                    return new UnterminatedTokenErrorNeedsReparseNode(Tok);
                    
                } else {
                    
                    return new ErrorNode(Tok);
                }
                
            } else {
                
                return new LeafNode(Tok);
            }
        }
        default: {
            
            assert(false);
            
            return nullptr;
        }
    }
}

NodeContainerPtr ParserSession::concreteParseLeaf(StringifyMode mode) {
    
    std::vector<NodePtr> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodePtr> exprs;
        
        exprs.emplace_back(concreteParseLeaf0(mode));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        std::vector<NodePtr> exprs;
        
        auto node = NodePtr(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag));
        
        exprs.push_back(std::move(node));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
#if !NISSUES
    //
    // Collect all issues from the various components
    //
    {
        //
        // if there are fatal issues, then only send fatal issues
        //
        if (!fatalIssues.empty()) {
            
            nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(fatalIssues))));
            
        } else {
            
            nodes.push_back(NodePtr(new CollectedIssuesNode(std::move(nonFatalIssues))));
        }
    }
#else
    {
        
        nodes.push_back(NodePtr(new CollectedIssuesNode({})));
    }
#endif // !NISSUES
    
    {
        auto& SimpleLineContinuations = TheCharacterDecoder->getSimpleLineContinuations();
        
        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(SimpleLineContinuations))));
    }
    
    {
        auto& ComplexLineContinuations = TheCharacterDecoder->getComplexLineContinuations();
        
        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(ComplexLineContinuations))));
    }
    
    {
        auto& EmbeddedNewlines = TheTokenizer->getEmbeddedNewlines();
        
        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(EmbeddedNewlines))));
    }
    
    {
        std::set<SourceLocation> tabs;
        
        auto& TokenizerEmbeddedTabs = TheTokenizer->getEmbeddedTabs();
        for (auto& T : TokenizerEmbeddedTabs) {
            tabs.insert(T);
        }
        
        auto& CharacterDecoderEmbeddedTabs = TheCharacterDecoder->getEmbeddedTabs();
        for (auto& T : CharacterDecoderEmbeddedTabs) {
            tabs.insert(T);
        }
        
        nodes.push_back(NodePtr(new CollectedSourceLocationsNode(std::move(tabs))));
    }
    
    auto C = new NodeContainer(std::move(nodes));
    
    return C;
}

NodeContainerPtr ParserSession::safeString() {
    
    std::vector<NodePtr> nodes;
    
    //
    // read all characters, just to set unsafeCharacterEncoding flag if necessary
    //
    while (true) {
        
        auto Char = TheByteDecoder->nextSourceCharacter0(TOPLEVEL);
        
        if (Char.isEndOfFile()) {
            break;
        }
        
    } // while (true)
    
    auto N = NodePtr(new SafeStringNode(bufAndLen));
    
    nodes.push_back(std::move(N));
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        auto N = NodePtr(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag));

        nodes.push_back(std::move(N));
    }
    
    auto C = new NodeContainer(std::move(nodes));
    
    return C;
}

//
// Does the file currently have permission to be read?
//
bool validatePath(WolframLibraryData libData, BufferAndLength bufAndLen) {
    
    if (!libData) {
        //
        // If running as a stand-alone executable, then always valid
        //
        return true;
    }
    
    auto inStr1 = reinterpret_cast<const char *>(bufAndLen.buffer);
    
    auto inStr2 = const_cast<char *>(inStr1);
    
    auto valid = libData->validatePath(inStr2, 'R');
    return valid;
}

void ParserSession::releaseContainer(NodeContainerPtr C) {
    delete C;
}

#if !NABORT
bool ParserSession::isAbort() const {
    
    if (!currentAbortQ) {
        return false;
    }

    return currentAbortQ();
}
#endif // !NABORT

void ParserSession::setUnsafeCharacterEncodingFlag(UnsafeCharacterEncodingFlag flag) {
    unsafeCharacterEncodingFlag = flag;
}

#if !NISSUES
void ParserSession::addIssue(IssuePtr I) {

    if (I->Sev == STRING_FATAL) {
        
        //
        // There may be situations where many (1000+) fatal errors are generated.
        // This has a noticeable impact on time to transfer for something that should be instantaneous.
        //
        // If there are, say, 10 fatal errors, then assume that the 11th is not going to give any new information,
        // and ignore.
        //
        if (fatalIssues.size() >= 10) {
            return;
        }

        fatalIssues.insert(std::move(I));

    } else {

        nonFatalIssues.insert(std::move(I));
    }
}
#endif // !NISSUES

void ParserSessionCreate() {
    TheParserSession = ParserSessionPtr(new ParserSession());
}

void ParserSessionDestroy() {
    TheParserSession.reset(nullptr);
}

void ParserSessionInit(Buffer buf,
                      size_t bufLen,
                      WolframLibraryData libData,
                      SourceConvention srcConvention,
                      uint32_t tabWidth,
                      FirstLineBehavior firstLineBehavior,
                      EncodingMode encodingMode) {
    BufferAndLength bufAndLen = BufferAndLength(buf, bufLen);
    TheParserSession->init(bufAndLen, libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
}

void ParserSessionDeinit() {
    TheParserSession->deinit();
}

NodeContainerPtr ParserSessionParseExpressions() {
    return TheParserSession->parseExpressions();
}

NodeContainerPtr ParserSessionTokenize() {
    return TheParserSession->tokenize();
}

NodeContainerPtr ParserSessionConcreteParseLeaf(StringifyMode mode) {
    return TheParserSession->concreteParseLeaf(mode);
}

NodeContainerPtr ParserSessionSafeString() {
    return TheParserSession->safeString();
}

void ParserSessionReleaseContainer(NodeContainerPtr C) {
    TheParserSession->releaseContainer(C);
}

ParserSessionPtr TheParserSession = nullptr;



mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

int WolframLibrary_initialize(WolframLibraryData libData) {
    
    ParserSessionCreate();
    
    return 0;
}

void WolframLibrary_uninitialize(WolframLibraryData libData) {
    ParserSessionDestroy();
}

#if USE_EXPR_LIB
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bytes = MArgument_getMNumericArray(Args[0]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[1]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    auto tabWidth = MArgument_getInteger(Args[2]);
    
    auto mlFirstLineBehavior = MArgument_getInteger(Args[3]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(bytes);
    
    auto data = reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(bytes));
    
    auto bufAndLen = BufferAndLength(data, numBytes);
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionParseExpressions();
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->numericarrayLibraryFunctions->MNumericArray_disown(bytes);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
DLLEXPORT int ConcreteParseBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
        
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(mlp, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArrayPtr(new ScopedMLByteArray(mlp));
    if (!arr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(mlp, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(mlp, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(mlp, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
        
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionParseExpressions();
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int TokenizeBytes_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bytes = MArgument_getMNumericArray(Args[0]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[1]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    auto tabWidth = MArgument_getInteger(Args[2]);
    
    auto mlFirstLineBehavior = MArgument_getInteger(Args[3]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    
    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(bytes);
    
    auto data = reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(bytes));
    
    auto bufAndLen = BufferAndLength(data, numBytes);
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionTokenize();
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->numericarrayLibraryFunctions->MNumericArray_disown(bytes);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int TokenizeBytes_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
        
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 4) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(mlp, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArrayPtr(new ScopedMLByteArray(mlp));
    if (!arr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(mlp, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(mlp, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(mlp, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
        
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionTokenize();
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 6) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto inStrRaw = MArgument_getUTF8String(Args[0]);
    auto inStr = std::string(inStrRaw);
    
    auto stringifyMode = MArgument_getInteger(Args[1]);
    
    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    auto tabWidth = MArgument_getInteger(Args[3]);
    
    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    auto mlEncodingMode = MArgument_getInteger(Args[5]);
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);
    
    
    auto bufAndLen = BufferAndLength(reinterpret_cast<Buffer>(inStr.c_str()), inStr.size());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
    
    auto C = ParserSessionConcreteParseLeaf(static_cast<StringifyMode>(stringifyMode));
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->UTF8String_disown(inStrRaw);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int ConcreteParseLeaf_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    std::string unescaped;
    
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen))  {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 6) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto inStr = ScopedMLUTF8StringPtr(new ScopedMLUTF8String(mlp));
    if (!inStr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int stringifyMode;
    if (!MLGetInteger(mlp, &stringifyMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlSrcConvention;
    if (!MLGetInteger(mlp, &mlSrcConvention)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);
    
    int tabWidth;
    if (!MLGetInteger(mlp, &tabWidth)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    int mlFirstLineBehavior;
    if (!MLGetInteger(mlp, &mlFirstLineBehavior)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);
    
    int mlEncodingMode;
    if (!MLGetInteger(mlp, &mlEncodingMode)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto encodingMode = static_cast<EncodingMode>(mlEncodingMode);

    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bufAndLen = BufferAndLength(inStr->get(), inStr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
    
    auto C = ParserSessionConcreteParseLeaf(static_cast<StringifyMode>(stringifyMode));
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_EXPR_LIB
int SafeString_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    
    if (Argc != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bytes = MArgument_getMNumericArray(Args[0]);
    
    size_t numBytes;
    numBytes = libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(bytes);
    
    auto data = reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(bytes));
    
    auto bufAndLen = BufferAndLength(data, numBytes);
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionSafeString();
    
    auto e = C->toExpr();
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    libData->numericarrayLibraryFunctions->MNumericArray_disown(bytes);
    
    MArgument_setInteger(Res, reinterpret_cast<mint>(e));
    
    return LIBRARY_NO_ERROR;
}
#elif USE_MATHLINK
int SafeString_LibraryLink(WolframLibraryData libData, MLINK mlp) {
    
    int mlLen;
    
    if (!MLTestHead(mlp, SYMBOL_LIST.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLTestHead(mlp, SYMBOL_BYTEARRAY.name(), &mlLen)) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    len = static_cast<size_t>(mlLen);
    
    if (len != 1) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto arr = ScopedMLByteArrayPtr(new ScopedMLByteArray(mlp));
    if (!arr->read()) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    if (!MLNewPacket(mlp) ) {
        return LIBRARY_FUNCTION_ERROR;
    }
    
    auto bufAndLen = BufferAndLength(arr->get(), arr->getByteCount());
    
    ParserSessionInit(bufAndLen.buffer, bufAndLen.length(), libData, SOURCECONVENTION_LINECOLUMN, DEFAULT_TAB_WIDTH, FIRSTLINEBEHAVIOR_NOTSCRIPT, ENCODINGMODE_NORMAL);
    
    auto C = ParserSessionSafeString();
    
    C->put(mlp);
    
    ParserSessionReleaseContainer(C);
    
    ParserSessionDeinit();
    
    return LIBRARY_NO_ERROR;
}
#endif // USE_EXPR_LIB


#if USE_MATHLINK
ScopedMLUTF8String::ScopedMLUTF8String(MLINK mlp) : mlp(mlp), buf(NULL), b(), c() {}

ScopedMLUTF8String::~ScopedMLUTF8String() {
    
    if (buf == NULL) {
        return;
    }
    
    MLReleaseUTF8String(mlp, buf, b);
}

bool ScopedMLUTF8String::read() {
    return MLGetUTF8String(mlp, &buf, &b, &c);
}

Buffer ScopedMLUTF8String::get() const {
    return buf;
}

size_t ScopedMLUTF8String::getByteCount() const {
    return b;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLString::ScopedMLString(MLINK mlp) : mlp(mlp), buf(NULL) {}

ScopedMLString::~ScopedMLString() {
    
    if (buf == NULL) {
        return;
    }
    
    MLReleaseString(mlp, buf);
}

bool ScopedMLString::read() {
    return MLGetString(mlp, &buf);
}

const char *ScopedMLString::get() const {
    return buf;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLSymbol::ScopedMLSymbol(MLINK mlp) : mlp(mlp), sym(NULL) {}

ScopedMLSymbol::~ScopedMLSymbol() {
    if (sym != NULL) {
        MLReleaseSymbol(mlp, sym);
    }
}

bool ScopedMLSymbol::read() {
    return MLGetSymbol(mlp, &sym);
}

const char *ScopedMLSymbol::get() const {
    return sym;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLFunction::ScopedMLFunction(MLINK mlp) : mlp(mlp), func(NULL), count() {}

ScopedMLFunction::~ScopedMLFunction() {
    
    if (func == NULL) {
        return;
    }
    
    MLReleaseSymbol(mlp, func);
}

bool ScopedMLFunction::read() {
    return MLGetFunction(mlp, &func, &count);
}

const char *ScopedMLFunction::getHead() const {
    return func;
}

int ScopedMLFunction::getArgCount() const {
    return count;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLByteArray::ScopedMLByteArray(MLINK mlp) : mlp(mlp), buf(NULL), dims(), heads(), depth() {}

ScopedMLByteArray::~ScopedMLByteArray() {
    
    if (buf == NULL) {
        return;
    }
    
    MLReleaseByteArray(mlp, buf, dims, heads, depth);
}

bool ScopedMLByteArray::read() {
    return MLGetByteArray(mlp, &buf, &dims, &heads, &depth);
}

Buffer ScopedMLByteArray::get() const {
    return buf;
}

size_t ScopedMLByteArray::getByteCount() const {
    return dims[0];
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLEnvironmentParameter::ScopedMLEnvironmentParameter() : p(MLNewParameters(MLREVISION, MLAPIREVISION)) {}

ScopedMLEnvironmentParameter::~ScopedMLEnvironmentParameter() {
    MLReleaseParameters(p);
}

MLEnvironmentParameter ScopedMLEnvironmentParameter::get() {
    return p;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
ScopedMLLoopbackLink::ScopedMLLoopbackLink() : mlp(NULL), ep(NULL) {
    
    ScopedMLEnvironmentParameterPtr p;
    
    int err;
    
#ifdef WINDOWS_MATHLINK
#else
    //
    // Needed because MathLink intercepts all signals
    //
    MLDoNotHandleSignalParameter(p.get(), SIGINT);
#endif // WINDOWS_MATHLINK
    
    ep = MLInitialize(p.get());
    
    if (ep == (MLENV)0) {
        
        return;
    }
    
    mlp = MLLoopbackOpen(ep, &err);
}

ScopedMLLoopbackLink::~ScopedMLLoopbackLink() {
    
    if (mlp == NULL) {
        return;
    }
    
    MLClose(mlp);
    
    MLDeinitialize(ep);
}

MLINK ScopedMLLoopbackLink::get() {
    return mlp;
}
#endif // USE_MATHLINK


#if USE_MATHLINK
void NodeContainer::put(MLINK mlp) const {
    
    if (!MLPutFunction(mlp, SYMBOL_LIST.name(), static_cast<int>(N.size()))) {
        assert(false);
    }
    
    for (auto& NN : N) {
        
#if !NABORT
        if (TheParserSession->isAbort()) {
            SYMBOL__ABORTED.put(mlp);
            continue;
        }
#endif // !NABORT
        
        NN->put(mlp);
    }
}
#endif // USE_MATHLINK

#if USE_EXPR_LIB
expr NodeContainer::toExpr() const {
    
    auto head = SYMBOL_LIST.toExpr();
        
    auto e = Expr_BuildExprA(head, static_cast<int>(N.size()));
    
    for (size_t i = 0; i < N.size(); i++) {
        
        //
        // Check isAbort() inside loops
        //
#if !NABORT
        if (TheParserSession->isAbort()) {
            Expr_InsertA(e, i + 1, SYMBOL__ABORTED.toExpr());
            continue;
        }
#endif // !NABORT
        
        auto& NN = N[i];
        auto NExpr = NN->toExpr();
        Expr_InsertA(e, i + 1, NExpr);
    }
    
    return e;
}
#endif // USE_EXPR_LIB


#if DIAGNOSTICS

int CharacterDecoder_LineContinuationCount = 0;

int CharacterDecoder_LongNameCount = 0;

int CharacterDecoder_4HexCount = 0;

int CharacterDecoder_2HexCount = 0;

int CharacterDecoder_6HexCount = 0;

int CharacterDecoder_OctalCount = 0;

int CharacterDecoder_StringMetaBackspace = 0;

int CharacterDecoder_StringMetaFormFeed = 0;

int CharacterDecoder_StringMetaLineFeedCount = 0;

int CharacterDecoder_StringMetaCarriageReturn = 0;

int CharacterDecoder_StringMetaTab = 0;

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

#endif // DIAGNOSTICS
