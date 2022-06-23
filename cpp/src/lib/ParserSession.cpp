
#include "ParserSession.h"

#include "Parser.h" // for Parser
#include "ParseletRegistration.h" // for prefixParselets
#include "Parselet.h" // for Parselet impls
#include "Tokenizer.h" // for Tokenizer
#include "CharacterDecoder.h" // for CharacterDecoder
#include "ByteDecoder.h" // for ByteDecoder
#include "ByteBuffer.h" // for ByteBuffer

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS


bool validatePath(WolframLibraryData libData, const unsigned char *inStr, size_t len);


ParserSession::ParserSession() : fatalIssues(), nonFatalIssues(), currentAbortQ(), unsafeCharacterEncodingFlag(), bufAndLen(), libData(), srcConvention(), tabWidth(), firstLineBehavior(), encodingMode() {
    
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
        
#if CHECK_ABORT
    if (libDataIn) {
        
        currentAbortQ = [libDataIn]() {
            
            //
            // AbortQ() returns a mint
            //
            bool res = libDataIn->AbortQ();
            
            return res;
        };
        
    } else {
        
        currentAbortQ = nullptr;
    }
#endif // CHECK_ABORT
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
    
#if DIAGNOSTICS
    DiagnosticsLog("enter parseExpressions");
    DiagnosticsMarkTime();
#endif // DIAGNOSTICS
    
    std::vector<NodeVariant> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodeVariant> exprs;
        
        while (true) {
            
#if CHECK_ABORT
            if (TheParserSession->isAbort()) {
                
                break;
            }
#endif // CHECK_ABORT
            
            auto peek = TheParser->currentToken(TOPLEVEL);
            
            if (peek.Tok == TOKEN_ENDOFFILE) {
                break;
            }
            
            if (peek.Tok.isTrivia()) {
                
                exprs.push_back(peek);
                
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
            
            exprs.push_back(TheParser->popNode());
            
            assert(TheParser->isQuiescent());
            
        } // while (true)
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        std::vector<NodeVariant> exprs;
        
        exprs.push_back(NodePtr(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag)));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    //
    // Now handle the out-of-band expressions, i.e., issues and metadata
    //
    {
#if CHECK_ISSUES
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
        
#endif // CHECK_ISSUES
    }
    
#if COMPUTE_OOB
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
#else
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
    
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
    
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
    
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
#endif // COMPUTE_OOB
    
    auto C = new NodeContainer(std::move(nodes));
    
#if DIAGNOSTICS
    DiagnosticsLog("exit parseExpressions");
    DiagnosticsLogTime();
#endif // DIAGNOSTICS
    
    return C;
}

NodeContainerPtr ParserSession::tokenize() {
    
    std::vector<NodeVariant> nodes;
    
    while (true) {
        
#if CHECK_ABORT
        if (TheParserSession->isAbort()) {
            break;
        }
#endif // CHECK_ABORT
        
        auto Tok = TheTokenizer->currentToken(TOPLEVEL);
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            break;
        }
        
        nodes.push_back(Tok);
        
        TheTokenizer->nextToken(Tok);
        
    } // while (true)
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        auto N = NodePtr(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag));

        nodes.push_back(std::move(N));
    }
    
    auto C = new NodeContainer(std::move(nodes));
    
    return C;
}


NodeVariant ParserSession::concreteParseLeaf0(int mode) {
    
    switch (mode) {
        case STRINGIFYMODE_NORMAL: {
            
            auto Tok = TheTokenizer->nextToken0(TOPLEVEL);
            
            return Tok;
        }
        case STRINGIFYMODE_TAG: {
            
            auto Tok = TheTokenizer->nextToken0_stringifyAsTag();
            
            return Tok;
        }
        case STRINGIFYMODE_FILE: {
            
            auto Tok = TheTokenizer->nextToken0_stringifyAsFile();
            
            return Tok;
        }
        default: {
            
            assert(false);
            
            return nullptr;
        }
    }
}

NodeContainerPtr ParserSession::concreteParseLeaf(StringifyMode mode) {
    
    std::vector<NodeVariant> nodes;
    
    //
    // Collect all expressions
    //
    {
        std::vector<NodeVariant> exprs;
        
        exprs.push_back(concreteParseLeaf0(mode));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        std::vector<NodeVariant> exprs;
        
        auto node = NodePtr(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag));
        
        exprs.push_back(std::move(node));
        
        NodePtr Collected = NodePtr(new CollectedExpressionsNode(std::move(exprs)));
        
        nodes.push_back(std::move(Collected));
    }
    
#if CHECK_ISSUES
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
#endif // CHECK_ISSUES
    
#if COMPUTE_OOB
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
#else
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
    
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
    
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
    
    nodes.push_back(NodePtr(new CollectedSourceLocationsNode({})));
#endif // COMPUTE_OOB
    
    auto C = new NodeContainer(std::move(nodes));
    
    return C;
}

NodeContainerPtr ParserSession::safeString() {
    
    std::vector<NodeVariant> nodes;
    
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

void ParserSession::releaseContainer(NodeContainerPtr C) {
    
#if DIAGNOSTICS
    DiagnosticsLog("before delete C");
    DiagnosticsMarkTime();
#endif // DIAGNOSTICS
    
    delete C;
    
#if DIAGNOSTICS
    DiagnosticsLog("after delete C");
    DiagnosticsLogTime();
#endif // DIAGNOSTICS
}

bool ParserSession::isAbort() const {
    
    if (!currentAbortQ) {
        return false;
    }

    return currentAbortQ();
}

void ParserSession::setUnsafeCharacterEncodingFlag(UnsafeCharacterEncodingFlag flag) {
    unsafeCharacterEncodingFlag = flag;
}

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


ParserSessionPtr TheParserSession = nullptr;


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
