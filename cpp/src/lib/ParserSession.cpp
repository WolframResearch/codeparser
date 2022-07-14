
#include "ParserSession.h"

#include "Parser.h" // for Parser
#include "ParseletRegistration.h" // for prefixParselets
#include "Parselet.h" // for Parselet impls
#include "Tokenizer.h" // for Tokenizer
#include "CharacterDecoder.h" // for CharacterDecoder
#include "ByteDecoder.h" // for ByteDecoder
#include "ByteBuffer.h" // for ByteBuffer
#include "MyStringRegistration.h"

#if DIAGNOSTICS
#include "Diagnostics.h"
#endif // DIAGNOSTICS


bool validatePath(WolframLibraryData libData, Buffer inStr);


ParserSession::ParserSession() : start(), end(), wasEOF(), buffer(), libData(), srcConvention(), tabWidth(), firstLineBehavior(), encodingMode(), unsafeCharacterEncodingFlag(), srcConventionManager(), SrcLoc(), fatalIssues(), nonFatalIssues(), SimpleLineContinuations(), ComplexLineContinuations(), EmbeddedNewlines(), EmbeddedTabs(), NodeStack(), ContextStack(), GroupStack(), trivia1(), trivia2() {}

int ParserSession::init(BufferAndLength bufAndLenIn, WolframLibraryData libDataIn, SourceConvention srcConventionIn, uint32_t tabWidthIn, FirstLineBehavior firstLineBehaviorIn, EncodingMode encodingModeIn) {
    
    start = bufAndLenIn.Buf;
    end = bufAndLenIn.end();
    wasEOF = false;
    buffer = start;
    
    libData = libDataIn;
    srcConvention = srcConventionIn;
    tabWidth = tabWidthIn;
    firstLineBehavior = firstLineBehaviorIn;
    encodingMode = encodingModeIn;
    
    unsafeCharacterEncodingFlag = UNSAFECHARACTERENCODING_OK;
    
#if COMPUTE_SOURCE
    switch (srcConvention) {
        case SOURCECONVENTION_LINECOLUMN: {
            
            srcConventionManager = new LineColumnManager();
            
            break;
        }
        case SOURCECONVENTION_SOURCECHARACTERINDEX: {
            
            srcConventionManager = new SourceCharacterIndexManager();
            
            break;
        }
        default: {
            return PARSERSESSIONINIT_ERROR;
        }
    }
    
    SrcLoc = srcConventionManager->newSourceLocation();
#endif // COMPUTE_SOURCE
    
    fatalIssues.clear();
    nonFatalIssues.clear();
    
    SimpleLineContinuations.clear();
    ComplexLineContinuations.clear();
    EmbeddedNewlines.clear();
    EmbeddedTabs.clear();
    
    NodeStack.clear();
    ContextStack.clear();
    GroupStack.clear();
    
    trivia1.clear();
    trivia2.clear();
    
    Parser_handleFirstLine(this);
    
    return PARSERSESSIONINIT_OK;
}

void ParserSession::deinit() {
    
#if COMPUTE_SOURCE
    delete srcConventionManager;
#endif // COMPUTE_SOURCE
}

NodeContainerPtr ParserSession::parseExpressions() {
    
#if DIAGNOSTICS
    DiagnosticsLog("enter parseExpressions");
    DiagnosticsMarkTime();
#endif // DIAGNOSTICS
    
    NodeSeq nodes;
    
    //
    // Collect all expressions
    //
    
    NodeSeq exprs;
    
    while (true) {
            
#if CHECK_ABORT
        if (abortQ()) {
            break;
        }
#endif // CHECK_ABORT
            
        auto peek = Tokenizer_currentToken(this, TOPLEVEL);
        
        if (peek.Tok == TOKEN_ENDOFFILE) {
            break;
        }
        
        if (peek.Tok.isTrivia()) {
            
            exprs.push(peek);
            
            peek.skip(this);
            
            continue;
        }
        
        //
        // special top-level handling of stray closers
        //
        if (peek.Tok.isCloser()) {
            
            PrefixToplevelCloserParselet_parsePrefix(this, prefixToplevelCloserParselet, peek);
            
            exprs.push(Parser_popNode(this));
            
            assert(Parser_isQuiescent(this));
            
            continue;
        }
            
        auto P = prefixParselets[peek.Tok.value()];
        
        (P->parsePrefix())(this, P, peek);
        
        exprs.push(Parser_popNode(this));
        
        assert(Parser_isQuiescent(this));
        
    } // while (true)
    
    auto Collected = new CollectedExpressionsNode(std::move(exprs));
    
    nodes.push(Collected);
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        NodeSeq exprs;
        
        exprs.push(new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag));
        
        auto Collected = new CollectedExpressionsNode(std::move(exprs));
        
        nodes.push(Collected);
    }
    
    //
    // Now handle the out-of-band expressions, i.e., issues and metadata
    //
    
    //
    // if there are fatal issues, then only send fatal issues
    //
    if (!fatalIssues.empty()) {
        
        nodes.push(new CollectedIssuesNode(fatalIssues));
        
    } else {
        
        nodes.push(new CollectedIssuesNode(nonFatalIssues));
    }
    
    nodes.push(new CollectedSourceLocationsNode(SimpleLineContinuations));
    nodes.push(new CollectedSourceLocationsNode(ComplexLineContinuations));
    nodes.push(new CollectedSourceLocationsNode(EmbeddedNewlines));
    nodes.push(new CollectedSourceLocationsNode(EmbeddedTabs));
    
    auto C = new NodeContainer(std::move(nodes));
    
#if DIAGNOSTICS
    DiagnosticsLog("exit parseExpressions");
    DiagnosticsLogTime();
#endif // DIAGNOSTICS
    
    return C;
}

NodeContainerPtr ParserSession::tokenize() {
    
    NodeSeq nodes;
    
    while (true) {
        
#if CHECK_ABORT
        if (abortQ()) {
            break;
        }
#endif // CHECK_ABORT
        
        auto Tok = Tokenizer_currentToken(this, TOPLEVEL);
        
        if (Tok.Tok == TOKEN_ENDOFFILE) {
            break;
        }
        
        nodes.push(Tok);
        
        Tok.skip(this);
        
    } // while (true)
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        auto N = new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag);

        nodes.push(N);
    }
    
    return new NodeContainer(std::move(nodes));
}


NodeVariant ParserSession::concreteParseLeaf0(StringifyMode mode) {
    
    switch (mode) {
        case STRINGIFYMODE_NORMAL: {
            return Tokenizer_nextToken(this, TOPLEVEL);
        }
        case STRINGIFYMODE_TAG: {
            return Tokenizer_nextToken_stringifyAsTag(this);
        }
        case STRINGIFYMODE_FILE: {
            return Tokenizer_nextToken_stringifyAsFile(this);
        }
    }
    
    assert(false);
    
    return nullptr;
}

NodeContainerPtr ParserSession::concreteParseLeaf(StringifyMode mode) {
    
    NodeSeq nodes;
    
    //
    // Collect all expressions
    //
    
    NodeSeq exprs;
    
    exprs.push(concreteParseLeaf0(mode));
    
    auto Collected = new CollectedExpressionsNode(std::move(exprs));
    
    nodes.push(Collected);
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        NodeSeq exprs;
        
        auto node = new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag);
        
        exprs.push(std::move(node));
        
        auto Collected = new CollectedExpressionsNode(std::move(exprs));
        
        nodes.push(std::move(Collected));
    }
    
    //
    // Collect all issues from the various components
    //
    
    //
    // if there are fatal issues, then only send fatal issues
    //
    if (!fatalIssues.empty()) {
        
        nodes.push(new CollectedIssuesNode(fatalIssues));
        
    } else {
        
        nodes.push(new CollectedIssuesNode(nonFatalIssues));
    }
    
    nodes.push(new CollectedSourceLocationsNode(SimpleLineContinuations));
    nodes.push(new CollectedSourceLocationsNode(ComplexLineContinuations));
    nodes.push(new CollectedSourceLocationsNode(EmbeddedNewlines));
    nodes.push(new CollectedSourceLocationsNode(EmbeddedTabs));
    
    return new NodeContainer(std::move(nodes));
}

NodeContainerPtr ParserSession::safeString() {
    
    NodeSeq nodes;
    
    //
    // read all characters, just to set unsafeCharacterEncoding flag if necessary
    //
    while (true) {
        
        auto Char = ByteDecoder_nextSourceCharacter(this, TOPLEVEL);
        
        if (Char.isEndOfFile()) {
            break;
        }
        
    } // while (true)
    
    auto N = new SafeStringNode(BufferAndLength(start, end - start));
    
    nodes.push(std::move(N));
    
    if (unsafeCharacterEncodingFlag != UNSAFECHARACTERENCODING_OK) {
        
        nodes.clear();
        
        auto N = new MissingBecauseUnsafeCharacterEncodingNode(unsafeCharacterEncodingFlag);

        nodes.push(std::move(N));
    }
    
    return new NodeContainer(std::move(nodes));
}

void ParserSession::releaseNodeContainer(NodeContainerPtr C) {
    
    C->release();
    
    delete C;
}

bool ParserSession::abortQ() const {
    
    if (!libData) {
        return false;
    }
    
    //
    // AbortQ() returns a mint
    //
    return libData->AbortQ();
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

        fatalIssues.insert(I);

    } else {
        
        nonFatalIssues.insert(I);
    }
}

void ParserSession::addSimpleLineContinuation(SourceLocation Loc) {
    SimpleLineContinuations.insert(Loc);
}

void ParserSession::addComplexLineContinuation(SourceLocation Loc) {
    ComplexLineContinuations.insert(Loc);
}

void ParserSession::addEmbeddedNewline(SourceLocation Loc) {
    EmbeddedNewlines.insert(Loc);
}

void ParserSession::addEmbeddedTab(SourceLocation Loc) {
    EmbeddedTabs.insert(Loc);
}

#if USE_MATHLINK
MLINK ParserSession::getSessionMathLink() const {
    return libData->getMathLink(libData);
}
#endif // USE_MATHLINK

//
// Does the file currently have permission to be read?
//
bool validatePath(WolframLibraryData libData, Buffer inStr) {
    
    if (!libData) {
        //
        // If running as a stand-alone executable, then always valid
        //
        return true;
    }
    
    auto inStr1 = reinterpret_cast<const char *>(inStr);
    
    auto inStr2 = const_cast<char *>(inStr1);
    
    return libData->validatePath(inStr2, 'R');
}
