
#include "API.h"

#include "Token.h"
#include "ByteDecoder.h" // for SourceConventionManager
#include "Parser.h" // for Context

#include <set>
#include <variant>
#include <vector>

class ParserSession;
class Issue;
class IssueCompare;
class Node;

using IssuePtr = Issue *;
using IssuePtrSet = std::set<IssuePtr, IssuePtrCompare>;
using ParserSessionPtr = ParserSession *;
using NodePtr = Node *;
using NodeVariant = std::variant<NodePtr, Token>;


//
// A parser session
//
class ParserSession {
private:
    
    NodeVariant concreteParseLeaf0(int mode);
    
public:
    
    Buffer start;
    Buffer end;
    bool wasEOF;
    Buffer buffer;
    
    WolframLibraryData libData;
    SourceConvention srcConvention;
    uint32_t tabWidth;
    FirstLineBehavior firstLineBehavior;
    EncodingMode encodingMode;
    
    UnsafeCharacterEncodingFlag unsafeCharacterEncodingFlag;
    
    SourceConventionManagerPtr srcConventionManager;
    SourceLocation SrcLoc;
    
private:
    IssuePtrSet fatalIssues;
    IssuePtrSet nonFatalIssues;
    
public:
    std::set<SourceLocation> SimpleLineContinuations;
    std::set<SourceLocation> ComplexLineContinuations;
    std::set<SourceLocation> EmbeddedNewlines;
    std::set<SourceLocation> EmbeddedTabs;
    
    std::vector<NodeVariant> NodeStack;
    std::vector<Context> ContextStack;
    std::vector<Closer> GroupStack;
    
    TriviaSeq trivia1;
    TriviaSeq trivia2;
    
    
    ParserSession();
    
    void init(BufferAndLength bufAndLen, WolframLibraryData libData, SourceConvention srcConvention, uint32_t tabWidth, FirstLineBehavior firstLineBehavior, EncodingMode encodingMode);
    
    void deinit();
    
    bool abortQ() const;
    
    NodeContainerPtr parseExpressions();
    NodeContainerPtr tokenize();
    NodeContainerPtr concreteParseLeaf(StringifyMode mode);
    NodeContainerPtr safeString();
    
    void releaseContainer(NodeContainerPtr C);
    
    void setUnsafeCharacterEncodingFlag(UnsafeCharacterEncodingFlag flag);
    
    void addIssue(IssuePtr I);
    
    void addSimpleLineContinuation(SourceLocation Loc);
    void addComplexLineContinuation(SourceLocation Loc);
    void addEmbeddedNewline(SourceLocation Loc);
    void addEmbeddedTab(SourceLocation Loc);
    
#if USE_MATHLINK
    MLINK getMathLink() const;
    
    bool processMathLink() const;
#endif // USE_MATHLINK
};
