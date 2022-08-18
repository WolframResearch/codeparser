
#include "API.h"

#include "Token.h"
#include "Parser.h" // for Context

#if USE_MATHLINK
#include "mathlink.h"
#undef P
#endif // USE_MATHLINK

#include <set>
#include <variant>
#include <vector>

class ParserSession;
class Issue;
class IssueCompare;
class Node;
class SourceConventionManager;

using IssuePtr = Issue *;
using IssuePtrSet = std::set<IssuePtr, IssuePtrCompare>;
using ParserSessionPtr = ParserSession *;
using NodePtr = Node *;
using NodeVariant = std::variant<NodePtr, Token>;
using SourceConventionManagerPtr = SourceConventionManager *;


constexpr int PARSERSESSIONINIT_OK = 0;
constexpr int PARSERSESSIONINIT_ERROR = 1;

//
// A parser session
//
class ParserSession {
public:
    
    Buffer start;
    Buffer end;
    Buffer buffer;
    
    WolframLibraryData libData;
    ParserSessionOptions opts;
    
    UnsafeCharacterEncodingFlag unsafeCharacterEncodingFlag;
    
    SourceConventionManagerPtr srcConventionManager;
    SourceLocation SrcLoc;
    
    IssuePtrSet fatalIssues;
    IssuePtrSet nonFatalIssues;
    
    std::set<SourceLocation> SimpleLineContinuations;
    std::set<SourceLocation> ComplexLineContinuations;
    std::set<SourceLocation> EmbeddedNewlines;
    std::set<SourceLocation> EmbeddedTabs;
    
    std::vector<NodeVariant> NodeStack;
    std::vector<Context> ContextStack;
    std::vector<Closer> GroupStack;
    
    TriviaSeq trivia1;
    TriviaSeq trivia2;
    
private:
    
    NodeVariant concreteParseLeaf0(StringifyMode mode);
    
public:
    
    ParserSession();
    
    int init(BufferAndLength bufAndLen, WolframLibraryData libData, ParserSessionOptions opts);
    
    void deinit();
    
    bool abortQ() const;
    
    NodeContainerPtr parseExpressions();
    NodeContainerPtr tokenize();
    NodeContainerPtr concreteParseLeaf(StringifyMode mode);
    NodeContainerPtr safeString();
    
    void releaseNodeContainer(NodeContainerPtr C);
    
    void setUnsafeCharacterEncodingFlag(UnsafeCharacterEncodingFlag flag);
    
    void addIssue(IssuePtr I);
    
    void addSimpleLineContinuation(SourceLocation Loc);
    void addComplexLineContinuation(SourceLocation Loc);
    void addEmbeddedNewline(SourceLocation Loc);
    void addEmbeddedTab(SourceLocation Loc);
    
#if USE_MATHLINK
    MLINK getSessionMathLink() const;
#endif // USE_MATHLINK
};
