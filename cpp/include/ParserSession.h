
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
class SourceConventionManager;

using IssuePtr = Issue *;
using IssuePtrVector = std::vector<IssuePtr>;
using ParserSessionPtr = ParserSession *;
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
    
    IssuePtrVector fatalIssues;
    IssuePtrVector nonFatalIssues;
    
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
    
    Token concreteParseLeaf0(StringifyMode mode);
    
public:
    
    ParserSession();
    
    int init(BufferAndLength bufAndLen, WolframLibraryData libData, ParserSessionOptions opts);
    
    void deinit();
    
    bool abortQ() const;
    
    NodePtr concreteParse();
    NodePtr tokenize();
    NodePtr concreteParseLeaf(StringifyMode mode);
    NodePtr safeString();
    
    void releaseNode(NodePtr N);
    
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
