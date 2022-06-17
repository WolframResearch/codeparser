
#include "API.h"

#include <functional> // for function with GCC and MSVC
#include <set>

class ParserSession;
class Issue;
class IssuePtrCompare;
class Node;

using IssuePtr = std::shared_ptr<Issue>;
using IssuePtrSet = std::set<IssuePtr, IssuePtrCompare>;
using ParserSessionPtr = std::unique_ptr<ParserSession>;


//
// A parser session
//
class ParserSession {
private:
    
    IssuePtrSet fatalIssues;
    IssuePtrSet nonFatalIssues;
    
    Node *concreteParseLeaf0(int mode);
    
public:
    
#if !NABORT
    std::function<bool ()> currentAbortQ;
#endif // !NABORT
    
    UnsafeCharacterEncodingFlag unsafeCharacterEncodingFlag;
    
    BufferAndLength bufAndLen;
    WolframLibraryData libData;
    SourceConvention srcConvention;
    uint32_t tabWidth;
    FirstLineBehavior firstLineBehavior;
    EncodingMode encodingMode;
    
    
    ParserSession();
    
    ~ParserSession();
    
    void init(
        BufferAndLength bufAndLen,
        WolframLibraryData libData,
        SourceConvention srcConvention,
        uint32_t tabWidth,
        FirstLineBehavior firstLineBehavior,
        EncodingMode encodingMode
    );
    
    void deinit();
    
    NodeContainerPtr parseExpressions();
    NodeContainerPtr tokenize();
    NodeContainerPtr concreteParseLeaf(StringifyMode mode);
    NodeContainerPtr safeString();
    
    void releaseContainer(NodeContainerPtr C);
    
#if !NABORT
    bool isAbort() const;
#endif // !NABORT
    
    void setUnsafeCharacterEncodingFlag(UnsafeCharacterEncodingFlag flag);
    
#if !NISSUES
    void addIssue(IssuePtr);
#endif // !NISSUES
};

extern ParserSessionPtr TheParserSession;
