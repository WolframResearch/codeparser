use std::fmt::{self, Debug};

use crate::{
    feature,
    node::{BinaryNode, CompoundNode, Node, NodeSeq, TernaryNode, TriviaSeq},
    panic_if_aborted,
    parselet::{InfixParselet, ParseFunction, ParseletPtr},
    parselet_registration::infixParselets,
    // parselet::Parselet,
    parser_session::ParserSession,
    precedence::{Precedence, *},
    source::{NextPolicy, TOPLEVEL},
    symbol_registration::{SYMBOL_PATTERN, SYMBOL_SPAN, *},

    token::Token,
    token_enum::Closer,
    token_enum_registration::TokenEnum::*,
    tokenizer::{Tokenizer, Tokenizer_currentToken, Tokenizer_currentToken_stringifyAsFile},
    FirstLineBehavior,
};

pub struct Context {
    pub(crate) f: Option<ParseFunction>,
    pub(crate) p: Option<ParseletPtr>,

    index: usize,
    pub(crate) prec: Precedence,
}

impl Debug for Context {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let Context { f, p, index, prec } = self;

        fmt.debug_struct("Context")
            .field("f", &f.is_some())
            .field("p", p)
            .field("index", index)
            .field("prec", prec)
            .finish()
    }
}

pub(crate) enum ColonLHS {
    Pattern,
    Optional,
    Error,
}

impl Context {
    pub fn new(index: usize, prec: Precedence) -> Self {
        Context {
            f: None,
            p: None,
            index,
            prec,
        }
    }
}

pub fn Parser_handleFirstLine<'i>(session: &mut Tokenizer<'i>) {
    let firstLineBehavior = session.firstLineBehavior;

    match firstLineBehavior {
        FirstLineBehavior::NotScript => {
            return;
        },
        FirstLineBehavior::Check => {
            //
            // Handle the optional #! shebang
            //

            let mut peek = Tokenizer_currentToken(session, TOPLEVEL);

            if peek.tok != TOKEN_HASH {
                // not #!

                //
                // reset
                //
                // session.buffer = peek.buf;
                session.offset = peek.span.offset;
                session.SrcLoc = peek.src.start;

                return;
            }

            peek.skip(session);

            peek = Tokenizer_currentToken(session, TOPLEVEL);

            if peek.tok != TOKEN_BANG {
                // not #!

                //
                // reset
                //
                // session.buffer = peek.buf;
                session.offset = peek.span.offset;
                session.SrcLoc = peek.src.start;

                return;
            }

            //
            // Definitely a shebang
            //

            peek.skip(session);

            loop {
                if feature::CHECK_ABORT && crate::abortQ() {
                    break;
                }

                let peek = Tokenizer_currentToken(session, TOPLEVEL);

                if peek.tok == TOKEN_ENDOFFILE {
                    break;
                }

                if peek.tok == TOKEN_TOPLEVELNEWLINE {
                    peek.skip(session);

                    break;
                }

                peek.skip(session);
            } // while (true)

            //
            // TODO: if anyone ever asks, then consider providing the shebang as a token
            // but only after BIGCODEMERGE!!
            //
        },
        FirstLineBehavior::Script => {
            //
            // Handle the #! shebang
            //

            let mut peek = Tokenizer_currentToken(session, TOPLEVEL);

            if peek.tok != TOKEN_HASH {
                //
                // TODO: add to Issues
                //

                return;
            }

            peek.skip(session);

            peek = Tokenizer_currentToken(session, TOPLEVEL);

            if peek.tok != TOKEN_BANG {
                //
                // TODO: add to Issues
                //

                return;
            }

            peek.skip(session);

            loop {
                if feature::CHECK_ABORT && crate::abortQ() {
                    break;
                }

                let peek = Tokenizer_currentToken(session, TOPLEVEL);

                if peek.tok == TOKEN_ENDOFFILE {
                    break;
                }

                if peek.tok == TOKEN_TOPLEVELNEWLINE {
                    peek.skip(session);

                    break;
                }

                peek.skip(session);
            } // while (true)
        },
    }
}

pub(crate) fn Parser_parseClimb<'i>(
    session: &mut ParserSession<'i>,
    Ignored: ParseletPtr,
    Ignored2: Token,
) {
    //
    // Check isAbort() inside loops
    //
    panic_if_aborted!();

    let Trivia1 = session.trivia1.clone();

    let mut token = Tokenizer_currentToken(&mut session.tokenizer, TOPLEVEL);

    //
    // not in the middle of parsing anything, so toplevel newlines will delimit
    //
    Parser_eatTriviaButNotToplevelNewlines_2(
        session,
        &mut token,
        TOPLEVEL,
        &mut Trivia1.borrow_mut(),
    );

    let mut I: &dyn InfixParselet = infixParselets[usize::from(token.tok.value())];

    token = I.processImplicitTimes(session, token);

    I = infixParselets[usize::from(token.tok.value())];

    let TokenPrecedence = I.getPrecedence(session);

    //
    // if (Ctxt.Prec > TokenPrecedence)
    //   break;
    // else if (Ctxt.Prec == TokenPrecedence && Ctxt.Prec.Associativity is NonRight)
    //   break;
    //

    if (Parser_topPrecedence(session) | 0x1) > TokenPrecedence {
        Trivia1.borrow_mut().reset(&mut session.tokenizer);

        // MUSTTAIL
        return Parser_tryContinue(session, Ignored, Ignored2);
    }

    Parser_pushContext(session, TokenPrecedence);

    Parser_pushTriviaSeq(session, &mut Trivia1.borrow_mut());

    // MUSTTAIL
    return (I.parseInfix())(session, I, token);
}

pub(crate) fn Parser_tryContinue<'i>(
    session: &mut ParserSession<'i>,
    _: ParseletPtr,
    Ignored2: Token,
) {
    if Parser_isContextStackEmpty(session) {
        // no call needed here
        return;
    }

    let ctxt: &mut Context = Parser_topContext(session);

    let F = ctxt.f.expect("Ctxt.f is unexpectedly None");
    let P = ctxt
        .p
        .unwrap_or_else(|| &crate::parselet::PrefixAssertFalseParselet {});

    // MUSTTAIL
    return F(session, P, Ignored2);
}

pub(crate) fn Parser_identity<'i>(_: &mut ParserSession<'i>, _: ParseletPtr, _firstTok: Token) {
    return;
}

pub(crate) fn Parser_eatTrivia<'i>(
    session: &mut ParserSession<'i>,
    token: &mut Token,
    policy: NextPolicy,
) {
    while token.tok.isTrivia() {
        session.NodeStack.push(token.clone().into());

        token.skip(&mut session.tokenizer);

        *token = Tokenizer_currentToken(&mut session.tokenizer, policy);
    }
}

/// [`Parser_eatTrivia`], but the fields of [`ParserSession`] used are
/// transparent to the borrow checker.
pub(crate) fn Parser_eatTrivia_transparent<'i>(
    node_stack: &mut Vec<Node>,
    tokenizer: &mut Tokenizer<'i>,
    token: &mut Token,
    policy: NextPolicy,
) {
    while token.tok.isTrivia() {
        node_stack.push(token.clone().into());

        token.skip(tokenizer);

        *token = Tokenizer_currentToken(tokenizer, policy);
    }
}

pub(crate) fn Parser_eatTrivia_2<'i>(
    session: &mut ParserSession<'i>,
    token: &mut Token,
    policy: NextPolicy,
    Args: &mut TriviaSeq,
) {
    while token.tok.isTrivia() {
        Args.push(token.clone().into());

        token.skip(&mut session.tokenizer);

        *token = Tokenizer_currentToken(&mut session.tokenizer, policy);
    }
}

pub(crate) fn Parser_eatTrivia_stringifyAsFile<'i>(
    session: &mut ParserSession<'i>,
    token: &mut Token,
) {
    while token.tok.isTrivia() {
        session.NodeStack.push(token.clone().into());

        token.skip(&mut session.tokenizer);

        *token = Tokenizer_currentToken_stringifyAsFile(&mut session.tokenizer);
    }
}

pub(crate) fn Parser_eatTriviaButNotToplevelNewlines<'i>(
    session: &mut ParserSession<'i>,
    token: &mut Token,
    policy: NextPolicy,
) {
    while token.tok.isTriviaButNotToplevelNewline() {
        session.NodeStack.push(token.clone().into());

        token.skip(&mut session.tokenizer);

        *token = Tokenizer_currentToken(&mut session.tokenizer, policy);
    }
}

pub(crate) fn Parser_eatTriviaButNotToplevelNewlines_2<'i>(
    session: &mut ParserSession<'i>,
    token: &mut Token,
    policy: NextPolicy,
    Args: &mut TriviaSeq,
) {
    while token.tok.isTriviaButNotToplevelNewline() {
        Args.push(token.clone().into());

        token.skip(&mut session.tokenizer);

        *token = Tokenizer_currentToken(&mut session.tokenizer, policy);
    }
}

pub(crate) fn Parser_pushContext<'i, 's>(
    session: &'s mut ParserSession<'i>,
    prec: Precedence,
) -> &'s mut Context {
    assert!(!session.NodeStack.is_empty());

    session
        .ContextStack
        .push(Context::new(session.NodeStack.len() - 1, prec));

    return session.ContextStack.last_mut().unwrap();
}

/// Push a context, transparently to the borrow checker.
pub(crate) fn Parser_pushContext_transparent<'c>(
    node_stack: &mut Vec<Node>,
    context_stack: &'c mut Vec<Context>,
    prec: Precedence,
) -> &'c mut Context {
    assert!(!node_stack.is_empty());

    context_stack.push(Context::new(node_stack.len() - 1, prec));

    return context_stack.last_mut().unwrap();
}

pub(crate) fn Parser_popContext<'i>(session: &mut ParserSession<'i>) -> NodeSeq {
    assert!(!session.ContextStack.is_empty());

    //
    // get the top Context
    //

    let ctxt = session.ContextStack.pop().unwrap();

    //
    // Move args from back of NodeStack to ArgsTmp
    //

    let ArgsTmp: NodeSeq = {
        // let vec = vec![session.NodeStack.begin() + Ctxt.Index, session.NodeStack.end()];
        let vec = Vec::from_iter(session.NodeStack.drain(ctxt.index..));
        NodeSeq { vec }
    };

    debug_assert_eq!(session.NodeStack.len(), ctxt.index);

    //
    // return ArgsTmp
    //

    return ArgsTmp;
}

fn Parser_isContextStackEmpty<'i>(session: &mut ParserSession<'i>) -> bool {
    return session.ContextStack.is_empty();
}

pub(crate) fn Parser_topContext<'i, 's>(session: &'s mut ParserSession<'i>) -> &'s mut Context {
    assert!(!session.ContextStack.is_empty());

    return session.ContextStack.last_mut().unwrap();
}

pub(crate) fn Parser_topPrecedence<'i>(session: &mut ParserSession<'i>) -> Precedence {
    if session.ContextStack.is_empty() {
        return PRECEDENCE_LOWEST;
    }

    return session.ContextStack.last().unwrap().prec;
}

pub(crate) fn Parser_setPrecedence<'i>(session: &mut ParserSession<'i>, prec: Precedence) {
    assert!(!session.ContextStack.is_empty());

    let ctxt: &mut _ = session.ContextStack.last_mut().unwrap();

    ctxt.prec = prec;
}

pub(crate) fn Parser_pushLeaf<'i>(session: &mut ParserSession<'i>, token: Token) {
    session.NodeStack.push(Node::Token(token));
}

pub(crate) fn Parser_pushLeafAndNext<'i>(session: &mut ParserSession<'i>, token: Token) {
    session.NodeStack.push(Node::Token(token));

    token.skip(&mut session.tokenizer);
}

pub(crate) fn Parser_pushTriviaSeq<'i>(session: &mut ParserSession<'i>, seq: &mut TriviaSeq) {
    //
    // Move all trivia from Seq to back of ArgsStack
    //
    let TriviaSeq { vec } = seq;

    session.NodeStack.extend(vec.drain(0..).map(Node::Token));

    //
    // Forget about Seq
    //

    seq.clear();
}

pub(crate) fn Parser_pushNode<'i, N: Into<Node>>(session: &mut ParserSession<'i>, node: N) {
    let node = node.into();
    session.NodeStack.push(node);
}

pub(crate) fn Parser_popNode<'i>(session: &mut ParserSession<'i>) -> Node {
    assert!(!session.NodeStack.is_empty());

    let top = session.NodeStack.pop().unwrap();

    return top;
}

#[cfg(test)]
pub(crate) fn Parser_topNode<'i, 's>(session: &'s mut ParserSession<'i>) -> &'s mut Node {
    assert!(!session.NodeStack.is_empty());

    return session.NodeStack.last_mut().unwrap();
}

pub(crate) fn Parser_pushGroup<'i>(session: &mut ParserSession<'i>, closer: Closer) {
    session.tokenizer.GroupStack.push(closer);
}

pub(crate) fn Parser_popGroup<'i>(session: &mut ParserSession<'i>) {
    assert!(!session.tokenizer.GroupStack.is_empty());

    session.tokenizer.GroupStack.pop();
}

pub(crate) fn Parser_checkGroup<'i>(session: &mut ParserSession<'i>, closer: Closer) -> bool {
    for value in session.tokenizer.GroupStack.iter().rev() {
        if *value == closer {
            return true;
        }
    }

    return false;
}

pub(crate) fn Parser_checkPatternPrecedence<'i>(session: &mut ParserSession<'i>) -> bool {
    for ctxt in session.ContextStack.iter().rev() {
        let prec = ctxt.prec;

        if prec > PRECEDENCE_FAKE_PATTERNCOLON {
            continue;
        }

        if prec < PRECEDENCE_FAKE_PATTERNCOLON {
            return false;
        }

        assert!(prec == PRECEDENCE_FAKE_PATTERNCOLON);

        return true;
    }

    return false;
}

pub(crate) fn Parser_checkColonLHS<'i>(session: &mut ParserSession<'i>) -> ColonLHS {
    //
    // work backwards, looking for a symbol or something that is a pattern
    //

    //
    // skip any trivia
    //

    let ctxt = session.ContextStack.last().unwrap();

    let mut i: usize = session.NodeStack.len() - 1;
    while i >= ctxt.index {
        if let Node::Token(tok) = session.NodeStack[i] {
            if tok.tok.isTrivia() {
                i -= 1;
                continue;
            }

            break;
        }

        break;
    }

    // if i == ctxt.index.checked_sub(1).expect("subtracted from 0 unsigned number") {
    //     panic!();
    // }

    if Some(i) == ctxt.index.checked_sub(1) {
        panic!();
    }

    match &session.NodeStack[i] {
        Node::Binary(BinaryNode { op }) => {
            //
            // Something like  a:b:c
            //                  ^ Pattern
            //                    ^ Optional
            //

            let op = op.getOp();

            if op == SYMBOL_PATTERN {
                return ColonLHS::Optional;
            }

            return ColonLHS::Error;
        },

        Node::Compound(CompoundNode { op }) => {
            //
            // Something like  a_:b
            //                   ^ Optional
            //

            let op = op.getOp();

            match op {
                SYMBOL_CODEPARSER_PATTERNBLANK
                | SYMBOL_CODEPARSER_PATTERNBLANKSEQUENCE
                | SYMBOL_CODEPARSER_PATTERNBLANKNULLSEQUENCE
                | SYMBOL_BLANK
                | SYMBOL_BLANKSEQUENCE
                | SYMBOL_BLANKNULLSEQUENCE => {
                    return ColonLHS::Optional;
                },
                _ => return ColonLHS::Error,
            }
        },

        Node::Token(tok) => {
            match tok.tok {
                TOKEN_SYMBOL => {
                    //
                    // Something like  a:b
                    //                  ^ Pattern
                    //

                    return ColonLHS::Pattern;
                },
                TOKEN_UNDER | TOKEN_UNDERUNDER | TOKEN_UNDERUNDERUNDER => {
                    //
                    // Something like  _:b
                    //                  ^ Optional
                    //

                    return ColonLHS::Optional;
                },
                TOKEN_COLON => {
                    panic!("Fix at call site")
                },
                _ => (),
            }

            if tok.tok.isError() {
                //
                // allow errors to be on LHS of :
                //
                // This is a bit confusing. The thinking is that since there is already an error, then we do not need to introduce another error.
                //
                return ColonLHS::Pattern;
            }

            return ColonLHS::Error;
        },
        _ => return ColonLHS::Error,
    }
}

pub(crate) fn Parser_checkTilde<'i>(session: &mut ParserSession<'i>) -> bool {
    //
    // work backwards, looking for ~
    //

    if session.ContextStack.is_empty() {
        return false;
    }

    let ctxt = session.ContextStack.last().unwrap();

    let mut i: usize = session.NodeStack.len() - 1;

    //
    // skip past top
    //
    i -= 1;

    //
    // skip any trivia
    //
    while i >= ctxt.index {
        if let Node::Token(tok) = session.NodeStack[i] {
            if tok.tok.isTrivia() {
                i -= 1;
                continue;
            }

            break;
        }

        break;
    }

    // if i == ctxt.index - 1 {
    //     panic!();
    // }

    if Some(i) == ctxt.index.checked_sub(1) {
        panic!();
    }

    if let Node::Token(tok) = session.NodeStack[i] {
        if tok.tok == TOKEN_TILDE {
            return true;
        }
    }

    return false;
}

pub(crate) fn Parser_checkSpan<'i>(session: &mut ParserSession<'i>) -> bool {
    assert!(!session.NodeStack.is_empty());

    let N: &mut Node = session.NodeStack.last_mut().unwrap();

    {
        let NN = N;

        if let Node::Binary(BinaryNode { op: B }) = NN {
            let op = B.getOp();

            if op == SYMBOL_SPAN {
                return true;
            }

            //
            // there is a BinaryNode, but it is not a Span
            //

            return false;
        }

        if let Node::Ternary(TernaryNode { op }) = NN {
            let op = op.getOp();

            if op == SYMBOL_SPAN {
                return true;
            }

            //
            // there is a TernaryNode, but it is not a Span
            //

            return false;
        }
    }

    return false;
}

pub fn Parser_isQuiescent<'i>(session: &mut ParserSession<'i>) -> bool {
    assert!(session.NodeStack.is_empty());
    assert!(session.ContextStack.is_empty());
    assert!(session.tokenizer.GroupStack.is_empty());
    assert!(session.trivia1.borrow().is_empty());
    assert!(session.trivia2.borrow().is_empty());

    return true;
}
