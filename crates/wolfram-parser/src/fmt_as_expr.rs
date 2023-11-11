//! Format parser types as expressions, for printing.

use std::fmt::Display;

use crate::{
    cst::{
        BinaryNode, CallBody, CallHead, CallNode, CompoundNode, Cst,
        GroupMissingCloserNode, GroupMissingOpenerNode, GroupNode, InfixNode,
        OperatorNode, PostfixNode, PrefixBinaryNode, PrefixNode,
        SyntaxErrorNode, TernaryNode,
    },
    parse::operators::{
        BinaryOperator, CompoundOperator, InfixOperator, Operator,
        PostfixOperator, PrefixBinaryOperator, PrefixOperator, TernaryOperator,
    },
    source::{LineColumn, LineColumnSpan, Location, Source, Span, SpanKind},
    symbol::Symbol,
    symbols as sym,
    tokenize::{Token, TokenInput, TokenSource},
    NodeSeq,
};

pub struct FmtAsExpr<T>(pub T);

//==========================================================
// Impls
//==========================================================

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&NodeSeq<Cst<I, S>>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(NodeSeq(nodes)) = self;

        write!(f, "{}", sym::List.as_str())?;
        write!(f, "[")?;

        for node in nodes {
            write!(f, "{}", FmtAsExpr(node))?;
            write!(f, ", ")?;
        }

        write!(f, "]")?;

        Ok(())
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&NodeSeq<Token<I, S>>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(NodeSeq(tokens)) = self;

        write!(f, "{}", sym::List.as_str())?;
        write!(f, "[")?;

        for token in tokens {
            write!(f, "{}", FmtAsExpr(token))?;
            write!(f, ", ")?;
        }

        write!(f, "]")?;

        Ok(())
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&Token<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(Token { tok, src, input }) = self;

        let head = if tok.isError() {
            sym::CodeParser_ErrorNode
        } else {
            sym::CodeParser_LeafNode
        };

        write!(f, "{}[{}, ", head.as_str(), tok.to_symbol().as_str())?;



        let source = std::str::from_utf8(input.as_bytes())
            .expect("token source span is not valid UTF-8");

        // FIXME: This should escape internal characters correctly.
        write!(f, "\"{source}\"")?;

        write!(f, ", {}]", FmtAsExpr(src))?;

        Ok(())
    }
}


impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&Cst<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(node) = self;

        match node {
            Cst::Token(token) => write!(f, "{}", FmtAsExpr(token))?,
            Cst::Call(node) => write!(f, "{}", FmtAsExpr(node))?,
            Cst::SyntaxError(node) => write!(f, "{}", FmtAsExpr(node))?,
            Cst::Prefix(PrefixNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            Cst::Infix(InfixNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            Cst::Postfix(PostfixNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            Cst::Binary(BinaryNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            Cst::Ternary(TernaryNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            Cst::PrefixBinary(PrefixBinaryNode(op)) => {
                write!(f, "{}", FmtAsExpr(op))?
            },
            Cst::Compound(CompoundNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            Cst::Group(node) => write!(f, "{}", FmtAsExpr(node))?,
            Cst::GroupMissingCloser(node) => write!(f, "{}", FmtAsExpr(node))?,
            Cst::GroupMissingOpener(node) => write!(f, "{}", FmtAsExpr(node))?,
            Cst::Box(_) => todo!(),
            Cst::Code(_) => todo!(),
        }

        Ok(())
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&CallNode<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(CallNode { head, body }) = self;

        write!(f, "{}", sym::CodeParser_CallNode.as_str())?;
        write!(f, "[")?;

        write!(f, "{}", FmtAsExpr(head))?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(body))?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(&self.0.get_source()))?;

        write!(f, "]")?;

        Ok(())
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&CallHead<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(call_body) = self;

        match call_body {
            CallHead::Concrete(head) => write!(f, "{}", FmtAsExpr(head)),
            CallHead::Aggregate(head) => write!(f, "{}", FmtAsExpr(&**head)),
        }
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&CallBody<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(call_body) = self;

        match call_body {
            CallBody::Group(group) => write!(f, "{}", FmtAsExpr(group)),
            CallBody::GroupMissingCloser(group) => {
                write!(f, "{}", FmtAsExpr(group))
            },
        }
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&SyntaxErrorNode<I, S>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(node @ SyntaxErrorNode { err, children }) = self;

        write!(f, "{}", sym::CodeParser_SyntaxErrorNode.as_str())?;
        write!(f, "[")?;

        write!(f, "{}", err.to_symbol().as_str())?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(children))?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(&node.get_source()))?;

        write!(f, "]")?;

        Ok(())
    }
}
//--------------------------------------
// Operator node variants
//--------------------------------------

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&OperatorNode<I, S, PrefixOperator>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_PrefixNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&OperatorNode<I, S, InfixOperator>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_InfixNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&OperatorNode<I, S, PostfixOperator>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_PostfixNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&OperatorNode<I, S, BinaryOperator>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_BinaryNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&OperatorNode<I, S, TernaryOperator>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_TernaryNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&OperatorNode<I, S, PrefixBinaryOperator>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_PrefixBinaryNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&OperatorNode<I, S, CompoundOperator>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_CompoundNode)
    }
}

impl<I: TokenInput, S: TokenSource, O: Operator> Display
    for FmtAsExpr<&GroupNode<I, S, O>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(GroupNode(op)) = self;

        put_op(f, op, sym::CodeParser_GroupNode)
    }
}

impl<I: TokenInput, S: TokenSource, O: Operator> Display
    for FmtAsExpr<&GroupMissingCloserNode<I, S, O>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(GroupMissingCloserNode(op)) = self;

        put_op(f, op, sym::CodeParser_GroupMissingCloserNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display
    for FmtAsExpr<&GroupMissingOpenerNode<I, S>>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(GroupMissingOpenerNode(op)) = self;

        put_op(f, op, sym::CodeParser_GroupMissingOpenerNode)
    }
}

fn put_op<I: TokenInput, S: TokenSource, O: Operator>(
    f: &mut std::fmt::Formatter,
    node: &OperatorNode<I, S, O>,
    op_head: Symbol,
) -> std::fmt::Result {
    let src = node.get_source();

    let OperatorNode { op, children } = node;

    write!(f, "{}", op_head.as_str())?;

    write!(f, "[")?;

    write!(f, "{}", op.to_symbol().as_str())?;
    write!(f, ", ")?;

    write!(f, "{}", FmtAsExpr(children))?;
    write!(f, ", ")?;

    write!(f, "{}", FmtAsExpr(&src))?;

    write!(f, "]")?;

    Ok(())
}

impl<S: TokenSource> Display for FmtAsExpr<&S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(source) = self;

        let source: S = (*source).clone();
        let source: Source = source.into_general();

        match source {
            Source::Span(span) => write!(f, "{}", FmtAsExpr(span)),
            Source::Box(_) => todo!(),
            Source::Unknown => todo!(),
        }
    }
}

impl Display for FmtAsExpr<Span> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(source) = self;

        match source.kind() {
            SpanKind::LineColumnSpan(LineColumnSpan { start, end }) => {
                write!(
                    f,
                    "{{{}, {}}}",
                    FmtAsExpr(&Location::from(start)),
                    FmtAsExpr(&Location::from(end))
                )
            },
            SpanKind::CharacterSpan(_) => todo!(),
        }
    }
}

impl Display for FmtAsExpr<&Location> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(src_loc) = self;

        match src_loc {
            Location::LineColumn(LineColumn(line, column)) => {
                write!(f, "{{{line}, {column}}}")
            },
            Location::CharacterIndex(index) => write!(f, "{index}"),
        }
    }
}
