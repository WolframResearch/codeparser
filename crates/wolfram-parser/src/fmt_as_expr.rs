//! Format parser types as expressions, for printing.

use std::fmt::Display;

use crate::{
    cst::{
        BinaryNode, BinaryOperator, CallBody, CallHead, CallNode, CompoundNode, CompoundOperator,
        CstNode, GroupMissingCloserNode, GroupMissingOpenerNode, GroupNode, InfixNode,
        InfixOperator, Operator, OperatorNode, PostfixNode, PostfixOperator, PrefixBinaryNode,
        PrefixBinaryOperator, PrefixNode, PrefixOperator, SyntaxErrorNode, TernaryNode,
        TernaryOperator,
    },
    source::{GeneralSource, LineColumn, LineColumnSpan},
    symbol::Symbol,
    symbol_registration as sym,
    token::{Token, TokenInput, TokenSource},
    token_enum_registration::TokenToSymbol,
    NodeSeq, Source, SourceLocation, StringSourceKind, Tokens,
};

pub struct FmtAsExpr<T>(pub T);

//==========================================================
// Impls
//==========================================================

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&NodeSeq<CstNode<I, S>>> {
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

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&NodeSeq<Token<I, S>>> {
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

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&Tokens<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(Tokens(tokens)) = self;

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

        write!(f, "{}[{}, ", head.as_str(), TokenToSymbol(*tok).as_str())?;



        let source =
            std::str::from_utf8(input.as_bytes()).expect("token source span is not valid UTF-8");

        // FIXME: This should escape internal characters correctly.
        write!(f, "\"{source}\"")?;

        write!(f, ", {}]", FmtAsExpr(src))?;

        Ok(())
    }
}


impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&CstNode<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(node) = self;

        match node {
            CstNode::Token(token) => write!(f, "{}", FmtAsExpr(token))?,
            CstNode::Call(node) => write!(f, "{}", FmtAsExpr(node))?,
            CstNode::SyntaxError(node) => write!(f, "{}", FmtAsExpr(node))?,
            CstNode::Prefix(PrefixNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            CstNode::Infix(InfixNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            CstNode::Postfix(PostfixNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            CstNode::Binary(BinaryNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            CstNode::Ternary(TernaryNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            CstNode::PrefixBinary(PrefixBinaryNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            CstNode::Compound(CompoundNode(op)) => write!(f, "{}", FmtAsExpr(op))?,
            CstNode::Group(node) => write!(f, "{}", FmtAsExpr(node))?,
            CstNode::GroupMissingCloser(node) => write!(f, "{}", FmtAsExpr(node))?,
            CstNode::GroupMissingOpener(node) => write!(f, "{}", FmtAsExpr(node))?,
            CstNode::Box(_) => todo!(),
            CstNode::Code(_) => todo!(),
        }

        Ok(())
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&CallNode<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(CallNode { head, body, src }) = self;

        write!(f, "{}", sym::CodeParser_CallNode.as_str())?;
        write!(f, "[")?;

        write!(f, "{}", FmtAsExpr(head))?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(body))?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(src))?;

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
            CallBody::GroupMissingCloser(group) => write!(f, "{}", FmtAsExpr(group)),
        }
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&SyntaxErrorNode<I, S>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(SyntaxErrorNode { err, children, src }) = self;

        write!(f, "{}", sym::CodeParser_SyntaxErrorNode.as_str())?;
        write!(f, "[")?;

        write!(f, "{}", err.to_symbol().as_str())?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(children))?;
        write!(f, ", ")?;

        write!(f, "{}", FmtAsExpr(src))?;

        write!(f, "]")?;

        Ok(())
    }
}
//--------------------------------------
// Operator node variants
//--------------------------------------

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&OperatorNode<I, S, PrefixOperator>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_PrefixNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&OperatorNode<I, S, InfixOperator>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_InfixNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&OperatorNode<I, S, PostfixOperator>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_PostfixNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&OperatorNode<I, S, BinaryOperator>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_BinaryNode)
    }
}

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&OperatorNode<I, S, TernaryOperator>> {
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

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&OperatorNode<I, S, CompoundOperator>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(op) = self;

        put_op(f, op, sym::CodeParser_CompoundNode)
    }
}

impl<I: TokenInput, S: TokenSource, O: Operator> Display for FmtAsExpr<&GroupNode<I, S, O>> {
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

impl<I: TokenInput, S: TokenSource> Display for FmtAsExpr<&GroupMissingOpenerNode<I, S>> {
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
    let OperatorNode { op, children, src } = node;

    write!(f, "{}", op_head.as_str())?;

    write!(f, "[")?;

    write!(f, "{}", op.to_symbol().as_str())?;
    write!(f, ", ")?;

    write!(f, "{}", FmtAsExpr(children))?;
    write!(f, ", ")?;

    write!(f, "{}", FmtAsExpr(src))?;

    write!(f, "]")?;

    Ok(())
}

impl<S: TokenSource> Display for FmtAsExpr<&S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(source) = self;

        let source: S = (*source).clone();
        let source: GeneralSource = source.into_general();

        match source {
            GeneralSource::String(source) => write!(f, "{}", FmtAsExpr(source)),
            GeneralSource::BoxPosition(_) => todo!(),
            GeneralSource::After(_) => todo!(),
        }
    }
}

impl Display for FmtAsExpr<Source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(source) = self;

        match source.kind() {
            StringSourceKind::LineColumnSpan(LineColumnSpan { start, end }) => {
                write!(
                    f,
                    "{{{}, {}}}",
                    FmtAsExpr(&SourceLocation::from(start)),
                    FmtAsExpr(&SourceLocation::from(end))
                )
            },
            StringSourceKind::CharacterSpan(_) => todo!(),
            StringSourceKind::Unknown => todo!(),
        }
    }
}

impl Display for FmtAsExpr<&SourceLocation> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let FmtAsExpr(src_loc) = self;

        match src_loc {
            SourceLocation::LineColumn(LineColumn(line, column)) => {
                write!(f, "{{{line}, {column}}}")
            },
            SourceLocation::CharacterIndex(index) => write!(f, "{index}"),
        }
    }
}