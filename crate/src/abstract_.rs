use std::fmt::Debug;

use crate::{
    node::{
        BinaryNode, BoxNode, CallNode, CompoundNode, GroupMissingCloserNode, GroupNode, InfixNode,
        Node, NodeSeq, Operator, OperatorNode, PostfixNode, PrefixBinaryNode, PrefixNode,
        SyntaxErrorNode, TernaryNode, UnterminatedGroupNode,
    },
    token::{Token, TokenKind},
};

//==========================================================
// Aggregate
//==========================================================

pub(crate) fn Aggregate<I: Debug, S: Debug>(agg: NodeSeq<I, S>) -> NodeSeq<I, S> {
    let NodeSeq(agg) = agg;

    let agg_children = agg.into_iter().flat_map(aggregate_replace).collect();

    NodeSeq(agg_children)
}

fn aggregate_replace<I: Debug, S: Debug>(node: Node<I, S>) -> Option<Node<I, S>> {
    let node: Node<_, _> = match node {
        // Remove comments, whitespace, and newlines
        Node::Token(Token {
            tok:
                TokenKind::Comment
                | TokenKind::InternalNewline
                | TokenKind::ToplevelNewline
                | TokenKind::Boxes_MultiWhitespace
                | TokenKind::Whitespace,
            ..
        }) => return None,
        Node::Token(_) => return Some(node),
        // Remove comments.
        Node::Group(GroupNode(OperatorNode {
            op: Operator::Token_Comment,
            ..
        })) => return None,

        // Multiple implicit Times tokens may have been inserted when parsing boxes, so remove them here
        Node::Infix(InfixNode(OperatorNode {
            op: Operator::Times,
            children: NodeSeq(children),
            src,
        })) => {
            let aggregated_children: Vec<_> =
                children.into_iter().flat_map(aggregate_replace).collect();

            // FIXME: Translate this line
            //aggregatedChildren = First /@ Split[aggregatedChildren, (MatchQ[#1, LeafNode[Token`Fake`ImplicitTimes, _, _]] && MatchQ[#2, LeafNode[Token`Fake`ImplicitTimes, _, _]])&];

            Node::Infix(InfixNode(OperatorNode {
                op: Operator::Times,
                children: NodeSeq(aggregated_children),
                src,
            }))
        },

        Node::Call(CallNode {
            head,
            body,
            src,
            is_concrete,
        }) => {
            let head = Aggregate(head);
            let body = aggregate_replace(*body)
                .expect("expected CallNode body to aggregate to a real value");

            debug_assert!(is_concrete);

            Node::Call(CallNode {
                head,
                body: Box::new(body),
                src,
                is_concrete: false,
            })
        },

        // Do not descend into CodeNode
        //    aggregate[n:CodeNode[_, _, _]] := n
        Node::Code(node) => Node::Code(node),

        //---------------------------------------------
        //  aggregate[node_[tag_, children_, data_]] :=
        //      node[tag, aggregate /@ children, data]
        //---------------------------------------------
        Node::SyntaxError(SyntaxErrorNode { err, children, src }) => {
            Node::SyntaxError(SyntaxErrorNode {
                err,
                children: Aggregate(children),
                src,
            })
        },
        Node::Group(GroupNode(op)) => Node::Group(GroupNode(aggregate_op(op))),
        Node::UnterminatedGroup(UnterminatedGroupNode(op)) => {
            Node::UnterminatedGroup(UnterminatedGroupNode(aggregate_op(op)))
        },
        Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
            Node::GroupMissingCloser(GroupMissingCloserNode(aggregate_op(op)))
        },
        Node::Box(BoxNode {
            kind,
            children,
            src,
        }) => Node::Box(BoxNode {
            kind,
            children: Aggregate(children),
            src,
        }),

        Node::Infix(InfixNode(op)) => Node::Infix(InfixNode(aggregate_op(op))),
        Node::Prefix(PrefixNode(op)) => Node::Prefix(PrefixNode(aggregate_op(op))),
        Node::Postfix(PostfixNode(op)) => Node::Postfix(PostfixNode(aggregate_op(op))),
        Node::Binary(BinaryNode(op)) => Node::Binary(BinaryNode(aggregate_op(op))),
        Node::Ternary(TernaryNode(op)) => Node::Ternary(TernaryNode(aggregate_op(op))),
        Node::PrefixBinary(PrefixBinaryNode(op)) => {
            Node::PrefixBinary(PrefixBinaryNode(aggregate_op(op)))
        },
        Node::Compound(CompoundNode(op)) => Node::Compound(CompoundNode(aggregate_op(op))),
    };

    /* FIXME: Port these:
        (*
            BoxNode[RowBox] and BoxNode[GridBox] have lists as children
        *)
        aggregate[l_List] := aggregate /@ l

        aggregate[BoxNode[RowBox, childrenIn_, dataIn_]] :=
            Catch[
                Module[{children, aggChildren, data},
                    children = childrenIn;
                    data = dataIn;

                    aggChildren = aggregate /@ children;

                    If[MatchQ[aggChildren, {{_}}],
                        (*
                        children is now a single node, so collapse the RowBox
                        *)
                        Throw[aggChildren[[1, 1]]]
                    ];

                    BoxNode[RowBox, aggChildren, data]
                ]
            ]
    */

    Some(node)
}

fn aggregate_op<I: Debug, S: Debug>(op: OperatorNode<I, S>) -> OperatorNode<I, S> {
    let OperatorNode { op, children, src } = op;

    OperatorNode {
        op,
        children: Aggregate(children),
        src,
    }
}

//==========================================================
// Abstract
//==========================================================

pub(crate) fn Abstract<I>(_cst: NodeSeq<I>) -> NodeSeq<I> {
    todo!()
}
