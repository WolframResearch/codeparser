use crate::{cst::Cst, source::Span, tokenize::TokenString, NodeSeq};

pub type AggNodeSeq<I = TokenString, S = Span> = NodeSeq<Cst<I, S>>;

//==========================================================
// Macros
//==========================================================

//======================================
// LHS!
//======================================

// Note: Don't make this public outside of this crate until `AggCallNode` is
//       made part of Node. And updating it to use $crate for types.
macro_rules! LHS {
    (LeafNode[$($head_kind:ident)|*, _, _]) => {
        $crate::cst::Cst::Token(Token {
            tok: $(TK::$head_kind)|*,
            ..
        })
    };

    //==================================
    // CallNode
    //==================================

    (CallNode[LeafNode[$token_kind:ident, _, _], $children:ident:_, _]) => {
        AggCallNode {
            head: Node::Token(Token {
                tok: TK::$token_kind,
                ..
            }),
            children: $children,
        }
    };

    (CallNode[
        $head_name:ident:$node_head:ident[$($node_args:tt)*],
        $group_name:ident:$group_head:ident[$group_kind:ident, _],
        $data:ident:_
    ]) => {
        AggCallNode {
            head: $head_name @ LHS!($node_head[$($node_args)*]),
            body: LHS!($group_name : $group_head[$group_kind, _]),
            src: $data,
        }
    };
    (CallNode[
        $head_name:ident:_,
        $group_name:ident:$group_head:ident[$($group_kind:ident)|*, _],
        $data:ident:_
    ]) => {
        AggCallNode {
            head: $head_name,
            body: LHS!($group_name:$group_head[$($group_kind)|*, _]),
            src: $data
        }
    };
    (CallNode[
        $head_name:ident:($($sub_head_pat:ident[$($sub_head_args:tt)*])|*),
        $group_name:ident:GroupNode[$group_kind:ident, _],
        $data:ident:_
    ]) => {
        AggCallNode {
            head: $head_name @ ($(LHS!($sub_head_pat[$($sub_head_args)*]))|*),
            body: LHS!($group_name:GroupNode[$group_kind, _]),
            src: $data
        }
    };
    (CallNode[_, _, _]) => {
        Cst::Call(_)
    };

    //==================================
    // CompoundNode, BinaryNode, InfixNode, PrefixNode
    //==================================

    (CompoundNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Compound(CompoundNode(OperatorNode {
            op: $(crate::parse::operators::CompoundOperator::$op_kind)|*,
            ..
        }))
    };

    (BinaryNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Binary(BinaryNode(OperatorNode {
            op: $($crate::parse::operators::BinaryOperator::$op_kind)|*,
            ..
        }))
    };

    (InfixNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Infix(InfixNode(OperatorNode {
            op: $($crate::parse::operators::InfixOperator::$op_kind)|*,
            ..
        }))
    };
    (PrefixNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Prefix(PrefixNode(OperatorNode {
            op: $($crate::parse::operators::PrefixOperator::$op_kind)|*,
            ..
        }))
    };

    (PostfixNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Postfix(PostfixNode(OperatorNode {
            op: $(crate::parse::operators::PostfixOperator::$op_kind)|*,
            ..
        }))
    };

    //==================================
    // GroupNode
    //==================================

    (GroupNode[$($op_kind:ident)|*, $children:ident:_]) => {
        $crate::cst::Cst::Group(GroupNode(OperatorNode {
            op: $(GroupOperator::$op_kind)|*,
            children: $children,
        }))
    };

    (GroupNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Group(GroupNode(OperatorNode {
            op: $(GroupOperator::$op_kind)|*,
            ..
        }))
    };
    ($name:ident:GroupNode[$group_kind:ident, _]) => {
        CallBody::Group($name @ GroupNode(OperatorNode {
            op: $crate::parse::operators::CallOperator::$group_kind,
            children: _,
        }))
    };

    (GroupNode[_, _, _]) => {
        Cst::Group(GroupNode(OperatorNode {
            op: _,
            ..
        }))
    };

    //----------------------------------
    // GroupMissingCloserNode
    //----------------------------------

    ($name:ident:GroupMissingCloserNode[$($op_kind:ident)|*, _]) => {
        $crate::cst::CallBody::GroupMissingCloser($name @ $crate::cst::GroupMissingCloserNode(OperatorNode {
            op: $($crate::parse::operators::CallOperator::$op_kind)|*,
            ..
        }))
    };

    //==================================
    // BoxNode
    //==================================

    (BoxNode[$box_kind:ident:_, _, _]) => {
        Cst::Box(BoxNode {
            kind: $box_kind,
            ..
        })
    };
    (BoxNode[$box_kind:ident:_, $children:ident:_, $data:ident:_]) => {
        Cst::Box(BoxNode {
            kind: $box_kind,
            children: $children,
            src: $data,
        })
    };
    (BoxNode[$box_kind:ident, $children:ident:_, $data:ident:_]) => {
        $crate::cst::Cst::Box(BoxNode {
            kind: BoxKind::$box_kind,
            children: $children,
            src: $data,
        })
    };
    (BoxNode[_, _, _]) => {
        Cst::Box(_)
    };
}


pub(crate) use LHS;
