use crate::{cst::Cst, source::Span, tokenize::OwnedTokenInput, NodeSeq};

pub type AggNodeSeq<I = OwnedTokenInput, S = Span> = NodeSeq<Cst<I, S>>;

//==========================================================
// Macros
//==========================================================

//======================================
// WL!
//======================================

macro_rules! WL {
    //========================
    // ToNode[..]
    //========================

    (ToNode[$sym:ident]) => {{
        // Sanity check that `$sym` is a System` symbol.
        {
            let _: $crate::symbol::Symbol = $crate::symbols::$sym;
        }

        let sym: &'static str = stringify!($sym);
        // FIXME(optimization): make debug only
        assert!(
            sym.chars().next().unwrap().is_ascii_uppercase(),
            "suspicious ToNode[{sym}] will create symbol with source {sym:?}. "
        );

        $crate::agg::WL!( LeafNode[Symbol, sym, <||>] )
    }};

    (ToNode[-1]) => {
        $crate::cst::Cst::Token($crate::tokenize::Token {
            tok: $crate::tokenize::TokenKind::Integer,
            input: I::fake("-1"),
            src: S::unknown(),
        })
    }; (ToNode[1]) => { $crate::cst::Cst::Token($crate::tokenize::Token {
            tok: $crate::tokenize::TokenKind::Integer,
            input: I::fake("1"),
            src: S::unknown(),
        })
    };

    //========================
    // LeafNode
    //========================

    (LeafNode[$token_kind:ident, $input:literal, <||>]) => {{
        let input: &'static str = $input;

        $crate::cst::Cst::Token($crate::tokenize::Token {
            tok: $crate::tokenize::TokenKind::$token_kind,
            input: I::fake(input),
            src: S::unknown(),
        })
    }};
    (LeafNode[$token_kind:ident, $input:expr, <||>]) => {{
        let input: &'static str = $input;
        $crate::cst::Cst::Token($crate::tokenize::Token {
            tok: $crate::tokenize::TokenKind::$token_kind,
            input: I::fake(input),
            src: S::unknown(),
        })
    }};
    (LeafNode[$token_kind:ident, $input:literal, $data:expr]) => {{
        let input: &str = $input;
        $crate::cst::Cst::Token(Token {
            tok: $crate::tokenize::TokenKind::$token_kind,
            input: I::fake(input),
            src: $data,
        })
    }};

    (LeafNode[$token_kind:ident, $input:expr, $data:expr]) => {{
        let input: String = String::from($input);

        $crate::cst::Cst::Token(Token {
            tok: $crate::tokenize::TokenKind::$token_kind,
            input: $crate::tokenize::OwnedTokenInput {
                buf: input.into_bytes(),
            },
            src: $data,
        })
    }};

    //========================
    // InfixNode
    //========================

    (InfixNode[$op:ident, { $($args:expr),*}, <||>]) => {
        $crate::cst::Cst::Infix(
            $crate::cst::InfixNode(
                $crate::cst::OperatorNode {
                    op: $crate::cst::InfixOperator::$op,
                    children: $crate::NodeSeq(vec![$($args),*]),
                    src: S::unknown()
                }
            )
        )
    };
}

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
        $group_name:ident:$group_head:ident[$group_kind:ident, _, _],
        $data:ident:_
    ]) => {
        AggCallNode {
            head: $head_name @ LHS!($node_head[$($node_args)*]),
            body: LHS!($group_name : $group_head[$group_kind, _, _]),
            src: $data,
        }
    };
    (CallNode[
        $head_name:ident:_,
        $group_name:ident:$group_head:ident[$($group_kind:ident)|*, _, _],
        $data:ident:_
    ]) => {
        AggCallNode {
            head: $head_name,
            body: LHS!($group_name:$group_head[$($group_kind)|*, _, _]),
            src: $data
        }
    };
    (CallNode[
        $head_name:ident:($($sub_head_pat:ident[$($sub_head_args:tt)*])|*),
        $group_name:ident:GroupNode[$group_kind:ident, _, _],
        $data:ident:_
    ]) => {
        AggCallNode {
            head: $head_name @ ($(LHS!($sub_head_pat[$($sub_head_args)*]))|*),
            body: LHS!($group_name:GroupNode[$group_kind, _, _]),
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
            op: $(crate::cst::CompoundOperator::$op_kind)|*,
            ..
        }))
    };

    (BinaryNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Binary(BinaryNode(OperatorNode {
            op: $($crate::cst::BinaryOperator::$op_kind)|*,
            ..
        }))
    };

    (InfixNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Infix(InfixNode(OperatorNode {
            op: $($crate::cst::InfixOperator::$op_kind)|*,
            ..
        }))
    };
    (PrefixNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Prefix(PrefixNode(OperatorNode {
            op: $($crate::cst::PrefixOperator::$op_kind)|*,
            ..
        }))
    };

    (PostfixNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Postfix(PostfixNode(OperatorNode {
            op: $(crate::cst::PostfixOperator::$op_kind)|*,
            ..
        }))
    };

    //==================================
    // GroupNode
    //==================================

    (GroupNode[$($op_kind:ident)|*, $children:ident:_, $data:ident:_]) => {
        $crate::cst::Cst::Group(GroupNode(OperatorNode {
            op: $(GroupOperator::$op_kind)|*,
            children: $children,
            src: $data,
        }))
    };

    (GroupNode[$($op_kind:ident)|*, _, _]) => {
        Cst::Group(GroupNode(OperatorNode {
            op: $(GroupOperator::$op_kind)|*,
            ..
        }))
    };
    ($name:ident:GroupNode[$group_kind:ident, _, _]) => {
        CallBody::Group($name @ GroupNode(OperatorNode {
            op: $crate::cst::CallOperator::$group_kind,
            children: _,
            src: _,
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

    ($name:ident:GroupMissingCloserNode[$($op_kind:ident)|*, _, _]) => {
        $crate::cst::CallBody::GroupMissingCloser($name @ $crate::cst::GroupMissingCloserNode(OperatorNode {
            op: $($crate::cst::CallOperator::$op_kind)|*,
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


pub(crate) use {LHS, WL};
