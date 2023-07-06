mod abstract_call_node;

use std::fmt::Debug;

use crate::{
    agg::{self, AggNodeSeq, LHS},
    ast::{AstCall, AstMetadata, AstNode, WL},
    cst::CstNodeSeq,
    issue::{Issue, IssueTag, Severity},
    node::{
        BinaryNode, BoxKind, BoxNode, CallBody, CallNode, CodeNode, CompoundNode,
        GroupMissingCloserNode, GroupMissingOpenerNode, GroupNode, InfixNode, Node, NodeSeq,
        Operator::{self, self as Op},
        OperatorNode, PostfixNode, PrefixBinaryNode, PrefixNode, SyntaxErrorKind, SyntaxErrorNode,
        TernaryNode,
    },
    quirks::{self, processInfixBinaryAtQuirk, Quirk},
    source::GeneralSource,
    symbol::{self as sym, Symbol},
    token::{
        OwnedTokenInput, Token, TokenInput,
        TokenKind::{self, self as TK},
        TokenSource,
    },
    Source,
};

//==========================================================
// Aggregate
//==========================================================

pub fn Aggregate<I: Debug, S: Debug>(agg: CstNodeSeq<I, S>) -> AggNodeSeq<I, S> {
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
            let body = body.map_op(aggregate_op);

            debug_assert!(is_concrete);

            Node::Call(CallNode {
                head,
                body,
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
        Node::GroupMissingCloser(GroupMissingCloserNode(op)) => {
            Node::GroupMissingCloser(GroupMissingCloserNode(aggregate_op(op)))
        },
        Node::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
            Node::GroupMissingOpener(GroupMissingOpenerNode(aggregate_op(op)))
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

//--------------------------------------
// WL Syntax Macros
//--------------------------------------

/// Returns a `LeafNode[Symbol, ..]`
fn ToNode_Op(op: Operator) -> AstNode {
    let s: wolfram_expr::symbol::SymbolRef = op.to_symbol();
    ToNode_Symbol(s)
}

fn ToNode_Symbol(s: Symbol) -> AstNode {
    // TODO(optimization): We only have to convert this to an allocated Symbol
    //                     because SymbolRef doesn't currently have context()
    //                     and symbol_name() methods. Add those methods to
    //                     SymbolRef in the wolfram-expr crate, and update this
    //                     to avoid the allocation.
    let s: wolfram_expr::Symbol = s.to_symbol();

    if s.context().as_str() == "System`" {
        WL!( LeafNode[Symbol, s.symbol_name().as_str(), <||>] )
    } else {
        // Play it safe for now and fully qualify any non-System` symbol
        WL!( LeafNode[Symbol, s.as_str(), <||>])
    }
}

/// Returns a `LeafNode[String, ..]`
#[allow(dead_code)]
fn ToNode_String(s: &str) -> AstNode {
    // FIXME: In the WL source this was escapeString(s);
    WL!( LeafNode[String, s, <||>])
}

/// Returns a `LeafNode[Integer, ..]`
// ToNode[i_Integer] := LeafNode[Integer, ToString[i], <||>]
fn ToNode_Integer(int: i64) -> AstNode {
    WL!( LeafNode[Integer, int.to_string(), <||>] )
}

fn ToNode_Integer_usize(int: usize) -> AstNode {
    WL!( LeafNode[Integer, int.to_string(), <||>] )
}

//======================================

macro_rules! expect_children {
    ($children:ident, {_, $name:ident:_}) => {
        let [_, $name] = expect_children($children);
    };
    ($children:ident, {$name1:ident:_, $name2:ident:_}) => {
        let [$name1, $name2] = expect_children($children);
    };
    ($children:ident, {_, $name1:ident:_, _}) => {
        let [_, $name1, _] = expect_children($children);
    };
    ($children:ident, {$name1:ident, _, $name2:ident:_}) => {
        let [$name1, _, $name2] = expect_children($children);
    };

    ($children:ident, {$name:ident:_, LeafNode[$token_kind:ident, _, $data:ident:_]}) => {
        let [$name, leaf] = expect_children($children);

        let $data = match leaf {
            Node::Token(Token {
                tok: TokenKind::$token_kind,
                input: _,
                src,
            }) => src,
            _ => unhandled(),
        };
    };
    ($children:ident, {_, $name:ident:LeafNode[$token_kind:ident, _, _]}) => {
        let [_, $name] = expect_children($children);

        if !matches!(
            $name,
            Node::Token(Token {
                tok: TokenKind::$token_kind,
                ..
            })
        ) {
            unhandled()
        }
    };
}

//--------------------------------------
// Functions
//--------------------------------------

pub(crate) fn Abstract<I: TokenInput + Debug, S: TokenSource + Debug>(
    agg: AggNodeSeq<I, S>,
) -> Vec<AstNode> {
    let NodeSeq(agg) = agg;

    let ast_children = agg.into_iter().map(abstract_).collect();

    ast_children
}

// TODO(cleanup): Make this private again. Abstract(..) is the crate-public
//                interface.
pub fn abstract_<I: TokenInput + Debug, S: TokenSource + Debug>(node: Node<I, S>) -> AstNode {
    match node {
        Node::Token(token) => return abstract_replace_token(token),
        Node::Compound(CompoundNode(OperatorNode {
            op,
            children,
            src: data,
        })) => match op {
            // CompoundNode[Blank, {_, sym2_}, data_]
            Operator::Blank => {
                expect_children!(children, {_, sym2:_});
                WL!( CallNode[ToNode[Blank], {abstract_(sym2)}, data] )
            },
            // CompoundNode[BlankSequence, {_, sym2_}, data_]
            Operator::BlankSequence => {
                expect_children!(children, {_, sym2:_});
                WL!( CallNode[ToNode[BlankSequence], {abstract_(sym2)}, data] )
            },
            // CompoundNode[BlankNullSequence, {_, sym2_}, data_]
            Operator::BlankNullSequence => {
                expect_children!(children, {_, sym2:_});
                WL!( CallNode[ToNode[BlankNullSequence], {abstract_(sym2)}, data] )
            },

            // CompoundNode[PatternBlank, {sym1_, blank_}, data_]
            Operator::CodeParser_PatternBlank => {
                expect_children!(children, {sym1:_, blank:_});
                WL!( CallNode[ToNode[Pattern], {abstract_(sym1), abstract_(blank)}, data] )
            },
            // CompoundNode[PatternBlankSequence,     {sym1_, blankSeq_}, data_]
            Operator::CodeParser_PatternBlankSequence => {
                expect_children!(children, {sym1:_, blankSeq:_});
                WL!( CallNode[ToNode[Pattern], {abstract_(sym1), abstract_(blankSeq)}, data] )
            },
            // CompoundNode[PatternBlankNullSequence, {sym1_, blankNullSeq_}, data_]
            Operator::CodeParser_PatternBlankNullSequence => {
                expect_children!(children, {sym1:_, blankNullSeq:_});
                WL!( CallNode[ToNode[Pattern], {abstract_(sym1), abstract_(blankNullSeq)}, data] )
            },
            // CompoundNode[PatternOptionalDefault, {sym1_, LeafNode[Token`UnderDot, _, optionalDefaultData_]}, data_]
            Operator::CodeParser_PatternOptionalDefault => {
                expect_children!(children, {sym1:_, LeafNode[UnderDot, _, optionalDefaultData:_]});

                WL!(
                    CallNode[
                        ToNode[Optional],
                        {
                            WL!(CallNode[
                                ToNode[Pattern],
                                {abstract_(sym1), WL!(CallNode[ToNode[Blank], {}, optionalDefaultData])},
                                data.clone()
                            ])
                        },
                        data
                    ]
                )
            },

            // CompoundNode[Slot, {_, arg:LeafNode[Integer, _, data1_]}, data_]
            // CompoundNode[Slot, {_, arg:LeafNode[Symbol, s_, data1_]}, data_]
            // CompoundNode[Slot, {_, arg:LeafNode[String, s_, data1_]}, data_]
            Operator::Slot => {
                let [_, arg] = expect_children(children);

                match arg {
                    Node::Token(arg) => match arg.tok {
                        TokenKind::Integer => {
                            WL!( CallNode[ToNode[Slot], {abstract_(Node::Token(arg))}, data] )
                        },
                        TokenKind::Symbol => {
                            let Token {
                                tok: _,
                                input: s,
                                src: data1,
                            } = arg;
                            WL!(CallNode[
                                ToNode[Slot],
                                {WL!( LeafNode[String, escapeString_of_abstractSymbolString(s.as_str()), data1])},
                                data
                            ])
                        },
                        TokenKind::String => {
                            let Token {
                                tok: _,
                                input: s,
                                src: data1,
                            } = arg;
                            WL!(CallNode[
                                ToNode[Slot],
                                {WL!( LeafNode[String, escapeString_of_abstractSymbolString(s.as_str()), data1] )},
                                data
                            ])
                        },
                        _ => unhandled(),
                    },
                    _ => unhandled(),
                }
            },

            // CompoundNode[SlotSequence, {_, arg:LeafNode[Integer, _, _]}, data_]
            Operator::SlotSequence => {
                expect_children!(children, {_, arg:LeafNode[Integer, _, _]});

                WL!( CallNode[ToNode[SlotSequence], {abstract_(arg)}, data] )
            },

            // CompoundNode[Out, {_, arg:LeafNode[Integer, _, _]}, data_]
            Operator::Out => {
                expect_children!(children, {_, arg:LeafNode[Integer, _, _]});

                WL!( CallNode[ToNode[Out], {abstract_(arg)}, data])
            },

            op => panic!("unexpected CompoundNode operator: {op:?}"),
        },

        //============
        // PrefixNode
        //============
        Node::Prefix(PrefixNode(OperatorNode {
            op,
            children,
            src: data,
        })) => match op {
            // PrefixNode[Minus, {_, rand_}, data_]
            Operator::Minus => {
                expect_children!(children, {_, rand:_});

                abstract_(negate(rand, data))
            },

            // PrefixNode[Plus, {_, rand_}, _], data_
            Op::Plus => {
                let [_, rand] = expect_children(children);

                abstractPrefixPlus(rand, data)
            },

            // PrefixNode[PrefixNot2, {notNotTok_, rand_}, data_]
            Op::CodeParser_PrefixNot2 => {
                let [notNotTok, rand] = expect_children(children);

                abstractNot2(rand, notNotTok, data)
            },

            Op::CodeParser_PrefixLinearSyntaxBang => {
                let NodeSeq(children) = children;

                match children.as_slice() {
                    // PrefixNode[PrefixLinearSyntaxBang, {rator_, rand:LeafNode[Token`LinearSyntaxBlob, _, _]}, data_]
                    [_, Node::Token(Token {
                        tok: TK::LinearSyntaxBlob,
                        ..
                    })] => {
                        let [rator, rand] = expect_children(NodeSeq(children));

                        // FIXME: keep linear syntax for now
                        // PrefixNode[PrefixLinearSyntaxBang, {rator, abstract[rand]}, data]
                        AstNode::PrefixNode_PrefixLinearSyntaxBang(
                            Box::new([abstract_(rator), abstract_(rand)]),
                            AstMetadata::from(data),
                        )
                    },
                    // PrefixNode[PrefixLinearSyntaxBang, {_, rand_}, data_]
                    [_, rand] => {
                        WL!( AbstractSyntaxErrorNode[LinearSyntaxBang, { abstract_(rand.clone()) }, data])
                    },
                    _ => unhandled(),
                }
            },

            // strings may be quoted
            //
            // concrete syntax: <<a
            // abstract syntax Get["a"]
            //
            // concrete syntax: <<"a"
            // abstract syntax Get["a"]
            //
            // PrefixNode[Get, {_, LeafNode[String, str_, data1_]}, data_]
            Op::Get => {
                // TODO(test): Add test case for prefix get (there doesn't seem
                //             to be one now).
                let [_, rand] = expect_children(children);

                match rand {
                    Node::Token(Token {
                        tok: TK::String,
                        input: str,
                        src: data1,
                    }) => {
                        WL!(CallNode[
                            ToNode[Get],
                            {WL!(LeafNode[String, escapeString_of_abstractFileString(str.as_str()), data1])},
                            data
                        ])
                    },
                    _ => unhandled(),
                }
            },

            // PrefixNode[op_, {_, operand_}, data_]
            op => {
                let [_, operand] = expect_children(children);

                WL!( CallNode[ToNode_Op(op), {abstract_(operand)}, data])
            },
        },

        //============
        // PostfixNode
        //============

        // PostfixNode[op_, {operand_, rator_}, data_]
        Node::Postfix(PostfixNode(OperatorNode {
            op,
            children,
            src: data,
        })) => {
            let [operand, rator] = expect_children(children);

            match op {
                // PostfixNode[System`HermitianConjugate, {rand_, _}, data_]
                // TODO(test): Add test case for this case.
                Op::HermitianConjugate => WL!(
                    CallNode[ToNode[ConjugateTranspose], {abstract_(operand)}, data]
                ),
                Op::Derivative => {
                    match rator {
                        // PostfixNode[Derivative, {rand_, LeafNode[Token`SingleQuote, _, _]}, _]
                        Node::Token(Token {
                            tok: TK::SingleQuote,
                            ..
                        }) => {
                            let (order, abstractedBody) = derivativeOrderAndAbstractedBody(operand);

                            WL!(CallNode[
                                WL!(CallNode[
                                    ToNode[Derivative],
                                    {WL!(LeafNode[Integer, (order + 1).to_string(), <||>])},
                                    <||>
                                ]),
                                { abstractedBody },
                                <||>
                            ])
                        },
                        // PostfixNode[Derivative, {rand_, LeafNode[Token`Boxes`MultiSingleQuote, quoteStr_, _]}, data_]
                        Node::Token(Token {
                            tok: TK::Boxes_MultiSingleQuote,
                            input: quoteStr,
                            ..
                        }) => {
                            let order = quoteStr.as_str().len();

                            WL!(CallNode[
                                WL!( CallNode[ToNode[Derivative], {ToNode_Integer_usize(order)}, <||>] ),
                                {abstract_(operand)},
                                <||>
                            ])
                        },
                        _ => unhandled(),
                    }
                },
                op => WL!(
                    CallNode[ToNode_Op(op), {abstract_(operand)}, data]
                ),
            }
        },

        //============
        // BinaryNode
        //============

        // BinaryNode[Divide, { left_, _, right_ }, data_]
        Node::Binary(BinaryNode(OperatorNode {
            op,
            children,
            src: data,
        })) => {
            let [left, middle, right] = expect_children(children);

            match op {
                Op::Divide => abstractTimes_BinaryNode([left, right], data),

                Operator::CodeParser_BinaryAt => WL!(
                    CallNode[abstract_(left), {abstract_(right)}, data]
                ),

                Operator::MapApply => {
                    if quirks::is_quirk_enabled(Quirk::OldAtAtAt) {
                        let group = GroupNode(OperatorNode {
                            op: Operator::List,
                            children: NodeSeq(vec![
                                agg::WL!(LeafNode[OpenCurly, "{", <||>]),
                                agg::WL!(ToNode[1]),
                                agg::WL!(LeafNode[CloseCurly, "}", <||>]),
                            ]),
                            src: S::unknown(),
                        });

                        WL!(CallNode[
                            ToNode[Apply],
                            {
                                abstract_(left),
                                abstract_(right),
                                abstract_(Node::Group(group))
                            },
                            data
                        ])
                    } else {
                        WL!( CallNode[ToNode[MapApply], {abstract_(left), abstract_(right)}, data] )
                    }
                },

                Operator::CodeParser_BinarySlashSlash => WL!(
                    // Make sure to reverse the arguments
                    CallNode[abstract_(right), {abstract_(left)}, data]
                ),
                Operator::Put | Operator::PutAppend => {
                    let (str, data1) = match right {
                        // {left_, _, LeafNode[String, str_, data1_]}
                        Node::Token(Token {
                            tok: TokenKind::String,
                            ref input,
                            src: data1,
                        }) => (input.as_str(), data1),
                        _ => unhandled(),
                    };


                    WL!(
                        CallNode[
                            ToNode_Op(op),
                            {
                                abstract_(left),
                                WL!( LeafNode[String, escapeString_of_abstractFileString(str), data1] )
                            },
                        data]
                    )
                },

                Operator::Pattern => {
                    WL!( CallNode[ToNode[Pattern], {abstract_(left), abstract_(right)}, data])
                },

                // BinaryNode[Unset, {left_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, data_]
                Operator::Unset => {
                    if !matches!(middle, Node::Token(Token { tok: TK::Equal, .. })) {
                        unhandled()
                    }

                    if !matches!(right, Node::Token(Token { tok: TK::Dot, .. })) {
                        unhandled()
                    }

                    WL!( CallNode[ToNode[Unset], {abstract_(left)}, data] )
                },

                // Abstract NonAssociative errors
                //
                // a ? b ? c being NonAssociative is alluded to being a bug in bug report 206938
                // Related bugs: 206938
                //
                // BinaryNode[PatternTest, {left:BinaryNode[PatternTest, _, _], _, right_}, data_]
                Operator::PatternTest
                    if matches!(
                        left,
                        Node::Binary(BinaryNode(OperatorNode {
                            op: Operator::PatternTest,
                            ..
                        }))
                    ) =>
                {
                    WL!(
                        AbstractSyntaxErrorNode[
                            NonAssociativePatternTest,
                            {abstract_(left), abstract_(right)},
                            data
                        ]
                    )
                },

                op => WL!(
                    CallNode[ToNode_Op(op), {abstract_(left), abstract_(right)}, data]
                ),
            }
        },

        //============
        // InfixNode
        //============
        Node::Infix(InfixNode(OperatorNode {
            op,
            children: NodeSeq(children),
            src: data,
        })) => {
            match op {
                // InfixNode[InfixInequality, children_, data_]
                Op::CodeParser_InfixInequality => abstractInfixInequality(children, data),

                // Handle SameQ and UnsameQ specially because they do not
                // participate in the InfixBinaryAt quirk
                //
                // InfixNode[op:SameQ | UnsameQ, children_ /; OddQ[Length[children]], data_]
                Op::SameQ | Op::UnsameQ if is_odd(children.len()) => {
                    let children = part_span_even_children(children, None);

                    let children = children.into_iter().map(abstract_).collect();

                    WL!( CallNode[ToNode_Op(op), children, data] )
                },

                // InfixNode[Plus, children_, data_]
                Op::Plus => {
                    // Do not do children[[;;;;2]]
                    // need to remember whether Token`Plus or Token`Minus
                    abstractPlus(children, data)
                },

                // InfixNode[Times, children_, data_]
                Op::Times => {
                    // Skip every other child, which are Star tokens.
                    //   children[[;; ;; 2]]
                    let children = part_span_even_children(children, None);

                    abstractTimes_InfixNode(InfixNode(OperatorNode {
                        op: Op::Times,
                        children: NodeSeq(children),
                        src: data,
                    }))
                },

                // InfixNode[Divisible, children_, data_]
                Op::Divisible => {
                    let children = part_span_even_children(children, None);

                    let processed = children
                        .into_iter()
                        .map(|node| processInfixBinaryAtQuirk(node, "Divisible"))
                        // make sure to reverse children of Divisible
                        .rev()
                        .map(abstract_)
                        .collect();

                    WL!( CallNode[ToNode[Divisible], processed, data] )
                },

                // InfixNode[CompoundExpression, children_, data_]
                Op::CompoundExpression => {
                    // Skip every other child, which are Semi tokens.
                    //   children[[;; ;; 2]]
                    let children = part_span_even_children(children, Some(TokenKind::Semi));

                    let children = children
                        .into_iter()
                        // abstractCompoundExpressionChild
                        .map(|node| match node {
                            Node::Token(Token {
                                tok: TK::Fake_ImplicitNull,
                                input: _,
                                src: data,
                            }) => WL!( LeafNode[Symbol, "Null", data.clone()] ),
                            node => abstract_(node),
                        })
                        .collect();

                    WL!( CallNode[ToNode[CompoundExpression], children, data] )
                },

                // InfixNode[MessageName, children_, data_]
                Op::MessageName => {
                    let children = part_span_even_children(children, Some(TK::ColonColon));

                    abstractMessageName(children, data)
                },

                Op::CodeParser_InfixTilde => {
                    // children[[;; ;;2]]
                    let children = part_span_even_children(children, Some(TokenKind::Tilde));

                    abstractInfixTilde(children, data)
                },

                // abstract[InfixNode[op_, children_ /; OddQ[Length[children]], data_]] :=
                //   CallNode[ToNode[op], abstract /@ (processInfixBinaryAtQuirk[#, ToString[op]]& /@ children[[;;;;2]]), data]
                _ => {
                    if !is_odd(children.len()) {
                        unhandled()
                    }

                    // TODO(robust): Add and use Operator::as_token_kind(), and
                    //               pass that as the second arg to
                    //               part_span_even_children()?
                    let children = part_span_even_children(children, None);

                    let children = children
                        .into_iter()
                        .map(|child| {
                            processInfixBinaryAtQuirk(child, op.to_symbol().symbol_name().as_str())
                        })
                        .map(abstract_)
                        .collect();

                    WL!( CallNode[ToNode_Op(op), children, data] )
                },
            }
        },

        //============
        // TernaryNode
        //============

        // TernaryNode[TagSet, {left_, _, middle_, _, right_}, data_]
        Node::Ternary(TernaryNode(OperatorNode {
            op,
            children,
            src: data,
        })) => {
            let [left, _, middle, middle_right, right] = expect_children(children);

            match op {
                Operator::CodeParser_TernaryTilde => {
                    // handle  a ~f,~ b
                    //
                    // Cannot have  (f,)[a, b]
                    if matches!(
                        middle,
                        Node::Infix(InfixNode(OperatorNode {
                            op: Op::CodeParser_Comma,
                            ..
                        }))
                    ) {
                        // TODO(test): Add test case for this case.
                        // TernaryNode[TernaryTilde, {left_, _, middle:InfixNode[Comma, _, _], _, right_}, data_]
                        let abstractedMiddle = abstract_(middle);

                        let (abstractedMiddle_2, abstractedMiddle_3) =
                            abstractedMiddle.into_children_and_source();

                        WL!(
                            CallNode[
                                WL!(AbstractSyntaxErrorNode[
                                    CommaTopLevel,
                                    abstractedMiddle_2,
                                    abstractedMiddle_3
                                ]),
                                { abstract_(left), abstract_(right)},
                                data
                            ]
                        )
                    } else {
                        // TernaryNode[TernaryTilde, {left_, _, middle_, _, right_}, data_]
                        WL!( CallNode[abstract_(middle), {abstract_(left), abstract_(right)}, data] )
                    }
                },
                // Allow non-Symbols for left; not a syntax error
                Operator::TagSet => {
                    WL!( CallNode[ToNode[TagSet], {abstract_(left), abstract_(middle), abstract_(right)}, data] )
                },
                // Allow non-Symbols for left; not a syntax error
                Operator::TagSetDelayed => {
                    WL!( CallNode[ToNode[TagSetDelayed], {abstract_(left), abstract_(middle), abstract_(right)}, data])
                },
                // Allow non-Symbols for left; not a syntax error
                // TernaryNode[TagUnset, {left_, _, middle_, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, data_]
                Operator::TagUnset => {
                    if !matches!(middle_right, Node::Token(Token { tok: TK::Equal, .. })) {
                        unhandled()
                    }

                    if !matches!(right, Node::Token(Token { tok: TK::Dot, .. })) {
                        unhandled()
                    }

                    WL!( CallNode[ToNode[TagUnset], {abstract_(left), abstract_(middle)}, data])
                },
                Operator::Span => WL!(
                    CallNode[ToNode[Span], {abstract_(left), abstract_(middle), abstract_(right)}, data]
                ),
                // TernaryOptionalPattern comes from boxes
                Operator::CodeParser_TernaryOptionalPattern => WL!(
                    CallNode[
                        ToNode[Optional],
                        {
                            WL!( CallNode[ToNode[Pattern], {abstract_(left), abstract_(middle)}, <||>] ),
                            abstract_(right)
                        },
                        data
                    ]
                ),
                _ => unhandled(),
            }
        },

        Node::Call(call) => abstract_call_node::abstract_call_node(call),

        //============
        // GroupNode
        //============
        Node::Group(GroupNode(OperatorNode {
            op,
            children,
            src: data,
        })) => {
            match op {
                Op::CodeParser_GroupParen => {
                    let NodeSeq(children) = children;

                    let children: Result<[_; 3], _> = children.try_into();

                    match children {
                        // GroupNode[GroupParen, { _, InfixNode[Comma, commaChildren, _], _ }, data_]
                        Ok(
                            [_, Node::Infix(InfixNode(OperatorNode {
                                op: Op::CodeParser_Comma,
                                children: NodeSeq(comma_children),
                                ..
                            })), _],
                        ) => {
                            let comma_children = part_span_even_children(comma_children, None);
                            let comma_children = Abstract(NodeSeq(comma_children));

                            WL!( AbstractSyntaxErrorNode[OpenParen, comma_children, data] )
                        },

                        // GroupNode[GroupParen, { _, child_, _}, data_]
                        Ok([_, child, _]) => abstract_(child),

                        // GroupNode[GroupParen, children_, data_]
                        Err(children) => {
                            // children[[2 ;; -2]]
                            let children = part_span_drop_first_and_last(children);
                            let children = Abstract(NodeSeq(children));

                            WL!( AbstractSyntaxErrorNode[OpenParen, children, data] )
                        },
                    }
                },

                // GroupNode errors
                //
                // naked []
                //
                // naked ::[]
                //
                // naked \[LeftDoubleBracket]\[RightDoubleBracket]

                // GroupNode[GroupSquare, children_, data_]
                Op::CodeParser_GroupSquare => match children.0.as_slice() {
                    // GroupNode[GroupSquare, {_, InfixNode[Comma, commaChildren_, _], _}, data_]
                    [_, Node::Infix(InfixNode(OperatorNode {
                        op: Op::CodeParser_Comma,
                        children: NodeSeq(comma_children),
                        ..
                    })), _] => {
                        let comma_children = part_span_even_children(comma_children.clone(), None);
                        let comma_children = Abstract(NodeSeq(comma_children));

                        WL!( AbstractSyntaxErrorNode[OpenSquare, comma_children, data] )
                    },
                    // GroupNode[GroupSquare, {_, child_, _}, data_]
                    [_, child, _] => {
                        WL!( AbstractSyntaxErrorNode[OpenSquare, { abstract_(child.clone()) }, data] )
                    },
                    // GroupNode[GroupSquare, {_, _}, data_]
                    [_, _] => {
                        WL!( AbstractSyntaxErrorNode[OpenSquare, {}, data] )
                    },
                    _ => unhandled(),
                },

                // GroupNode[GroupTypeSpecifier, children_, data_]
                Op::CodeParser_GroupTypeSpecifier => match children.0.as_slice() {
                    // GroupNode[GroupTypeSpecifier, {_, InfixNode[Comma, commaChildren_, _], _}, data_]
                    [_, Node::Infix(InfixNode(OperatorNode {
                        op: Op::CodeParser_Comma,
                        children: NodeSeq(comma_children),
                        ..
                    })), _] => {
                        let comma_children = part_span_even_children(comma_children.clone(), None);
                        let comma_children = Abstract(NodeSeq(comma_children));

                        WL!( AbstractSyntaxErrorNode[ColonColonOpenSquare, comma_children, data] )
                    },
                    // GroupNode[GroupTypeSpecifier, {_, child_, _}, data_]
                    [_, child, _] => {
                        WL!( AbstractSyntaxErrorNode[ColonColonOpenSquare, { abstract_(child.clone()) }, data] )
                    },
                    // GroupNode[GroupTypeSpecifier, {_, _}, data_]
                    [_, _] => {
                        WL!( AbstractSyntaxErrorNode[ColonColonOpenSquare, {}, data] )
                    },
                    _ => unhandled(),
                },

                // GroupNode[GroupDoubleBracket, children_, data_]
                Op::CodeParser_GroupDoubleBracket => match children.0.as_slice() {
                    // GroupNode[GroupDoubleBracket, {_, InfixNode[Comma, commaChildren_, _], _}, data_]
                    [_, Node::Infix(InfixNode(OperatorNode {
                        op: Op::CodeParser_Comma,
                        children: NodeSeq(comma_children),
                        ..
                    })), _] => {
                        let comma_children = part_span_even_children(comma_children.clone(), None);
                        let comma_children = Abstract(NodeSeq(comma_children));

                        WL!( AbstractSyntaxErrorNode[LeftDoubleBracket, comma_children, data] )
                    },
                    // GroupNode[GroupDoubleBracket, {_, child_, _}, data_]
                    [_, child, _] => {
                        WL!( AbstractSyntaxErrorNode[LeftDoubleBracket, { abstract_(child.clone()) }, data] )
                    },
                    // GroupNode[GroupDoubleBracket, {_, _}, data_]
                    [_, _] => {
                        WL!( AbstractSyntaxErrorNode[LeftDoubleBracket, {}, data] )
                    },
                    _ => unhandled(),
                },

                // GroupNode[tag_, children_, data_]
                _ => AstNode::from(abstractGroupNode(GroupNode(OperatorNode {
                    op,
                    children,
                    src: data,
                }))),
            }
        },

        //==============================
        // GroupMissingCloserNode
        //==============================
        Node::GroupMissingCloser(node) => {
            let (op, abstracted_children, data) = abstractGroupNode_GroupMissingCloserNode(node);

            AstNode::GroupMissingCloser {
                kind: op,
                children: abstracted_children,
                data,
            }
        },

        //==============================
        // GroupMissingOpenerNode
        //==============================
        Node::GroupMissingOpener(node) => abstractGroupNode_GroupMissingOpenerNode(node),

        //=================
        // PrefixBinaryNode
        //=================
        Node::PrefixBinary(PrefixBinaryNode(OperatorNode {
            op,
            children,
            src: data,
        })) => {
            let [_, operand1, operand2] = expect_children(children);

            match (op, operand2) {
                // PrefixBinaryNode[
                //     Integrate | etc.,
                //     {_, operand1_, PrefixNode[DifferentialD | CapitalDifferentialD, {_, var_}, _]},
                //     data_
                // ]
                (
                    Op::Integrate
                    | Op::ContourIntegral
                    | Op::DoubleContourIntegral
                    | Op::ClockwiseContourIntegral
                    | Op::CounterClockwiseContourIntegral,
                    //
                    Node::Prefix(PrefixNode(OperatorNode {
                        op: Op::DifferentialD | Op::CapitalDifferentialD,
                        children,
                        src: _,
                    })),
                ) => {
                    let [_, var] = expect_children(children);

                    WL!(CallNode[
                        ToNode_Op(op),
                        {abstract_(operand1), abstract_(var)},
                        data
                    ])
                },
                // TODO: Is this case reachable? Are there any legal
                //       PrefixBinaryNode's other than the Op::*Integral
                //       variants listed above?
                //
                // PrefixBinaryNode[op_, {_, operand1_, operand2_}, data_]
                (_, operand2) => {
                    WL!(CallNode[
                        ToNode_Op(op),
                        {abstract_(operand1), abstract_(operand2)},
                        data
                    ])
                },
            }
        },

        // Do not touch CodeNodes
        Node::Code(CodeNode { first, second, src }) => AstNode::Code {
            first,
            second,
            data: AstMetadata::from_src(src),
        },

        //==============================
        // BoxNode
        //==============================

        // FIXME: keep boxes for now
        //
        // Abstract any child boxes
        Node::Box(box_node) => abstract_box_node(box_node),

        //==============================
        // SyntaxErrorNode
        //==============================
        Node::SyntaxError(SyntaxErrorNode {
            err,
            children: NodeSeq(children),
            src: data,
        }) => match (err, children.as_slice()) {
            (SyntaxErrorKind::ExpectedTilde, [left, _, middle]) => WL!(
                SyntaxErrorNode[ExpectedTilde, {abstract_(left.clone()), abstract_(middle.clone())}, data]
            ),
            (SyntaxErrorKind::ExpectedSet, [left, _, middle]) => WL!(
                SyntaxErrorNode[ExpectedSet, {abstract_(left.clone()), abstract_(middle.clone())}, data]
            ),
            /*
            abstract[SyntaxErrorNode[SyntaxError`OldFESyntax, children_, data_]] :=
                SyntaxErrorNode[SyntaxError`OldFESyntax, abstract /@ children, data]

            abstract[SyntaxErrorNode[SyntaxError`BuggyFESyntax, children_, data_]] :=
                SyntaxErrorNode[SyntaxError`BuggyFESyntax, abstract /@ children, data]

            abstract[SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, {left_, _, _, right_}, data_]] :=
                SyntaxErrorNode[SyntaxError`ExpectedSetOperand1, {abstract[left], abstract[right]}, data]

            abstract[SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, {left_, _, middle_, _}, data_]] :=
                SyntaxErrorNode[SyntaxError`ExpectedSetOperand2, {abstract[left], abstract[middle]}, data]
            */
            (SyntaxErrorKind::ExpectedSymbol, [left, _, right]) => WL!(
                SyntaxErrorNode[ExpectedSymbol, {abstract_(left.clone()), abstract_(right.clone())}, data]
            ),
            _ => todo!("unhandled SyntaxErrorNode content: ({err:?}, {children:?})"),
        },
    }
}

fn abstract_replace_token<I: TokenInput, S: TokenSource>(token: Token<I, S>) -> AstNode {
    let Token {
        tok: kind,
        input,
        src: data,
    } = token;

    let node: AstNode = match kind {
        TokenKind::PercentPercent => {
            let str = input.as_str();

            // count = StringCount[s, "%" | "\\.25" | "\\:0025" | "\\|000025" | "\\045" | "\\[RawPercent]"];
            let mut count: usize = 0;
            count += str.matches("%").count();
            count += str.matches("\\.25").count();
            count += str.matches("\\:0025").count();
            count += str.matches("\\|000025").count();
            count += str.matches("\\045").count();
            count += str.matches("\\[RawPercent]").count();

            let count = i64::try_from(count).expect("Out[..] %-sequence overflows i64");

            WL!(CallNode[
                ToNode_Op(Op::Out),
                vec![ToNode_Integer(-count)],
                data
            ])
        },
        TokenKind::Under => WL!( CallNode[ToNode[Blank], {}, data] ),
        TokenKind::UnderUnder => WL!( CallNode[ToNode[BlankSequence], {}, data] ),
        TokenKind::UnderUnderUnder => WL!( CallNode[ToNode[BlankNullSequence], {}, data] ),
        TokenKind::UnderDot => {
            WL!( CallNode[ToNode[Optional], { WL!(CallNode[ToNode[Blank], {}, data.clone()]) }, data] )
        },
        TokenKind::Hash => WL!( CallNode[ToNode[Slot], { ToNode_Integer(1) }, data] ),
        TokenKind::HashHash => WL!( CallNode[ToNode[SlotSequence], { ToNode_Integer(1) }, data] ),
        TokenKind::Percent => WL!( CallNode[ToNode[Out], {}, data] ),

        TokenKind::Fake_ImplicitOne => WL!( LeafNode[Integer, "1", data] ),
        // FIXME: This should be "System`All", so that "All" doesn't resolve
        //        into the wrong context if System` is not on $ContextPath?
        TokenKind::Fake_ImplicitAll => WL!( LeafNode[Symbol, "All", data] ),

        TokenKind::Error_PrefixImplicitNull | TokenKind::Error_InfixImplicitNull => {
            WL!( LeafNode[Symbol, "Null", data] )
        },

        kind if kind.isError() => AstNode::Error {
            kind,
            input: input.into_owned(),
            data: AstMetadata::from_src(data),
        },

        TokenKind::Symbol => {
            match input.as_str() {
                // "\[Pi]"
                "\u{03c0}" | "\\[Pi]" | "\\:03c0" | "\\|0003c0" => {
                    WL!( LeafNode[Symbol, "Pi", data] )
                },

                // "\[Degree]"
                "\u{00b0}" | "\\[Degree]" | "\\:00b0" | "\\.b0" | "\\260" | "\\|0000b0" => {
                    WL!( LeafNode[Symbol, "Degree", data] )
                },

                // "\[Infinity]"
                "\u{221e}" | "\\[Infinity]" | "\\:221e" | "\\|00221e" => {
                    WL!( LeafNode[Symbol, "Infinity", data] )
                },

                // "\[ExponentialE]"
                "\u{f74d}" | "\\[ExponentialE]" | "\\:f74d" | "\\|00f74d" => {
                    WL!( LeafNode[Symbol, "E", data] )
                },

                // "\[ImaginaryI]"
                "\u{f74e}" | "\\[ImaginaryI]" | "\\:f74e" | "\\|00f74e" => {
                    WL!( LeafNode[Symbol, "I", data] )
                },

                // NOTE: It is NOT a bug that \[ImaginaryJ] turns into the same
                //       thing as \[ImaginaryI]. (Or, at the very least, its a
                //       bug so old its not going to change now.)
                // "\[ImaginaryJ]"
                "\u{f74f}" | "\\[ImaginaryJ]" | "\\:f74f" | "\\|00f74f" => {
                    WL!( LeafNode[Symbol, "I", data] )
                },

                _ => AstNode::Leaf {
                    kind,
                    input: input.into_owned(),
                    data: AstMetadata::from_src(data),
                },
            }
        },

        // Symbols, Strings, Integers, Reals, and Rationals just get passed
        // through
        //
        // Also, LinearSyntaxBlob just gets passed through
        kind => AstNode::Leaf {
            kind,
            input: input.into_owned(),
            data: AstMetadata::from_src(data),
        },
    };

    node
}

//======================================
// Helper Functions
//======================================

pub(crate) fn expect_children<I: Debug, S: Debug, const N: usize>(
    children: AggNodeSeq<I, S>,
) -> [Node<I, S>; N] {
    let NodeSeq(children) = children;

    let children: [Node<I, S>; N] = match children.try_into() {
        Ok(children) => children,
        Err(children) => panic!(
            "node did not have the expected number of children (expected {N}, got {})",
            children.len()
        ),
    };

    children
}


/// Ported composition of `escapeString(abstractSymbolString(s))`.
fn escapeString_of_abstractSymbolString(s: &str) -> String {
    // FIXME: This is made up. Consult original WL source and finish porting
    //        this function.
    if s.starts_with('"') {
        format!("{s}")
    } else if s.starts_with(r"\[") {
        format!("\"{s}\"")
    } else {
        format!("{s:?}")
    }
}

/// Ported composition of `escapeString(abstractFileString(s))`.
fn escapeString_of_abstractFileString(s: &str) -> String {
    // FIXME: This is made up. Consult original WL source and finish porting
    //        this function.
    if s.starts_with('"') {
        format!("{s}")
    } else {
        format!("{s:?}")
    }
}

/*
========================================
Original, unported definitions of
  - abstractSymbolString
  - abstractFileString
These are subtle string handling functions,
so I'm recording them here until a more
careful port and testing of them can be
completed.
========================================
(*

String "a" -> a
String a -> a

for handling the various stringification operators
#a
#"a"
a::b
a::"b"
*)
abstractSymbolString[str_String /; StringStartsQ[str, "\""]] :=
  Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

abstractSymbolString[str_String] :=
  Quiet[ToExpression["\""<>str<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

(*
a>>b
a>>"b"

The strings might be something like:
b\c => b\\c
b\f => b\\f

FIXME: once the semantics are completely understood, move this to library
*)
abstractFileString[str_String /; StringStartsQ[str, "\""]] :=
Module[{},
  Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]
]

abstractFileString[str_String] :=
Module[{replaced},

  (*
  convert to the language that is understood by quoted strings, to be given to ToExpression
  *)
  replaced = StringReplace[str, {
      (*
      single character escapes
      *)
      "\\b" -> "\\\\b",
      "\\f" -> "\\\\f",
      "\\n" -> "\\\\n",
      "\\r" -> "\\\\r",
      "\\t" -> "\\\\t",
      (*
      and double quote
      *)
      "\"" -> "\\\"",
      (*
      and backslash
      *)
      "\\" -> "\\\\"
    }];

  Quiet[ToExpression["\""<>replaced<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]
]
*/

// fn escapeString(s: &str) -> String {
//     // FIXME: In WL this was defined as:
//     //   ToString[s, InputForm, CharacterEncoding -> "ASCII"]
//     //
//     // What *exactly* does ToString do in that situation? What special
//     // characters are escaped or not?

//     // TODO: Use some combination of str::escape_debug()/std::escape_default()/
//     //       custom escaping logic here?

//     format!("{s:?}")
// }

// /// String "a" -> a
// /// String a -> a
// ///
// /// for handling the various stringification operators
// ///
// /// * `#a`
// /// * `#"a"`
// /// * `a::b`
// /// * `a::"b"`
// fn abstractSymbolString(s: &str) -> String {
//     // abstractSymbolString[str_String /; StringStartsQ[str, "\""]] :=
//     //   Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

//     // abstractSymbolString[str_String] :=
//     //   Quiet[ToExpression["\""<>str<>"\""], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]

//     if s.starts_with('\"') {
//     }
// }

#[track_caller]
fn unhandled() -> ! {
    // TODO: Turn this into an Err(..) instead of a panic?
    // Failure["Unhandled", ..]
    todo!("unhandled case")
}

//--------------------------------------
// negate
//--------------------------------------

fn parenthesizedIntegerOrRealQ<I: Debug, S: Debug>(node: &Node<I, S>) -> bool {
    match node {
        // parenthesizedIntegerOrRealQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
        //     parenthesizedIntegerOrRealQ[child]
        Node::Group(GroupNode(OperatorNode {
            op: Operator::CodeParser_GroupParen,
            children,
            src: _,
        })) => {
            let [_, child, _]: &[_; 3] = children
                .0
                .as_slice()
                .try_into()
                .expect("GroupParen Group node has unexpected number of children");

            parenthesizedIntegerOrRealQ(child)
        },
        // parenthesizedIntegerOrRealQ[LeafNode[Integer, _, _]] := True
        // parenthesizedIntegerOrRealQ[LeafNode[Real, _, _]] := True
        Node::Token(Token {
            tok: TokenKind::Integer | TokenKind::Real,
            input: _,
            src: _,
        }) => true,
        // parenthesizedIntegerOrRealQ[_] := False
        _ => false,
    }
}

fn extractParenthesizedIntegerOrRealQ<I: Debug, S: Debug>(node: Node<I, S>) -> Node<I, S> {
    debug_assert!(parenthesizedIntegerOrRealQ(&node));

    match node {
        // parenthesizedIntegerOrRealQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
        //     parenthesizedIntegerOrRealQ[child]
        Node::Group(GroupNode(OperatorNode {
            op: Operator::CodeParser_GroupParen,
            children: NodeSeq(children),
            src: _,
        })) => {
            let [_, child, _]: [_; 3] = children
                .try_into()
                .expect("GroupParen Group node has unexpected number of children");

            child
        },
        _ => panic!("unable to extract inner parenthesized Integer or Real"),
    }
}

// TODO(optimization): Make this take a `&Node`, so we don't have to
//                     clone at the callsite to this function.
fn possiblyNegatedZeroQ<I: TokenInput + Debug, S: Debug>(node: Node<I, S>) -> bool {
    match node {
        // possiblyNegatedZeroQ[LeafNode[Integer, "0", _]] := True
        Node::Token(Token {
            tok: TokenKind::Integer,
            input,
            src: _,
        }) if input.as_str() == "0" => true,
        // possiblyNegatedZeroQ[GroupNode[GroupParen, { _, child_, _ }, _]] :=
        //     possiblyNegatedZeroQ[child]
        Node::Group(GroupNode(OperatorNode {
            op: Operator::CodeParser_GroupParen,
            children,
            src: _,
        })) => {
            expect_children!(children, {_, child:_, _});

            possiblyNegatedZeroQ(child)
        },
        // possiblyNegatedZeroQ[PrefixNode[Minus, { _, child_}, _]] :=
        //     possiblyNegatedZeroQ[child]
        Node::Prefix(PrefixNode(OperatorNode {
            op: Operator::Minus,
            children,
            src: _,
        })) => {
            expect_children!(children, {_, child:_});

            possiblyNegatedZeroQ(child)
        },
        // possiblyNegatedZeroQ[_] := False
        _ => false,
    }
}

// concrete syntax does not have negated numbers
// abstract syntax is allowed to have negated numbers
//
// This can happen with:  a-EOF
//
// negate[node:ErrorNode[Token`Error`ExpectedOperand, _, _], _] :=
//   node
fn negate<I: TokenInput + Debug, S: TokenSource + Debug>(
    node: Node<I, S>,
    data: S,
) -> Node<OwnedTokenInput, S> {
    match node {
        Node::Token(Token {
            tok: TokenKind::Integer,
            input,
            src: _,
        }) => {
            let str = input.as_str();

            if str == "0" {
                // negate[LeafNode[Integer, "0", _], data_] :=
                agg::WL!(LeafNode[Integer, "0", data]).into_owned_input()
            } else {
                // negate[LeafNode[Integer, str_, _], data_] :=
                agg::WL!(LeafNode[Integer, format!("-{str}"), data])
            }
        },
        Node::Token(Token {
            tok: TokenKind::Real,
            input,
            src: _,
        }) => {
            let str = input.as_str();

            // negate[LeafNode[Real, str_, _], data_] :=
            agg::WL!( LeafNode[Real, format!("-{str}"), data] )
        },
        // dig down into parens
        //
        // something like  -(1.2)  is still parsed as  -1.2
        //
        // TODO: maybe this is a kernel quirk?
        //
        // negate[GroupNode[GroupParen, {_, child_?possiblyNegatedZeroQ, _}, _], data_] :=
        //   negate[child, data]
        Node::Group(GroupNode(OperatorNode {
            op: Operator::CodeParser_GroupParen,
            children: NodeSeq(mut children),
            src: _,
            // TODO(optimization): Avoid this clone().
        })) if possiblyNegatedZeroQ(children[1].clone()) => negate(children.remove(1), data),
        // negate[PrefixNode[Minus, {_, child_?possiblyNegatedZeroQ}, _], data_] :=
        //   negate[child, data]
        Node::Prefix(PrefixNode(OperatorNode {
            op: Operator::Minus,
            children: NodeSeq(mut children),
            src: _,
            // TODO(optimization): Avoid this clone().
        })) if possiblyNegatedZeroQ(children[1].clone()) => negate(children.remove(1), data),
        // negate[node_?parenthesizedIntegerOrRealQ, data_] :=
        //   negate[node[[2, 2]], data]
        node if parenthesizedIntegerOrRealQ(&node) => {
            let child = extractParenthesizedIntegerOrRealQ(node);
            negate(child, data)
        },

        //
        // NOT ABSTRACTED YET!
        //
        // Important to use InfixNode[Times and not just CallNode[Times,
        //
        // This allows these nodes to be merged later e.g., 1-a/b
        //

        // negate[InfixNode[Times, children_, _], data_] :=
        //   InfixNode[Times, { ToNode[-1], LeafNode[Token`Star, "*", <||>] } ~Join~ children, data]
        Node::Infix(InfixNode(OperatorNode {
            op: Operator::Times,
            children: NodeSeq(mut children),
            src: _,
        })) => {
            children.insert(0, agg::WL!(LeafNode[Star, "*", <||>]));
            children.insert(0, agg::WL!(ToNode[-1]));

            let infix = InfixNode(OperatorNode {
                op: Operator::Times,
                children: NodeSeq(children).into_owned_input(),
                src: data,
            });

            Node::Infix(infix)
        },
        // negate[node_, data_] :=
        //   InfixNode[Times, { ToNode[-1], LeafNode[Token`Star, "*", <||>], node }, data]
        node => {
            let children = NodeSeq(vec![
                agg::WL!(ToNode[-1]),
                agg::WL!(LeafNode[Star, "*", <||>]),
                node,
            ]);

            let infix = InfixNode(OperatorNode {
                op: Operator::Times,
                children: children.into_owned_input(),
                src: data,
            });

            Node::Infix(infix)
        },
    }
}

//======================================

fn reciprocate<I: TokenInput, S: TokenSource>(node: Node<I, S>, data: S) -> Node<I, S> {
    /*
        CallNode[
            ToNode[Power],
            GroupNode[GroupSquare, {
                LeafNode[Token`OpenSquare, "[", <||>],
                InfixNode[Comma, { node, LeafNode[Token`Comma, ",", <||>], ToNode[-1] }, <||>],
                LeafNode[Token`CloseSquare, "]", <||>]
            }, <||> ],
            data
        ]
    */
    Node::Call(CallNode {
        head: NodeSeq(vec![agg::WL!(ToNode[Power])]),
        body: CallBody::Group(GroupNode(OperatorNode {
            op: Op::CodeParser_GroupSquare,
            children: NodeSeq(vec![
                agg::WL!(LeafNode[OpenSquare, "[", <||>]),
                agg::WL!(InfixNode[
                    CodeParser_Comma,
                    {
                        node,
                        agg::WL!(LeafNode[Comma, ",", <||>]),
                        agg::WL!(ToNode[-1])
                    },
                    <||>
                ]),
                agg::WL!(LeafNode[CloseSquare, "]", <||>]),
            ]),
            src: S::unknown(),
        })),
        src: data,
        is_concrete: false,
    })
}

//======================================

/// Collect all of the `'` in `f'''[x]`
fn derivativeOrderAndAbstractedBody<I: TokenInput + Debug, S: TokenSource + Debug>(
    node: Node<I, S>,
) -> (usize, AstNode) {
    match node {
        Node::Postfix(PostfixNode(OperatorNode {
            op: Op::Derivative,
            children,
            src: _,
        })) => {
            let [rand, _] = expect_children(children);

            let (order, body) = derivativeOrderAndAbstractedBody(rand);

            (order + 1, body)
        },
        _ => (0, abstract_(node)),
    }
}

//======================================

fn processPlusPair<I: TokenInput + Debug, S: TokenSource + Debug>(
    pair: [Node<I, S>; 2],
) -> Node<OwnedTokenInput, S> {
    match pair {
        // {LeafNode[Token`Plus | Token`LongName`ImplicitPlus, _, _], rand_}
        [Node::Token(Token {
            tok: TK::Plus | TK::LongName_ImplicitPlus,
            input: _,
            src: _,
        }), rand] => rand.into_owned_input(),
        // {LeafNode[Token`Minus | Token`LongName`Minus, _, opData_], rand_}
        [Node::Token(Token {
            tok: TK::Minus | TK::LongName_Minus,
            input: _,
            src: opData,
        }), rand] => {
            // When parsing a - b + c, make sure to give the abstracted Times expression the correct Source.
            // That is, the source of  - b
            let source: S = {
                let opData = opData.into_general();

                match (opData, rand.source().into_general()) {
                    (a, b) if a.is_unknown() || b.is_unknown() => S::unknown(),
                    (GeneralSource::String(opData), GeneralSource::String(rand)) => {
                        S::from_source(Source {
                            start: opData.start,
                            end: rand.end,
                        })
                    },
                    (GeneralSource::BoxPosition(_), _) | (_, GeneralSource::BoxPosition(_)) => {
                        todo!("processPlusPair synthetic source of BoxPosition's")
                    },
                    (GeneralSource::After(_), _) | (_, GeneralSource::After(_)) => {
                        todo!("processPlusPair synthetic source of After's")
                    },
                }
            };

            negate(rand, source)
        },
        _ => unhandled(),
    }
}

// is it a quirk that  a + + b  is parsed as  a + b  ?
// The prefix + is eaten
// TODO: add to kernel quirks mode
fn flattenPrefixPlus<I: Debug, S: Debug>(node: Node<I, S>) -> Node<I, S> {
    match node {
        Node::Prefix(PrefixNode(OperatorNode {
            op: Op::Plus,
            children,
            src: _,
        })) => {
            let [_, rand] = expect_children(children);

            flattenPrefixPlus(rand)
        },
        rand => rand,
    }
}

/// abstract syntax of  +a + b - c \[ImplicitPlus] d  is a single Plus expression
///
/// except when it's not
///
/// Related bugs: 365287
///
/// TODO: add 365287 to kernel quirks mode
fn abstractPlus<I: TokenInput + Debug, S: TokenSource + Debug>(
    children: Vec<Node<I, S>>,
    data: S,
) -> AstNode {
    debug_assert!(children.len() > 0 && is_odd(children.len()));

    let pairs: Vec<_> = children[1..]
        .chunks(2)
        .map(|chunk: &[Node<I, S>]| {
            let array: [Node<I, S>; 2] = chunk.to_vec().try_into().unwrap();
            array
        })
        .collect();

    let processedPairs = pairs.into_iter().map(processPlusPair);

    let flattened = {
        let mut children = vec![children[0].clone().into_owned_input()];
        children.extend(processedPairs);
        children.into_iter().map(flattenPrefixPlus)
    };

    let processed = flattened.map(|node| processInfixBinaryAtQuirk(node, "Plus"));

    let children = processed.map(abstract_).collect();

    WL!( CallNode[ToNode[Plus], children, data])
}

/// + +a  parses the same as  +a
///
/// is it a quirk that  + +a  is parsed as  +a  ?
///
/// The first + is eaten
///
/// TODO: add to kernel quirks mode
fn abstractPrefixPlus<I: TokenInput + Debug, S: TokenSource + Debug>(
    rand: Node<I, S>,
    data: S,
) -> AstNode {
    match rand {
        // PrefixNode[Plus, {_, rand_}, _], data_
        Node::Prefix(PrefixNode(OperatorNode {
            op: Op::Plus,
            children,
            src: _,
        })) => {
            let [_, rand] = expect_children(children);

            abstractPrefixPlus(rand, data)
        },
        // rand_, data_
        _ => WL!( CallNode[ToNode[Plus], {abstract_(rand)}, data] ),
    }
}

/// abstract syntax of  -a * b / c d \[InvisibleTimes] e \[Times] f  is a single Times expression
fn flattenTimes<I: TokenInput + Debug, S: TokenSource + Debug>(
    nodes: Vec<Node<I, S>>,
    data: S,
) -> Vec<Node<OwnedTokenInput, S>> {
    let flattenTimesQuirk = quirks::is_quirk_enabled(Quirk::FlattenTimes);

    nodes
        .into_iter()
        .flat_map(|node| {
            match node {
                // These rules for PrefixNode illustrate the difference between the FE and kernel
                // Related bugs: 139531
                //
                // TODO: add to kernel quirks mode
                // TODO: add to frontend quirks mode
                Node::Prefix(PrefixNode(OperatorNode {
                    op: Op::Minus,
                    ref children,
                    src: _,
                })) => {
                    let [_, operand] = expect_children(children.clone());

                    match operand {
                        // PrefixNode[Minus, { _, LeafNode[Integer | Real, _, _] }, _]
                        Node::Token(Token {
                            tok: TK::Integer | TK::Real,
                            input: _,
                            src: _,
                        }) => vec![negate(operand, data.clone())],
                        // PrefixNode[Minus, { _, _?parenthesizedIntegerOrRealQ }, _]
                        _ if parenthesizedIntegerOrRealQ(&operand) => {
                            vec![negate(operand, data.clone())]
                        },
                        // PrefixNode[Minus, {_, _}, _]
                        _ => {
                            if flattenTimesQuirk {
                                // (*
                                // it is possible to have nested prefix Minus, e.g., - - a
                                // so must call recursively into flattenTimes
                                // *)
                                let mut vec: Vec<Node<OwnedTokenInput, S>> =
                                    vec![agg::WL!(ToNode[-1]).into_owned_input()];
                                vec.extend_from_slice(&flattenTimes(vec![operand], data.clone()));
                                vec
                            } else {
                                vec![node.into_owned_input()]
                            }
                        },
                    }
                },
                Node::Infix(InfixNode(OperatorNode {
                    op: Op::Times,
                    children: NodeSeq(children),
                    src: _,
                })) => {
                    let children = part_span_even_children(children, Some(TK::Plus));

                    flattenTimes(children, data.clone())
                },
                // This rule for BinaryNode[Divide] illustrates the difference between the FE and kernel
                //
                // TODO: add to kernel quirks mode
                // TODO: add to frontend quirks mode
                Node::Binary(BinaryNode(OperatorNode {
                    op: Op::Divide,
                    ref children,
                    src: _,
                })) => {
                    if flattenTimesQuirk {
                        let [left, _, right] = expect_children(children.clone());

                        flattenTimes(vec![left, reciprocate(right, data.clone())], data.clone())
                    } else {
                        vec![node.into_owned_input()]
                    }
                },
                _ => vec![node.into_owned_input()],
            }
        })
        .collect()
}

// InfixNode[Times, children_, data_]
fn abstractTimes_InfixNode<I: TokenInput + Debug, S: TokenSource + Debug>(
    infix: InfixNode<I, S>,
) -> AstNode {
    let InfixNode(OperatorNode {
        op,
        children: NodeSeq(children),
        src: data,
    }) = infix;

    debug_assert!(op == Op::Times);

    let flattened = flattenTimes(children, data.clone());

    let processed: Vec<Node<OwnedTokenInput, S>> = flattened
        .into_iter()
        .map(|node| processInfixBinaryAtQuirk(node, "Times"))
        .collect();

    let children = processed.into_iter().map(abstract_).collect();

    WL!( CallNode[ToNode[Times], children, data] )
}

// BinaryNode[Divide, {left_, right_}, data_]
fn abstractTimes_BinaryNode<I: TokenInput + Debug, S: TokenSource + Debug>(
    [left, right]: [Node<I, S>; 2],
    data: S,
) -> AstNode {
    let children = flattenTimes(vec![left, reciprocate(right, data.clone())], data.clone())
        .into_iter()
        .map(abstract_)
        .collect();

    WL!( CallNode[ToNode[Times], children, data] )
}

//======================================

// (*
// strings may be quoted
//
// concrete syntax: a::b
// abstract syntax MessageName[a, "b"]
//
// concrete syntax: a::"b"
// abstract syntax MessageName[a, "b"]
// *)
fn abstractMessageName<I: TokenInput + Debug, S: TokenSource + Debug>(
    mut children: Vec<Node<I, S>>,
    data: S,
) -> AstNode {
    let (left, rest) = (children.remove(0), children);

    // FIXME: Port this issues code
    let mut issues = Vec::new();

    // a::b::c::d
    if rest.len() > 2 {
        let source = data.clone().into_general();

        issues.push(Issue::syntax(
            IssueTag::SyntaxUndocumentedMessageName,
            "This syntax is not documented.".to_owned(),
            Severity::Error,
            source,
            1.0,
        ));
    };

    if !issues.is_empty() {
        // TODO: Port this? At the moment, Node's can't contain syntax
        //       issues anyway, so there is nothing to merge.
        // issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
        // AssociateTo[data, AbstractSyntaxIssues -> issues];
    };

    let data = AstMetadata {
        source: data.into_general(),
        issues,
    };

    let mut children = vec![abstract_(left)];
    children.extend(rest.into_iter().map(|node| match node {
        // LeafNode[String, str_, data_]
        Node::Token(Token {
            tok: TK::String,
            input: str,
            src: data,
        }) => WL!(
            LeafNode[String, escapeString_of_abstractSymbolString(str.as_str()), data]
        ),
        // FIXME: The original WL here didn't call abstract[..] here. Is that
        //        because it is not possible for `children` to contain anything
        //        but LeafNode's?
        child => abstract_(child),
    }));

    WL!( CallNode[ToNode[MessageName], children, data] )
}


/// attempt to simplify e.g. Inequality[a, Less, b, Less, c] to Less[a, b, c]
///
/// Also integrate the newer VectorInequality functionality
fn abstractInfixInequality<I: TokenInput + Debug, S: TokenSource + Debug>(
    children: Vec<Node<I, S>>,
    data: S,
) -> AstNode {
    let first = children[0].clone();
    let first = abstract_(first);

    let mut processed: (AstNode, Vec<(Symbol, AstNode)>) = (first, vec![]);

    let pairs: Vec<(Token<I, S>, Node<I, S>)> = children[1..]
        .chunks(2)
        .map(|chunk: &[Node<I, S>]| {
            let [left, right]: [Node<I, S>; 2] = chunk.to_vec().try_into().unwrap();

            let left = match left {
                Node::Token(token) => token,
                other => panic!(
                    "abstractInfixInequality: expected odd index to contain Token; got: {other:?}"
                ),
            };

            (left, right)
        })
        .collect();

    //
    // affinity is purposely not True nor False when starting
    //
    let mut affinity = None;

    for (rator, rand) in pairs {
        let rator: Symbol = inequalityOperatorToSymbol(rator.tok);
        let rand = abstract_(rand);

        if vectorInequalityAffinity(rator) == Some(false) {
            if affinity == Some(true) {
                // (*
                // affinity is True, so all operators up to now are 1 sub node
                // *)
                processed = (
                    simplifyInfixInequality(processed, affinity, data.clone()),
                    vec![(rator, rand)],
                );
            } else {
                processed.1.push((rator, rand));
            };
            //
            // affinity is definitely False now
            //
            affinity = Some(false);
        } else if vectorInequalityAffinity(rator) == Some(true) {
            if affinity == Some(false) {
                //
                // affinity is True, so all operators up to now are 1 sub node
                //
                processed = (
                    simplifyInfixInequality(processed, affinity, data.clone()),
                    vec![(rator, rand)],
                );
            } else {
                processed.1.push((rator, rand));
            };
            //
            // affinity is definitely True now
            //
            affinity = Some(true);
        } else {
            processed.1.push((rator, rand));
        }
    }

    simplifyInfixInequality(processed, affinity, data)
}

fn simplifyInfixInequality<S: TokenSource>(
    processed: (AstNode, Vec<(Symbol, AstNode)>),
    affinity: Option<bool>,
    data: S,
) -> AstNode {
    // rators = processed[[2;;-2;;2]];
    // rands = processed[[1;;-1;;2]];

    // let rators = part_span_even_children(part_span_drop_first_and_last(processed), None);
    // let rands = part_span_even_children(processed, None);

    let rators: Vec<Symbol> = processed.1.iter().map(|(rator, _)| *rator).collect();
    // TODO(optimization): Refactor to remove clone()'s
    let rands: Vec<AstNode> = std::iter::once(processed.0.clone())
        .chain(processed.1.clone().into_iter().map(|(_, rand)| rand))
        .collect();

    let all_rators = |needle: Symbol| -> bool { rators.iter().all(|rator| *rator == needle) };

    //
    // Try simple cases of all the same operator first
    //
    match rators {
        _ if all_rators(sym::Equal) => WL!(CallNode[ToNode_Symbol(sym::Equal), rands, data]),
        _ if all_rators(sym::Unequal) => WL!(CallNode[ToNode_Symbol(sym::Unequal), rands, data]),
        _ if all_rators(sym::Greater) => WL!(CallNode[ToNode_Symbol(sym::Greater), rands, data]),
        _ if all_rators(sym::Less) => WL!(CallNode[ToNode_Symbol(sym::Less), rands, data]),
        _ if all_rators(sym::GreaterEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::GreaterEqual), rands, data])
        },
        _ if all_rators(sym::GreaterEqualLess) => {
            WL!(CallNode[ToNode_Symbol(sym::GreaterEqualLess), rands, data])
        },
        _ if all_rators(sym::GreaterFullEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::GreaterFullEqual), rands, data])
        },
        _ if all_rators(sym::GreaterGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::GreaterGreater), rands, data])
        },
        _ if all_rators(sym::GreaterLess) => {
            WL!(CallNode[ToNode_Symbol(sym::GreaterLess), rands, data])
        },
        _ if all_rators(sym::GreaterTilde) => {
            WL!(CallNode[ToNode_Symbol(sym::GreaterTilde), rands, data])
        },
        _ if all_rators(sym::LessEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::LessEqual), rands, data])
        },
        _ if all_rators(sym::LessEqualGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::LessEqualGreater), rands, data])
        },
        _ if all_rators(sym::LessFullEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::LessFullEqual), rands, data])
        },
        _ if all_rators(sym::LessGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::LessGreater), rands, data])
        },
        _ if all_rators(sym::LessLess) => WL!(CallNode[ToNode_Symbol(sym::LessLess), rands, data]),
        _ if all_rators(sym::LessTilde) => {
            WL!(CallNode[ToNode_Symbol(sym::LessTilde), rands, data])
        },
        _ if all_rators(sym::NestedGreaterGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::NestedGreaterGreater), rands, data])
        },
        _ if all_rators(sym::NestedLessLess) => {
            WL!(CallNode[ToNode_Symbol(sym::NestedLessLess), rands, data])
        },
        _ if all_rators(sym::NotGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::NotGreater), rands, data])
        },
        _ if all_rators(sym::NotGreaterEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::NotGreaterEqual), rands, data])
        },
        _ if all_rators(sym::NotGreaterFullEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::NotGreaterFullEqual), rands, data])
        },
        _ if all_rators(sym::NotGreaterGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::NotGreaterGreater), rands, data])
        },
        _ if all_rators(sym::NotGreaterLess) => {
            WL!(CallNode[ToNode_Symbol(sym::NotGreaterLess), rands, data])
        },
        _ if all_rators(sym::NotGreaterSlantEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::NotGreaterSlantEqual), rands, data])
        },
        _ if all_rators(sym::NotGreaterTilde) => {
            WL!(CallNode[ToNode_Symbol(sym::NotGreaterTilde), rands, data])
        },
        _ if all_rators(sym::NotLess) => WL!(CallNode[ToNode_Symbol(sym::NotLess), rands, data]),
        _ if all_rators(sym::NotLessEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::NotLessEqual), rands, data])
        },
        _ if all_rators(sym::NotLessFullEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::NotLessFullEqual), rands, data])
        },
        _ if all_rators(sym::NotLessGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::NotLessGreater), rands, data])
        },
        _ if all_rators(sym::NotLessLess) => {
            WL!(CallNode[ToNode_Symbol(sym::NotLessLess), rands, data])
        },
        _ if all_rators(sym::NotLessSlantEqual) => {
            WL!(CallNode[ToNode_Symbol(sym::NotLessSlantEqual), rands, data])
        },
        _ if all_rators(sym::NotLessTilde) => {
            WL!(CallNode[ToNode_Symbol(sym::NotLessTilde), rands, data])
        },
        _ if all_rators(sym::NotNestedGreaterGreater) => {
            WL!(CallNode[ToNode_Symbol(sym::NotNestedGreaterGreater), rands, data])
        },
        _ if all_rators(sym::NotNestedLessLess) => {
            WL!(CallNode[ToNode_Symbol(sym::NotNestedLessLess), rands, data])
        },
        _ if all_rators(sym::VectorLess) => {
            //
            // Yes, make sure that it is VectorLess[{a, b, c}] and not VectorLess[a, b, c]
            //
            WL!(CallNode[ToNode[VectorLess], { WL!(CallNode[ToNode[List], rands, <||>]) }, data])
        },
        _ if all_rators(sym::VectorGreater) => {
            WL!(CallNode[ToNode[VectorGreater], { WL!(CallNode[ToNode[List], rands, <||>]) }, data])
        },
        _ if all_rators(sym::VectorLessEqual) => {
            WL!(CallNode[ToNode[VectorLessEqual], { WL!(CallNode[ToNode[List], rands, <||>]) }, data])
        },
        _ if all_rators(sym::VectorGreaterEqual) => {
            WL!(CallNode[ToNode[VectorGreaterEqual], { WL!(CallNode[ToNode[List], rands, <||>]) }, data])
        },
        _ => {
            let children = {
                let mut children = vec![processed.0];

                for (rator, rand) in processed.1 {
                    children.push(ToNode_Symbol(rator));
                    children.push(rand);
                }

                children
            };

            match affinity {
                Some(true) => {
                    //
                    // Anything containing a combination inequality and Vector inequality operators is abstracted to VectorInequality
                    // Related bugs: 385771
                    //
                    WL!(CallNode[ToNode_Symbol(sym::Developer::VectorInequality), children, data])
                },
                Some(false) => {
                    WL!(CallNode[ToNode_Symbol(sym::Inequality), children, data])
                },
                None => {
                    WL!(CallNode[ToNode_Symbol(sym::Inequality), children, data])
                },
            }
        },
    }
}

fn inequalityOperatorToSymbol(tok: TokenKind) -> Symbol {
    match tok {
        TK::EqualEqual | TK::LongName_Equal | TK::LongName_LongEqual => sym::Equal,
        TK::BangEqual | TK::LongName_NotEqual => sym::Unequal,
        TK::Less => sym::Less,
        TK::Greater => sym::Greater,
        TK::LessEqual | TK::LongName_LessEqual => sym::LessEqual,
        TK::GreaterEqual | TK::LongName_GreaterEqual => sym::GreaterEqual,
        TK::LongName_GreaterEqualLess => sym::GreaterEqualLess,
        TK::LongName_GreaterFullEqual => sym::GreaterFullEqual,
        TK::LongName_GreaterGreater => sym::GreaterGreater,
        TK::LongName_GreaterLess => sym::GreaterLess,
        //
        // GreaterSlantEqual parses to GreaterEqual
        // Related bugs: 78439
        //
        TK::LongName_GreaterSlantEqual => sym::GreaterEqual,
        TK::LongName_GreaterTilde => sym::GreaterTilde,
        TK::LongName_LessEqualGreater => sym::LessEqualGreater,
        TK::LongName_LessFullEqual => sym::LessFullEqual,
        TK::LongName_LessGreater => sym::LessGreater,
        TK::LongName_LessLess => sym::LessLess,
        //
        // LessSlantEqual parses to LessEqual
        // Related bugs: 78439
        //
        TK::LongName_LessSlantEqual => sym::LessEqual,
        TK::LongName_LessTilde => sym::LessTilde,
        TK::LongName_NestedGreaterGreater => sym::NestedGreaterGreater,
        TK::LongName_NestedLessLess => sym::NestedLessLess,
        TK::LongName_NotGreater => sym::NotGreater,
        TK::LongName_NotGreaterEqual => sym::NotGreaterEqual,
        TK::LongName_NotGreaterFullEqual => sym::NotGreaterFullEqual,
        TK::LongName_NotGreaterGreater => sym::NotGreaterGreater,
        TK::LongName_NotGreaterLess => sym::NotGreaterLess,
        TK::LongName_NotGreaterSlantEqual => sym::NotGreaterSlantEqual,
        TK::LongName_NotGreaterTilde => sym::NotGreaterTilde,
        TK::LongName_NotLess => sym::NotLess,
        TK::LongName_NotLessEqual => sym::NotLessEqual,
        TK::LongName_NotLessFullEqual => sym::NotLessFullEqual,
        TK::LongName_NotLessGreater => sym::NotLessGreater,
        TK::LongName_NotLessLess => sym::NotLessLess,
        TK::LongName_NotLessSlantEqual => sym::NotLessSlantEqual,
        TK::LongName_NotLessTilde => sym::NotLessTilde,
        TK::LongName_NotNestedGreaterGreater => sym::NotNestedGreaterGreater,
        TK::LongName_NotNestedLessLess => sym::NotNestedLessLess,

        TK::LongName_VectorLess => sym::VectorLess,
        TK::LongName_VectorGreater => sym::VectorGreater,
        TK::LongName_VectorLessEqual => sym::VectorLessEqual,
        TK::LongName_VectorGreaterEqual => sym::VectorGreaterEqual,

        other => panic!(
            "inequalityOperatorToSymbol(): token '{other:?}' is not a known inequality operator"
        ),
    }
}


fn vectorInequalityAffinity(op: Symbol) -> Option<bool> {
    let boole = match op {
        //
        // Just these operators do not have an affinity, neither True nor False
        //
        sym::Equal
        | sym::Unequal
        | sym::Less
        | sym::Greater
        | sym::LessEqual
        | sym::GreaterEqual => return None,

        //
        // Definitely NOT a VectorInequality
        //
        sym::GreaterEqualLess
        | sym::GreaterFullEqual
        | sym::GreaterGreater
        | sym::GreaterLess
        | sym::GreaterSlantEqual
        | sym::GreaterTilde
        | sym::LessEqualGreater
        | sym::LessFullEqual
        | sym::LessGreater
        | sym::LessLess
        | sym::LessSlantEqual
        | sym::LessTilde
        | sym::NestedGreaterGreater
        | sym::NestedLessLess
        | sym::NotGreater
        | sym::NotGreaterEqual
        | sym::NotGreaterFullEqual
        | sym::NotGreaterGreater
        | sym::NotGreaterLess
        | sym::NotGreaterSlantEqual
        | sym::NotGreaterTilde
        | sym::NotLess
        | sym::NotLessEqual
        | sym::NotLessFullEqual
        | sym::NotLessGreater
        | sym::NotLessLess
        | sym::NotLessSlantEqual
        | sym::NotLessTilde
        | sym::NotNestedGreaterGreater
        | sym::NotNestedLessLess => false,

        //
        // Definitely a VectorInequality
        //
        sym::VectorLess | sym::VectorGreater | sym::VectorLessEqual | sym::VectorGreaterEqual => {
            true
        },

        _ => panic!("vectorInequalityAffinity(): unrecognized symbol: {op:?}"),
    };

    Some(boole)
}

//======================================

//
// only from boxes
//

fn abstractInfixTilde<I: TokenInput + Debug, S: TokenSource + Debug>(
    children: Vec<Node<I, S>>,
    data: S,
) -> AstNode {
    // TODO:
    match children.as_slice() {
        [_, _] => {
            let [left, middle] = expect_children(NodeSeq(children));

            let left = abstract_(left);
            let middle = abstract_(middle);

            WL!(AbstractSyntaxErrorNode[ExpectedTilde, {left, middle}, data])
        },
        [_, _, _] => {
            let [left, middle, right] = expect_children(NodeSeq(children));

            abstractInfixTildeLeftAlreadyAbstracted(abstract_(left), vec![middle, right], data)
        },
        // abstractInfixTilde[InfixNode[InfixTilde, {left_, middle_, right_, rest___}, dataIn_]] :=
        //     abstractInfixTildeLeftAlreadyAbstracted[InfixNode[InfixTilde, {abstractInfixTilde[InfixNode[InfixTilde, {left, middle, right}, <||>]], rest}, dataIn]]
        [left, middle, right, rest @ ..] => {
            // TODO(optimization): Refactor to remove these clone()'s/to_vec().
            let left = left.clone();
            let middle = middle.clone();
            let right = right.clone();
            let rest = rest.to_vec();

            abstractInfixTildeLeftAlreadyAbstracted(
                abstractInfixTilde(vec![left, middle, right], S::unknown()),
                rest,
                data,
            )
        },
        _ => panic!("abstractInfixTilde: invalid number of children: {children:?}"),
    }
}

fn abstractInfixTildeLeftAlreadyAbstracted<I: TokenInput + Debug, S: TokenSource + Debug>(
    left: AstNode,
    rest: Vec<Node<I, S>>,
    data: S,
) -> AstNode {
    match rest.as_slice() {
        [_] => {
            let [middle] = expect_children(NodeSeq(rest));
            let middle = abstract_(middle);
            WL!(AbstractSyntaxErrorNode[ExpectedTilde, {left, middle}, data])
        },
        [_, _] => {
            let [middle, right] = expect_children(NodeSeq(rest));

            WL!(CallNode[abstract_(middle), {left, abstract_(right)}, data])
        },
        [middle, right, rest @ ..] => {
            let middle = middle.clone();
            let right = right.clone();
            let rest = rest.to_vec();

            abstractInfixTildeLeftAlreadyAbstracted(
                abstractInfixTildeLeftAlreadyAbstracted(left, vec![middle, right], S::unknown()),
                rest,
                data,
            )
        },
        _ => panic!("abstractInfixTildeLeftAlreadyAbstracted: invalid number of rest: {rest:?}"),
    }
}


//======================================
// abstractGroupNode
//======================================

/// Precondition: Opener and Closer are still present
/// Precondition: Commas are still present
///
/// Removes all commas
///
/// Fills in Nulls and gives SyntaxIssues for e.g. {1,,2}
fn abstractGroupNode<I: TokenInput + Debug, S: TokenSource + Debug>(
    group: GroupNode<I, S>,
) -> AstCall {
    let GroupNode(OperatorNode {
        op: tag,
        children: NodeSeq(children),
        src: data,
    }) = group;

    // children = children[[2 ;; -2]];
    let children = part_span_drop_first_and_last(children);

    // abstractedChildren = Flatten[selectChildren /@ (abstract /@ children)];
    let abstracted_children = children
        .into_iter()
        .map(abstract_)
        .flat_map(|child: AstNode| selectChildren(child))
        .collect();

    /*  FIXME: Port this issues handling code
        issues = Lookup[data, AbstractSyntaxIssues, {}];

        If issues != {} {
            AssociateTo[data, AbstractSyntaxIssues -> issues];
        };
    */

    // CallNode[ToNode[tag], abstractedChildren, data]
    AstCall {
        // TODO(clean): Is this head field read anywhere in the callers of
        //              abstractGroupNode()? I don't think so, since the
        //              ToNode_Op(tag) where tag is CodeParser`* are not valid
        //              abstract syntax nodes anyway.
        head: Box::new(ToNode_Op(tag)),
        args: abstracted_children,
        data: data.into_general(),
    }
}

fn abstractGroupNode_GroupMissingCloserNode<I: TokenInput + Debug, S: TokenSource + Debug>(
    group: GroupMissingCloserNode<I, S>,
) -> (Operator, Vec<AstNode>, AstMetadata) {
    let GroupMissingCloserNode(OperatorNode {
        op,
        children: NodeSeq(mut children),
        src: data,
    }) = group;

    // children[[2;;]]
    children.remove(0);

    let abstractedChildren = children
        .into_iter()
        .map(abstract_)
        .flat_map(selectChildren)
        .collect();

    (op, abstractedChildren, AstMetadata::from_src(data))
}

fn abstractGroupNode_GroupMissingOpenerNode<I: TokenInput + Debug, S: TokenSource + Debug>(
    group: GroupMissingOpenerNode<I, S>,
) -> AstNode {
    let GroupMissingOpenerNode(OperatorNode {
        op,
        children: NodeSeq(mut children),
        src: data,
    }) = group;

    // children[[;;-2]]
    children.pop();

    let abstracted_children = children
        .into_iter()
        .map(abstract_)
        .flat_map(selectChildren)
        .collect();

    AstNode::GroupMissingOpener {
        kind: op,
        children: abstracted_children,
        data: AstMetadata::from_src(data),
    }
}


fn selectChildren(node: AstNode) -> Vec<AstNode> {
    // selectChildren[CallNode[ToNode[Comma], children_, _]] := children
    //
    // selectChildren[n_] := n
    match node {
        // TODO(cleanup): Refactor how Comma nodes are abstracted so that
        //                fake AstNode::Call of CodeParser`Comma heads is not
        //                necessary.
        AstNode::Call {
            ref head,
            ref args,
            data: _,
        } => {
            if let AstNode::Leaf {
                kind: TK::Symbol,
                input,
                data: _,
            } = &**head
            {
                if input.as_str() == "CodeParser`Comma" {
                    // TODO(optimization): Refactor to remove this clone()
                    args.clone()
                } else {
                    vec![node]
                }
            } else {
                vec![node]
            }
        },
        _ => vec![node],
    }
}

//======================================

fn abstractNot2<I: TokenInput + Debug, S: TokenSource + Debug>(
    rand: Node<I, S>,
    notNotTok: Node<I, S>,
    data: S,
) -> AstNode {
    // notNotData = notNotTok[[3]];
    let notNotData = notNotTok.source();

    // let mut issues = Lookup[data, AbstractSyntaxIssues, {}];
    let mut issues = Vec::new();

    let notNotData_source = notNotData.into_general();

    issues.push(Issue::syntax(
        IssueTag::PrefixNotNot,
        "Unexpected parse.".to_owned(),
        Severity::Warning,
        notNotData_source,
        1.0,
    ));

    let data = AstMetadata {
        source: data.into_general(),
        issues,
    };

    WL!(CallNode[
        WL!(LeafNode[Symbol, "Not", <||>]),
        {
            WL!(CallNode[
                WL!(LeafNode[Symbol, "Not", <||>]),
                { abstract_(rand) },
                <||>
            ])
        },
        data
    ])
}

//======================================

fn abstract_box_node<I: TokenInput + Debug, S: TokenSource + Debug>(
    box_node: BoxNode<I, S>,
) -> AstNode {
    // FIXME: Add test cases for and finish porting the todo!(..) cases below.
    match box_node.kind {
        //
        // `a` is a List of boxes
        //
        // BoxNode[RowBox, {a_}, data_]
        BoxKind::RowBox => {
            todo!("RowBox[..] abstracting logic")
            // BoxNode[RowBox, {abstract /@ a}, data]
        },
        //
        // `a` is a List of Lists
        //
        // BoxNode[GridBox, {a_, rest___}, data_]
        BoxKind::GridBox => {
            todo!("GridBox[..] abstracting logic");
            // WL!(BoxNode[GridBox, {Map[abstract, a, {2}]} ~Join~ (abstract /@ {rest}), data])
        },
        _ => (),
    }

    let box_node = match try_subscript_box_part_special_cases(box_node) {
        Ok(ast) => return ast,
        Err(node) => node,
    };

    let box_node = match try_superscript_box_derivative_special_case(box_node) {
        Ok(ast) => return ast,
        Err(node) => node,
    };

    //-----------------------------------------
    // Now handle the BoxNode[..] general case:
    //   BoxNode[kind_, children_, data_]
    //-----------------------------------------

    let BoxNode {
        kind,
        children: NodeSeq(children),
        src: data,
    } = box_node;

    let children = children.into_iter().map(abstract_).collect();

    WL!(BoxNode[(kind), children, data])
}

/// Handle special form of [[x]] in subscript
///
/// Keep the [[]] structure un-abstracted
//
// FIXME: when things like SuperscriptBox[] -> Power[] and
//        FractionBox[] -> Divide, then also do
//        SubscriptBox[..., [[]] ] -> Part
fn try_subscript_box_part_special_cases<I: TokenInput + Debug, S: TokenSource + Debug>(
    box_node: BoxNode<I, S>,
) -> Result<AstNode, BoxNode<I, S>> {
    /* Original WL pattern:
    BoxNode[
        SubscriptBox,
        {
            a_,
            GroupNode[GroupSquare, {
                o1:LeafNode[Token`OpenSquare, _, _],
                GroupNode[GroupSquare, {
                    o2:LeafNode[Token`OpenSquare, _, _],
                    b_,
                    c2:LeafNode[Token`CloseSquare, _, _]
                },
                    data2_
                ],
                c1:LeafNode[Token`CloseSquare, _, _]
            },
                data1_
            ],
            ___
        },
        data_
    ]
    */
    let (children, data) = match Node::from(box_node.clone()) {
        LHS!(BoxNode[
            SubscriptBox,
            children:_,
            data:_
        ]) => (children, data),
        _ => return Err(box_node),
    };

    let (a, middle) = match children.0.as_slice() {
        [a, middle, ..] => (a.clone(), middle.clone()),
        _ => return Err(box_node),
    };

    match middle {
        LHS!(GroupNode[
            CodeParser_GroupSquare,
            children1:_,
            data1:_
        ]) => {
            let [o1, group, c1] = expect_children(children1);

            match o1 {
                LHS!(LeafNode[OpenSquare, _, _]) => (),
                _ => return Err(box_node),
            };

            match c1 {
                LHS!(LeafNode[CloseSquare, _, _]) => (),
                _ => return Err(box_node),
            }

            match group {
                LHS!(GroupNode[
                    CodeParser_GroupSquare,
                    children2:_,
                    data2:_
                ]) => {
                    let [o2, b, c2] = expect_children(children2);

                    match o2 {
                        LHS!(LeafNode[OpenSquare, _, _]) => (),
                        _ => return Err(box_node),
                    };

                    match c2 {
                        LHS!(LeafNode[CloseSquare, _, _]) => (),
                        _ => return Err(box_node),
                    }

                    let (o1, o2) = (abstract_(o1), abstract_(o2));
                    let (c1, c2) = (abstract_(c1), abstract_(c2));

                    let ast = WL!(BoxNode[
                        SubscriptBox,
                        vec![
                            abstract_(a),
                            AstNode::Group {
                                kind: Op::CodeParser_GroupSquare,
                                children: Box::new((
                                    o1,
                                    AstNode::Group{
                                        kind: Op::CodeParser_GroupSquare,
                                        children: Box::new((
                                            o2,
                                            abstract_(b),
                                            c2
                                        )),
                                        data: AstMetadata::from_src(data2)
                                    },
                                    c1
                                )),
                                data: AstMetadata::from_src(data1)
                            }
                        ],
                        data
                    ]);

                    return Ok(ast);
                },
                _ => return Err(box_node),
            }
        },
        LHS!(GroupNode[
            CodeParser_GroupDoubleBracket,
            children1:_,
            data1:_
        ]) => {
            let [o, b, c] = expect_children(children1);

            match o {
                LHS!(LeafNode[LongName_LeftDoubleBracket, _, _]) => (),
                _ => return Err(box_node),
            };

            match c {
                LHS!(LeafNode[LongName_RightDoubleBracket, _, _]) => (),
                _ => return Err(box_node),
            }

            // BoxNode[SubscriptBox, {abstract[a], GroupNode[GroupDoubleBracket, {o, abstract[b], c}, data1]}, data]
            let ast = WL!(BoxNode[
                SubscriptBox,
                vec![
                    abstract_(a),
                    AstNode::Group {
                        kind: Op::CodeParser_GroupDoubleBracket,
                        children: Box::new((
                            abstract_(o),
                            abstract_(b),
                            abstract_(c)
                        )),
                        data: AstMetadata::from_src(data1)
                    }
                ],
                data
            ]);

            return Ok(ast);
        },
        _ => Err(box_node),
    }
}

/// Handle special form of `TagBox[(), Derivative]` in superscript
///
/// Keep the `TagBox[(), Derivative]` structure un-abstracted
///
/// TagBox is considered "easier" than say, FormBox
///
/// Contents of TagBox are largely valid boxes
///
// FIXME: when things like SuperscriptBox[] -> Power[] and
//        FractionBox[] -> Divide, then also do
//        SuperscriptBox[..., TagBox[(), Derivative] ] -> Derivative
//
// FIXME: maybe first arg of TagBox should be treated as a CodeNode and not parsed at all
fn try_superscript_box_derivative_special_case<I: TokenInput + Debug, S: TokenSource + Debug>(
    box_node: BoxNode<I, S>,
) -> Result<AstNode, BoxNode<I, S>> {
    /* Original WL pattern that the nested Rust match/if let statements below
       are unpacking. If this pattern matches, an `Ok(AstNode)` is returned. If
       this pattern does not match, then the original BoxNode is returned as
       the `Err(_)` value.

    BoxNode[
        SuperscriptBox,
        {
            a_,
            BoxNode[TagBox, {
                GroupNode[GroupParen, {
                    o:LeafNode[Token`OpenParen, _, _],
                    b_,
                    c:LeafNode[Token`CloseParen, _, _]
                },
                    data2_
                ],
                t:CodeNode[Evaluated, Derivative, _]},
                data1_
            ],
            ___
        },
        data_
    ]
        => BoxNode[SuperscriptBox, {
            abstract[a],
            BoxNode[TagBox, {
                GroupNode[GroupParen, {o, abstract[b], c}, data2],
                t
            }, data1]
        }, data]
    */
    match Node::from(box_node.clone()) {
        LHS!(BoxNode[
            SuperscriptBox,
            children:_
            ,
            data:_
        ]) => {
            let (a, middle) = match children.0.as_slice() {
                [a, middle, ..] => (a.clone(), middle.clone()),
                _ => todo!("Error?"),
            };

            if let LHS!(BoxNode[
                TagBox,
                children1:_,
                data1:_
            ]) = middle
            {
                let [left, t] = expect_children(children1);

                if let LHS!(GroupNode[
                    CodeParser_GroupParen,
                    children2:_,
                    data2:_
                ]) = left
                {
                    match t {
                        // CodeNode[Null, Derivative, _]
                        Node::Code(
                            ref t @ CodeNode {
                                ref first,
                                ref second,
                                src: _,
                            },
                        ) if first.try_as_symbol().map(wolfram_expr::Symbol::as_str)
                            == Some("System`Evaluated")
                            && second.try_as_symbol().map(wolfram_expr::Symbol::as_str)
                                == Some("System`Derivative") =>
                        {
                            let [o, b, c] = expect_children(children2);


                            if let [o @ LHS!(LeafNode[OpenParen, _, _]), b @ _, c @ LHS!(LeafNode[CloseParen, _, _])] =
                                [o, b, c]
                            {
                                let o = abstract_(o);
                                let b = abstract_(b);
                                let c = abstract_(c);

                                let t = {
                                    let CodeNode { first, second, src } = t.clone();
                                    CodeNode {
                                        first,
                                        second,
                                        src: src.into_general(),
                                    }
                                };

                                let ast = WL!(BoxNode[
                                    SuperscriptBox,
                                    vec! {
                                        abstract_(a),
                                        // GroupNode[
                                        //     CodeParser_GroupParen,
                                        //     NodeSeq(vec![o, abstract_(b), c]),
                                        //     data2
                                        // ]
                                        AstNode::TagBox_GroupParen {
                                            group: Box::new((o, b, c, data2.into_general())),
                                            tag: t,
                                            data: AstMetadata::from_src(data1),
                                        }
                                    },
                                    data
                                ]);

                                Ok(ast)
                            } else {
                                Err(box_node)
                            }
                        },
                        _ => Err(box_node),
                    }
                } else {
                    Err(box_node)
                }
            } else {
                Err(box_node)
            }
        },
        _ => Err(box_node),
    }
}

//======================================

fn is_even(x: usize) -> bool {
    x % 2 == 0
}

fn is_odd(x: usize) -> bool {
    x % 2 == 1
}

/// `children[[;; ;; 2]]`
fn part_span_even_children<I: Debug, S: Debug>(
    children: Vec<Node<I, S>>,
    debug_expected_separator: Option<TokenKind>,
) -> Vec<Node<I, S>> {
    children
        .into_iter()
        .enumerate()
        .filter(|(index, child)| {
            debug_assert!(
                is_even(*index) || is_expected_separator(child, debug_expected_separator),
                "child with is_even == {}, expected to be {debug_expected_separator:?}, was: {child:?}",
                is_even(*index)
            );

            is_even(*index)
        })
        .map(|(_, child)| child)
        .collect()
}

fn is_expected_separator<I, S>(
    child: &Node<I, S>,
    debug_expected_separator: Option<TokenKind>,
) -> bool {
    if let Some(sep) = debug_expected_separator {
        match child {
            Node::Token(Token { tok, .. }) if *tok == sep => true,
            _ => false,
        }
    } else {
        true
    }
}

/// `children[[2 ;; -2]]`
fn part_span_drop_first_and_last<I, S>(mut children: Vec<Node<I, S>>) -> Vec<Node<I, S>> {
    children.remove(0);
    children.pop().unwrap();
    children
}
