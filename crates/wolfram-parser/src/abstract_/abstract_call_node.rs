use std::fmt::Debug;

use crate::{
    agg::LHS,
    ast::WL,
    ast::{AstMetadata, AstNode},
    issue::{Issue, IssueTag, Severity},
    node::{
        BinaryNode, BoxKind, BoxNode, CallBody, CallNode, CompoundNode, GroupNode, InfixNode, Node,
        NodeSeq, Operator as Op, OperatorNode, PostfixNode, PrefixNode,
    },
    symbol as sym,
    token::{
        Token, TokenInput,
        TokenKind::{self as TK},
        TokenSource,
    },
};

use super::{
    abstractGroupNode, abstractGroupNode_GroupMissingCloserNode, abstract_, expect_children,
    AstCall, ToNode_Symbol,
};

/// These boxes are ok to have as head of calls
///
/// What is the process for adding boxes to this list?
///
/// It's on-demand as case-by-case basis
const OK_CALL_BOX_KINDS: &[BoxKind] = &[
    BoxKind::TemplateBox,
    BoxKind::InterpretationBox,
    BoxKind::SubscriptBox,
    BoxKind::SuperscriptBox,
    BoxKind::StyleBox,
    BoxKind::NamespaceBox,
    BoxKind::OverscriptBox,
    BoxKind::SubsuperscriptBox,
];

pub(super) fn abstract_call_node<I: TokenInput + Debug, S: TokenSource + Debug>(
    call: CallNode<I, S>,
) -> AstNode {
    match AggCallNode::from_cst(call) {
        //==============================
        // handle CallNode before possible GroupNode errors
        //
        // what are the different shapes that calls can have?
        //
        // They are:
        //
        // f [ ]
        //
        // f [ [ ] ]
        //
        // f ::[ ]
        //
        // f \[LeftDoubleBracket] \[RightDoubleBracket]
        //==============================

        //
        // concrete parse of a[[2]] returns CallNode[a, GroupNode[Square, {GroupNode[Square, {2}]}]]
        // abstract parse of a[[2]] returns CallNode[Part, {a, 2}]
        //
        // So convert from concrete [[ syntax to abstract Part syntax
        //
        /*
        CallNode[
            headIn_,
            c:GroupNode[GroupSquare, {first_, inner:GroupNode[GroupSquare, _, _], last_}, _],
            dataIn_
        ]
        */
        // FIXME(test): Add tests for the branches below. It seems like none of
        //              these are currently tested. E.g. "f[[x]]".
        AggCallNode {
            head,
            body:
                CallBody::Group(GroupNode(OperatorNode {
                    op: Op::CodeParser_GroupSquare,
                    children,
                    src: _,
                })),
            src: data,
        } if matches!(
            children.0[1], // inner
            Node::Group(GroupNode(OperatorNode {
                op: Op::CodeParser_GroupSquare,
                ..
            }))
        ) =>
        {
            // {first_, inner:GroupNode[GroupSquare, _, _], last_}
            let [first, inner, last] = expect_children(children);

            let part = match inner {
                Node::Group(group) => group,
                _ => unreachable!(),
            };

            let mut data = AstMetadata::from_src(data);

            //------------------------------------------------
            // Create issues for some bad or suspicious parses
            //------------------------------------------------

            match head {
                // feel strongly about ##2[[arg]]
                // ##2 represents a sequence of arguments, so it is wrong to call
                LHS!(LeafNode[HashHash, _, _]) | LHS!(CompoundNode[SlotSequence, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCallSlotSequence,
                            "Unexpected ``Part`` call.".to_owned(),
                            Severity::Error,
                            first.source().into_general(),
                            1.0,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    )
                },
                LHS!(LeafNode[Symbol /* | String */ | Hash | Under | UnderUnder | UnderUnderUnder, _, _])
                | LHS!(CallNode[_, _, _])
                | LHS!(CompoundNode[
                    Blank | BlankSequence | BlankNullSequence
                    | CodeParser_PatternBlank | CodeParser_PatternBlankSequence | CodeParser_PatternBlankNullSequence
                    | Slot /* | SlotSequence */,
                    _,
                    _
                ]) => {
                    // these are fine
                },
                LHS!(LeafNode[Percent | PercentPercent, _, _]) | LHS!(CompoundNode[Out, _, _]) => {
                    /*
                    was:

                    data.issues.push(
                        SyntaxIssue["StrangeCall", "Unexpected ``Part`` call.", "Warning", <|
                        Source -> first[[3, Key[Source]]],
                        ConfidenceLevel -> 0.95,
                        "AdditionalSources" -> {last[[3, Key[Source]]]}
                    |>]
                    ];

                    but % is already scanned in CodeInspector TokenRules, and this just adds more noise

                    */

                    // these are fine
                },
                LHS!(LeafNode[LinearSyntaxBlob, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected ``Part`` call.".to_owned(),
                            Severity::Remark,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                LHS!(PrefixNode[CodeParser_PrefixLinearSyntaxBang, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected ``Part`` call.".to_owned(),
                            Severity::Remark,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                LHS!(InfixNode[CompoundExpression, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected ``Part`` call.".to_owned(),
                            Severity::Remark,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                // (*
                // BinaryNode[PatternTest, _, _],
                //     (* these are fine *)
                //     Null
                // ,*)
                LHS!(GroupNode[CodeParser_GroupParen | List | Association, _, _]) => {
                    // these are fine
                },
                LHS!(GroupNode[_, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected ``Part`` call.".to_owned(),
                            Severity::Warning,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                LHS!(PostfixNode[Transpose, _, _]) => {
                    // (*
                    // a\[Transpose][[2]] is fine
                    // *)
                },
                //
                // Now handle boxes
                //
                LHS!(BoxNode[box_kind:_, _, _]) if OK_CALL_BOX_KINDS.contains(&box_kind) => {
                    // (* this is fine *)
                    // Null
                },
                LHS!(BoxNode[_, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            // FIXME: This should format `head` using a pretty
                            //        display form, not Debug.
                            format!("Unexpected ``Part`` call: ``{:?}``.", head),
                            Severity::Error,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                _ => {
                    // (*
                    // warn about anything else
                    // *)

                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected ``Part`` call.".to_owned(),
                            Severity::Error,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
            };

            //-----------------------------------------
            // Construct and return the abstracted node
            //-----------------------------------------

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            // {head} ~Join~ part[[2]]
            let args = {
                let mut args = part.args;
                args.insert(0, head);
                args
            };

            WL!(CallNode[ToNode[Part], args, data])
        },
        //
        // Concrete parse of a[2] returns CallNode[a, GroupNode[Square, {2}]]
        // abstract parse of a[2] returns CallNode[a, {2}]
        //
        // So convert from concrete [ syntax to abstract Call syntax

        // feel strongly about ##2[arg]
        // ##2 represents a sequence of arguments, so it is wrong to call
        LHS!(CallNode[
            head:(LeafNode[HashHash, _, _] | CompoundNode[SlotSequence, _, _]),
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let first = part.0.children.0.first().unwrap();
            let last = part.0.children.0.last().unwrap();

            let mut data = AstMetadata::from_src(data);

            data.issues.push(
                Issue::syntax(
                    IssueTag::StrangeCallSlotSequence,
                    "Unexpected call.".to_owned(),
                    Severity::Error,
                    // first[[3, Key[Source]]]
                    first.source().into_general(),
                    1.0,
                )
                .with_additional_sources(vec![last.source().into_general()]),
            );

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        LHS!(CallNode[
            head:(
                LeafNode[Symbol | String | Hash | Under | UnderUnder | UnderUnderUnder, _, _]
                | CallNode[_, _, _]
                | CompoundNode[Blank | BlankSequence | BlankNullSequence | CodeParser_PatternBlank | CodeParser_PatternBlankSequence | CodeParser_PatternBlankNullSequence | Slot /*| SlotSequence*/, _, _]
            ),
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let head = abstract_(head);
            let AstCall {
                head: _,
                args,
                data: _partData,
            } = abstractGroupNode(part);

            /* FIXME: Finish porting this issue handling logic
                issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

                If[issues != {},
                    issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
                    AssociateTo[data, AbstractSyntaxIssues -> issues];
                ];
            */

            AstNode::Call {
                head: Box::new(head),
                args,
                data: AstMetadata::from_src(data),
            }
        },
        // TODO(test): Add test for this case ("%[5]")
        LHS!(CallNode[
            head:(LeafNode[Percent | PercentPercent, _, _] | CompoundNode[Out, _, _]),
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            // let first = part.0.children.0.first().unwrap();
            // let last = part.0.children.0.last().unwrap();

            // was:
            // AppendTo[issues,
            //     SyntaxIssue["StrangeCall", "Unexpected call.", "Warning", <|
            //     Source -> first[[3, Key[Source]]],
            //     ConfidenceLevel -> 0.95,
            //     "AdditionalSources" -> {last[[3, Key[Source]]]}
            //     |>]
            // ];
            //
            // but % is already scanned in CodeInspector TokenRules, and this just adds more noise

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        LHS!(CallNode[
            head:BinaryNode[PatternTest, _, _],
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        LHS!(CallNode[
            head:InfixNode[CompoundExpression, _, _],
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let first = part.0.children.0.first().unwrap();
            let last = part.0.children.0.last().unwrap();

            let mut data = AstMetadata::from_src(data);

            data.issues.push(
                Issue::syntax(
                    IssueTag::StrangeCall,
                    "Unexpected call.".to_owned(),
                    Severity::Warning,
                    first.source().into_general(),
                    0.95,
                )
                .with_additional_sources(vec![last.source().into_general()]),
            );

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        // these are fine
        // List is allowed because this is popular to do:
        // Through[{a, b, c}[1]]
        LHS!(CallNode[
            head:GroupNode[CodeParser_GroupParen | List | Association, _, _],
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        LHS!(CallNode[
            head:GroupNode[_, _, _],
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let first = part.0.children.0.first().unwrap();
            let last = part.0.children.0.last().unwrap();

            let mut data = AstMetadata::from_src(data);

            data.issues.push(
                Issue::syntax(
                    IssueTag::StrangeCall,
                    "Unexpected call.".to_owned(),
                    Severity::Warning,
                    first.source().into_general(),
                    0.95,
                )
                .with_additional_sources(vec![last.source().into_general()]),
            );

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        //
        // these are fine
        //
        LHS!(CallNode[
            head:PostfixNode[Function | Derivative, _, _],
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        //
        // this is fine
        //
        // LHS!(CallNode[
        //     head:BoxNode[box_kind:_, _, _],
        //     part:GroupNode[CodeParser_GroupSquare, _, _],
        //     data:_
        // ]) if OK_CALL_BOX_KINDS.contains(&box_kind)
        // TODO(test): Add test case for this branch.
        AggCallNode {
            head: Node::Box(head @ BoxNode { kind: box_kind, .. }),
            body: LHS!(part:GroupNode[CodeParser_GroupSquare, _, _]),
            src: data,
        } if OK_CALL_BOX_KINDS.contains(&box_kind) => {
            let head = abstract_(Node::from(head));
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        // LHS!(CallNode[
        //     head:BoxNode[tag:_, _, _],
        //     part:GroupNode[CodeParser_GroupSquare, _, _],
        //     data:_
        // ])
        // TODO(test): Add test case that covers this branch.
        AggCallNode {
            head: Node::Box(head @ BoxNode { kind: tag, .. }),
            body: LHS!(part:GroupNode[CodeParser_GroupSquare, _, _]),
            src: data,
        } => {
            let first = part.0.children.0.first().unwrap();
            let last = part.0.children.0.last().unwrap();

            let mut data = AstMetadata::from_src(data);

            data.issues.push(
                Issue::syntax(
                    IssueTag::StrangeCall,
                    format!("Unexpected call: ``{}``.", tag.as_str()),
                    Severity::Error,
                    first.source().into_general(),
                    0.95,
                )
                .with_additional_sources(vec![last.source().into_general()]),
            );

            let head = abstract_(Node::from(head));
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        //
        // warn about anything else
        //
        LHS!(CallNode[
            head:_,
            part:GroupNode[CodeParser_GroupSquare, _, _],
            data:_]) => {
            let first = part.0.children.0.first().unwrap();
            let last = part.0.children.0.last().unwrap();

            let mut data = AstMetadata::from_src(data);

            data.issues.push(
                Issue::syntax(
                    IssueTag::StrangeCall,
                    "Unexpected call.".to_owned(),
                    Severity::Error,
                    first.source().into_general(),
                    0.95,
                )
                .with_additional_sources(vec![last.source().into_general()]),
            );

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[head, part.args, data])
        },
        //-------------------
        // GroupTypeSpecifier
        //-------------------
        //
        // this is fine
        //
        LHS!(CallNode[
            head:LeafNode[String, _, _],
            part:GroupNode[CodeParser_GroupTypeSpecifier, _, _],
            data:_
        ]) => {
            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[
                WL!(CallNode[ToNode[TypeSpecifier], {head}, <||>]),
                part.args,
                data
            ])
        },
        //
        // warn about anything else
        //
        LHS!(CallNode[head:_, part:GroupNode[CodeParser_GroupTypeSpecifier, _, _], data:_]) => {
            let mut data = AstMetadata::from_src(data);

            data.issues.push(
                Issue::syntax(
                    IssueTag::StrangeCall,
                    "Unexpected call.".to_owned(),
                    Severity::Error,
                    head.source().into_general(),
                    0.95,
                )
                .with_additional_descriptions(vec![
                    "The head of ``::[]`` syntax is usually a string.".to_owned(),
                ]),
            );

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            WL!(CallNode[
                WL!(CallNode[ToNode[TypeSpecifier], {head}, <||>]),
                part.args,
                data
            ])
        },
        //--------------------
        // Concrete parse of a\[LeftDoubleBracket]2\[RightDoubleBracket] returns CallNode[a, GroupNode[DoubleBracket, {2}]]
        // abstract parse of a\[LeftDoubleBracket]2\[RightDoubleBracket] returns CallNode[Part, {a, 2}]
        //--------------------
        LHS!(CallNode[
            head:_,
            part:GroupNode[CodeParser_GroupDoubleBracket, _, _],
            data:_
        ]) => {
            let first = part.0.children.0.first().unwrap();
            let last = part.0.children.0.last().unwrap();

            let mut data = AstMetadata::from_src(data);

            //------------------------------------------------
            // Create issues for some bad or suspicious parses
            //------------------------------------------------

            match head {
                // feel strongly about ##2[arg]
                // ##2 represents a sequence of arguments, so it is wrong to call
                LHS!(LeafNode[HashHash, _, _]) | LHS!(CompoundNode[SlotSequence, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCallSlotSequence,
                            "Unexpected call.".to_owned(),
                            Severity::Error,
                            first.source().into_general(),
                            1.0,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                LHS!(LeafNode[Symbol /* | String */ | Hash | Under | UnderUnder | UnderUnderUnder, _, _])
                | LHS!(CallNode[_, _, _])
                | LHS!(CompoundNode[
                    Blank | BlankSequence | BlankNullSequence
                    | CodeParser_PatternBlank | CodeParser_PatternBlankSequence | CodeParser_PatternBlankNullSequence
                    | Slot /* | SlotSequence */,
                    _,
                    _
                ]) => {
                    // these are fine
                },
                LHS!(LeafNode[Percent | PercentPercent, _, _]) | LHS!(CompoundNode[Out, _, _]) => {
                    // was:
                    //
                    // AppendTo[issues,
                    //     Issue::syntax(IssueTag::StrangeCall, "Unexpected Call.".to_owned(), "Warning", <|
                    //     Source -> first[[3, Key[Source]]],
                    //     ConfidenceLevel -> 0.95,
                    //     "AdditionalSources" -> {last[[3, Key[Source]]]}
                    //     |>]
                    // ];
                    //
                    // but % is already scanned in CodeInspector TokenRules, and this just adds more noise

                    // these are fine
                },
                LHS!(LeafNode[LinearSyntaxBlob, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected Call.".to_owned(),
                            Severity::Remark,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                LHS!(PrefixNode[CodeParser_PrefixLinearSyntaxBang, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected Call.".to_owned(),
                            Severity::Remark,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                /*
                BinaryNode[PatternTest, _, _],
                    (* these are fine *)
                    Null
                ,*/
                LHS!(InfixNode[CompoundExpression, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected ``Part`` call.".to_owned(),
                            Severity::Remark,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                LHS!(GroupNode[CodeParser_GroupParen | List | Association, _, _]) => {
                    // these are fine
                },
                LHS!(GroupNode[_, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected Call.".to_owned(),
                            Severity::Warning,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },

                LHS!(PostfixNode[Transpose, _, _]) => {
                    // a\[Transpose]\[LeftDoubleBracket]2\[RightDoubleBracket] is fine
                },
                //
                // Now handle boxes
                //
                LHS!(BoxNode[box_kind:_, _, _]) if OK_CALL_BOX_KINDS.contains(&box_kind) => {
                    // this is fine
                },
                LHS!(BoxNode[_, _, _]) => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            // FIXME: This should format `head` using a pretty
                            //        display form, not Debug.
                            format!("Unexpected call: ``{:?}``.", head),
                            Severity::Error,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
                //
                // warn about anything else
                //
                _ => {
                    data.issues.push(
                        Issue::syntax(
                            IssueTag::StrangeCall,
                            "Unexpected Call.".to_owned(),
                            Severity::Error,
                            first.source().into_general(),
                            0.95,
                        )
                        .with_additional_sources(vec![last.source().into_general()]),
                    );
                },
            };

            //-----------------------------------------
            // Construct and return the abstracted node
            //-----------------------------------------

            let head = abstract_(head);
            let part = abstractGroupNode(part);

            // {head} ~Join~ (part[[2]])
            let args = {
                let mut args = part.args;
                args.insert(0, head);
                args
            };

            WL!(CallNode[ToNode[Part], args, data])
        },
        //
        // We need special node CallMissingCloserNode because it used to be the
        // case that both `{[a}` and `List[a` had the same AST:
        //
        // CallNode[LeafNode[Symbol, "List", <||>], {
        // GroupMissingCloserNode[GroupSquare, {
        //     LeafNode[Token`OpenSquare, "[", <||>],
        //     LeafNode[Symbol, "a", <||>]}, <||>]}, <||>]
        //
        //
        // we need to distinguish these cases, so it makes sense to have special node to say
        //
        // "this is a CallNode, but with the closer missing"
        //
        // GroupMissingCloserNode gets abstracted
        //
        LHS!(CallNode[
            head:_,
            part:GroupMissingCloserNode[CodeParser_GroupSquare, _, _],
            data:_
        ]) => {
            let head = abstract_(head);

            let (_, children, _) = abstractGroupNode_GroupMissingCloserNode(part);

            WL!(CallMissingCloserNode[head, children, data])
        },
        LHS!(CallNode[
            head:_,
            part:GroupMissingCloserNode[CodeParser_GroupTypeSpecifier, _, _],
            data:_
        ]) => {
            let head = abstract_(head);

            let (_, children, _) = abstractGroupNode_GroupMissingCloserNode(part);

            /* TODO: Port this issue joining logic
                part = abstractGroupNode[part];
                partData = part[[3]];

                issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

                If[issues != {},
                    issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
                    AssociateTo[data, AbstractSyntaxIssues -> issues];
                ];
            */

            WL!(
                CallMissingCloserNode[
                    WL!( CallNode[ToNode_Symbol(sym::TypeSpecifier), {head}, <||>] ),
                    children,
                    data
                ]
            )
        },
        LHS!(CallNode[
            head:_,
            part:GroupMissingCloserNode[CodeParser_GroupDoubleBracket, _, _],
            data:_
        ]) => {
            let head = abstract_(head);

            let (_, mut children, _) = abstractGroupNode_GroupMissingCloserNode(part);

            /* TODO: Port this issue joining logic
                part = abstractGroupNode[part];
                partData = part[[3]];

                issues = Lookup[partData, AbstractSyntaxIssues, {}] ~Join~ issues;

                If[issues != {},
                    issues = Lookup[data, AbstractSyntaxIssues, {}] ~Join~ issues;
                    AssociateTo[data, AbstractSyntaxIssues -> issues];
                ];
            */

            // {head} ~Join~ part[[2]]
            children.insert(0, head);

            WL!(
                CallMissingCloserNode[
                    ToNode_Symbol(sym::Part),
                    children,
                    data
                ]
            )
        },
        call => todo!("abstract_call_node: {call:#?}"),
    }
}

//======================================
// Helper Types
//======================================

//======================================

#[derive(Debug, Clone)]
struct AggCallNode<I, S> {
    head: Node<I, S>,
    body: CallBody<I, S>,
    src: S,
}

impl<I: Debug, S: Debug> AggCallNode<I, S> {
    fn from_cst(call: CallNode<I, S>) -> Self {
        let CallNode {
            head: NodeSeq(head),
            body,
            src,
            is_concrete,
        } = call;

        // TODO(cleanup): Remove or make this a debug_assert!
        assert!(!is_concrete);

        if head.len() != 1 {
            panic!("AggCallNode::from_cst(): CallNode head.len is not equal to 1: {head:?}");
        }

        let head = head.into_iter().next().unwrap();

        AggCallNode { head, body, src }
    }
}
