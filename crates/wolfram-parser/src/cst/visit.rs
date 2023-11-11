//! Visitor methods for [`Cst`] and related types.

use crate::{
    cst::{
        BinaryNode, BoxNode, CallHead, CallNode, CompoundNode, Cst, CstSeq,
        GroupMissingCloserNode, GroupMissingOpenerNode, GroupNode, InfixNode,
        OperatorNode, PostfixNode, PrefixBinaryNode, PrefixNode,
        SyntaxErrorNode, TernaryNode,
    },
    NodeSeq,
};

//======================================
// CstSeq
//======================================

impl<I, S> CstSeq<I, S> {
    pub fn visit(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        let NodeSeq(elements) = self;

        for elem in elements {
            elem.visit(visit);
        }
    }

    pub fn visit_mut(&mut self, visit: &mut dyn FnMut(&mut Cst<I, S>)) {
        let NodeSeq(elements) = self;

        for elem in elements {
            elem.visit_mut(visit);
        }
    }

    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        let NodeSeq(elements) = self;

        let elements = elements
            .into_iter()
            .map(|elem| elem.map_visit(visit))
            .collect();

        NodeSeq(elements)
    }
}

//======================================
// Cst
//======================================

impl<I, S> Cst<I, S> {
    /// Visit this node and every child node, recursively.
    pub fn visit(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        // Visit the current node.
        visit(self);

        // Visit child nodes.
        match self {
            Cst::Token(_) => (),
            Cst::Call(CallNode { head, body }) => {
                head.visit(visit);
                body.as_op().visit_children(visit);
            },
            Cst::SyntaxError(SyntaxErrorNode { err: _, children }) => {
                children.visit(visit);
            },
            Cst::Prefix(PrefixNode(op)) => op.visit_children(visit),
            Cst::Infix(InfixNode(op)) => op.visit_children(visit),
            Cst::Postfix(PostfixNode(op)) => op.visit_children(visit),
            Cst::Binary(BinaryNode(op)) => op.visit_children(visit),
            Cst::Ternary(TernaryNode(op)) => op.visit_children(visit),
            Cst::PrefixBinary(PrefixBinaryNode(op)) => op.visit_children(visit),
            Cst::Compound(CompoundNode(op)) => op.visit_children(visit),
            Cst::Group(GroupNode(op))
            | Cst::GroupMissingCloser(GroupMissingCloserNode(op))
            | Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                op.visit_children(visit)
            },
            Cst::Box(BoxNode {
                kind: _,
                children,
                src: _,
            }) => {
                children.visit(visit);
            },
            // These node types have no child nodes.
            Cst::Code(_) => (),
        }
    }

    /// Mutably visit this node and every child node, recursively.
    pub fn visit_mut(&mut self, visit: &mut dyn FnMut(&mut Cst<I, S>)) {
        // Visit the current node.
        visit(self);

        // Visit child nodes.
        match self {
            Cst::Token(_) => (),
            Cst::Call(CallNode { head, body }) => {
                head.visit_mut(visit);

                body.as_op_mut().visit_children_mut(visit);
            },
            Cst::SyntaxError(SyntaxErrorNode { err: _, children }) => {
                children.visit_mut(visit)
            },

            Cst::Infix(InfixNode(op)) => {
                op.visit_children_mut(visit);
            },
            Cst::Prefix(PrefixNode(op)) => {
                op.visit_children_mut(visit);
            },
            Cst::Postfix(PostfixNode(op)) => {
                op.visit_children_mut(visit);
            },
            Cst::Binary(BinaryNode(op)) => {
                op.visit_children_mut(visit);
            },
            Cst::Ternary(TernaryNode(op)) => {
                op.visit_children_mut(visit);
            },
            Cst::PrefixBinary(PrefixBinaryNode(op)) => {
                op.visit_children_mut(visit)
            },
            Cst::Compound(CompoundNode(op)) => op.visit_children_mut(visit),
            Cst::Group(GroupNode(op)) => op.visit_children_mut(visit),
            Cst::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                op.visit_children_mut(visit)
            },
            Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                op.visit_children_mut(visit)
            },

            Cst::Box(BoxNode {
                kind: _,
                children,
                src: _,
            }) => {
                children.visit_mut(visit);
            },

            // These node types have no child nodes.
            Cst::Code(_) => (),
        }
    }

    /// Transform this node tree by visiting this node and every child node, recursively.
    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        // Visit the current node.
        let self_ = visit(self);

        // Visit child nodes.
        let node: Cst<I, S> = match self_ {
            Cst::Token(_) => return self_,
            Cst::Call(CallNode { head, body }) => {
                let head = head.map_visit(visit);

                let body = body.map_op(|body_op: OperatorNode<_, _, _>| {
                    body_op.map_visit(visit)
                });

                Cst::Call(CallNode { head, body })
            },
            Cst::SyntaxError(SyntaxErrorNode { err, children }) => {
                let children = children.map_visit(visit);

                Cst::SyntaxError(SyntaxErrorNode { err, children })
            },

            Cst::Infix(InfixNode(op)) => {
                Cst::Infix(InfixNode(op.map_visit(visit)))
            },
            Cst::Prefix(PrefixNode(op)) => {
                Cst::Prefix(PrefixNode(op.map_visit(visit)))
            },
            Cst::Postfix(PostfixNode(op)) => {
                Cst::Postfix(PostfixNode(op.map_visit(visit)))
            },
            Cst::Binary(BinaryNode(op)) => {
                Cst::Binary(BinaryNode(op.map_visit(visit)))
            },
            Cst::Ternary(TernaryNode(op)) => {
                Cst::Ternary(TernaryNode(op.map_visit(visit)))
            },
            Cst::PrefixBinary(PrefixBinaryNode(op)) => {
                Cst::PrefixBinary(PrefixBinaryNode(op.map_visit(visit)))
            },
            Cst::Compound(CompoundNode(op)) => {
                Cst::Compound(CompoundNode(op.map_visit(visit)))
            },
            Cst::Group(GroupNode(op)) => {
                Cst::Group(GroupNode(op.map_visit(visit)))
            },
            Cst::GroupMissingCloser(GroupMissingCloserNode(op)) => {
                Cst::GroupMissingCloser(GroupMissingCloserNode(
                    op.map_visit(visit),
                ))
            },
            Cst::GroupMissingOpener(GroupMissingOpenerNode(op)) => {
                Cst::GroupMissingOpener(GroupMissingOpenerNode(
                    op.map_visit(visit),
                ))
            },

            Cst::Box(BoxNode {
                kind,
                children,
                src,
            }) => {
                let children = children.map_visit(visit);

                Cst::Box(BoxNode {
                    kind,
                    children,
                    src,
                })
            },

            // These node types have no child nodes.
            node @ Cst::Code(_) => node,
        };

        node
    }
}

//======================================
// OperatorNode
//======================================

impl<I, S, O> OperatorNode<I, S, O> {
    /// Visit this node and every child node, recursively.
    fn visit_children(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        let OperatorNode { op: _, children } = self;

        children.visit(visit);
    }

    /// Mutably visit this node and every child node, recursively.
    fn visit_children_mut(&mut self, visit: &mut dyn FnMut(&mut Cst<I, S>)) {
        let OperatorNode { op: _, children } = self;

        children.visit_mut(visit);
    }

    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        let OperatorNode { op, children } = self;

        let children = children.map_visit(visit);

        OperatorNode { op, children }
    }
}

//======================================
// CallHead
//======================================

impl<I, S> CallHead<I, S> {
    /// Visit this node and every child node, recursively.
    pub fn visit(&self, visit: &mut dyn FnMut(&Cst<I, S>)) {
        match self {
            CallHead::Concrete(head) => head.visit(visit),
            CallHead::Aggregate(head) => head.visit(visit),
        }
    }

    /// Mutably visit this node and every child node, recursively.
    pub fn visit_mut(&mut self, visit: &mut dyn FnMut(&mut Cst<I, S>)) {
        match self {
            CallHead::Concrete(head) => head.visit_mut(visit),
            CallHead::Aggregate(head) => head.visit_mut(visit),
        }
    }

    pub fn map_visit(
        self,
        visit: &mut dyn FnMut(Cst<I, S>) -> Cst<I, S>,
    ) -> Self {
        match self {
            CallHead::Concrete(head) => {
                CallHead::Concrete(head.map_visit(visit))
            },
            CallHead::Aggregate(head) => {
                let head: Cst<I, S> = *head;
                CallHead::Aggregate(Box::new(head.map_visit(visit)))
            },
        }
    }
}
