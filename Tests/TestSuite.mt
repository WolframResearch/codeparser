(* Wolfram Language Test file *)

Needs["MUnit`"]

SetEnvironment["CODEPARSER_DEBUG" -> "True"]

TestSuite[{
	"Abstract.mt",
	"AbstractCallNode.mt",
	"AbstractSyntaxErrorNodes.mt",
	"AbstractSyntaxIssues.mt",
	"Aggregate.mt",
	"Arrows.mt",
	"Boxes.mt",
	"CallMissingCloserNodes.mt",
	"Characters.mt",
	"CodeParser.mt",
	"CodeSyntaxQ.mt",
	"Concrete.mt",
	"Concretify.mt",
	"Definitions.mt",
	"Error.mt",
	"Errors.mt",
	"File.mt",
	"Inequality.mt",
	"LineContinuations.mt",
	"Parse.mt",
	"Quirks.mt",
	"Regressions.mt",
	"SafeString.mt",
	"Scoping.mt",
	"Span.mt",
	"SyntaxErrorNodes.mt",
	"SyntaxIssues.mt",
	"TokenErrors.mt",
	"TokenEnum.mt",
	"Tokenize.mt",
	"ToNode.mt",
	"TopLevel.mt",
	"ToString.mt",
	"TypeSpecifier.mt",
	"Unsafe.mt",
	"Weird.mt"
}]
