Print["\n===== Start TokenEnum.mt =====\n"]

Needs["CodeParser`TokenEnum`"]

Test @ tokenIsEmpty[EndOfFile]
Test @ tokenIsEmpty[Token`Fake`ImplicitTimes]
Test @ tokenIsEmpty[Token`Error`Aborted]
Test @ tokenIsEmpty[Token`Fake`ImplicitNull]
Test @ tokenIsEmpty[Token`Fake`ImplicitOne]
Test @ tokenIsEmpty[Token`Fake`ImplicitAll]
Test @ tokenIsEmpty[Token`Error`ExpectedOperand]
Test @ tokenIsEmpty[Token`Error`ExpectedTag]
Test @ tokenIsEmpty[Token`Error`ExpectedFile]
Test @ tokenIsEmpty[Token`Error`PrefixImplicitNull]
Test @ tokenIsEmpty[Token`Error`InfixImplicitNull]

Test @ !tokenIsEmpty[String]
Test @ !tokenIsEmpty[Token`Comma]