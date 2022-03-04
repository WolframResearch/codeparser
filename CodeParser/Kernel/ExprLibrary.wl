
BeginPackage["CodeParser`ExprLibrary`"]

ExprTest

GetMetadata


$exprLib

$exprCompiledLib

$exprCompiledLibFuns


(*
CodeParser`ExprLibrary`$BuildExprLibrary must be set to True before CodeParser` is loaded
*)
$BuildExprLibrary


Begin["`Private`"]

Needs["CodeParser`Library`"]

Needs["CompiledLibrary`"]


$exprLib = FileNameJoin[{libraryResources, "expr."<>$sharedExt}]

$exprCompiledLib = CompiledLibrary`CompiledLibrary[$exprLib]

$exprCompiledLibFuns = CompiledLibrary`CompiledLibraryLoadFunctions[$exprCompiledLib]



loadExprLibraryFuncs[] := (

exprTestFunc := exprTestFunc = loadFunc["ExprTest_LibraryLink", {}, Integer];

getMetadataFunc := getMetadataFunc = loadFunc["Get_LibraryLink", {Integer}, Integer];
)

(*
This uses func := func = def idiom and is fast
*)
loadExprLibraryFuncs[]


ExprTest[] :=
Module[{p, e},

  p = libraryFunctionWrapper[exprTestFunc];

  e = $exprCompiledLibFuns["Expr_FromPointer"][p];

  $exprCompiledLibFuns["Expr_Release"][e];

  e
]

GetMetadata[expr_] :=
Module[{p, m},

  p = $exprCompiledLibFuns["Expr_Pointer"][expr];

  m = libraryFunctionWrapper[getMetadataFunc, p];

  m
]

End[]

EndPackage[]
