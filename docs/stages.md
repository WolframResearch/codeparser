docs: stages of parser





bytes

-> decode bytes ->

Source characters

-> decode Source characters ->

WL characters

-> tokenize ->

tokens

-> parse ->

concrete nodes

-> aggregate ->

aggregate nodes

-> abstract ->

abstract nodes













different levels of syntax



# boxes

```
RowBox[{"1", "+", RowBox[{"(*", "*)"}], "a"}]
```

```
RowBox[{"1", "+", RowBox[{"(*", "*)"}], SqrtBox["a"]}]
```

tree structure of tokens

no type information

no Implicit tokens

Trivia is kept



## What is trivia?

Taken from:
https://github.com/dotnet/roslyn/wiki/Roslyn-Overview#syntax-trivia

comments

whitespace

newlines

trivia is only ever RIFFLED between tokens, never at the beginning or end










# concrete

```
InfixNode[Plus, {
	LeafNode[Integer, "1", <||>],
	LeafNode[Token`Plus, "+", <||>],
	LeafNode[Token`Comment, "(**)", <||>],
	LeafNode[Symbol, "a", <||>] }, <||>]

InfixNode[Plus, {
	LeafNode[Integer, "1", <||>],
	LeafNode[Token`Plus, "+", <||>],
	LeafNode[Token`Comment, "(**)", <||>],
	LeafNode[SqrtBox, "a", <||>] }, <||>]
```

Trivia is kept

type information is added
type information is the wrapper like InfixNode[Plus, ...]

and also type information is all of the riffled tokens InfixNode[Plus, { 1, +, 2, \[ImplicitPlus], 3 }]



Implicit tokens are added


## What are Implicit tokens?


when parsing   ;;   it is convenient to remember the implicit   1 ;; All

when parsing a; ;  it is convenient to remember the implicit a ; Null ; Null



implicit Times

when parsing   a b   it is convenient to remember the implicit   a ImplicitTimes b



concrete syntax is everything

concrete syntax has CallNode[{head, comment}, {child1}]

concrete syntax has InfixNode[Plus, {1, +, comment, 1}]














# aggregate

```
InfixNode[Plus, {
	LeafNode[Integer, "1", <||>],
	LeafNode[Token`Plus, "+", <||>],
	LeafNode[Symbol, "a", <||>] }, <||>]

InfixNode[Plus, {
	LeafNode[Integer, "1", <||>],
	LeafNode[Token`Plus, "+", <||>],
	LeafNode[SqrtBox, "a", <||>] }, <||>]
```

type information is kept

Implicit tokens are kept

Trivia is removed

aggregate syntax

aggregate removes comments, whitespace, and newlines

aggregate syntax has CallNode[head, {child1}]

aggregate syntax has InfixNode[Plus, {1, +, 1}]















# abstract

```
CallNode[LeafNode[Symbol, "Plus", <||>], {
				LeafNode[Integer, "1", <||>],
				LeafNode[Symbol, "a", <||>] }, <||>]

CallNode[LeafNode[Symbol, "Plus", <||>], {
				LeafNode[Integer, "1", <||>],
				LeafNode[SqrtBox, "a", <||>] }, <||>]
```

everything is a Call

type information is lost because everything is a CallNode

Implicit tokens are converted to actual tokens

abstract syntax

abstract syntax has CallNode[head, {child1}]

abstract syntax has CallNode[Plus, {1, 1}]





# further work that could be done

* removing line continuations

* converting characters (e.g., \[Infinity] and \[Degree]) to symbols (e.g., Infinity and Degree)

* removing \< \> from strings

* more?










