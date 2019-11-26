
# Nodes

## Terminology

leaf: Integer, Real, Symbol, String, etc.




## Philosophy

if a node is not something else, then it is leaf



## Structure

All nodes have a uniform structure:

`Node[tag or operator, contents or children, opts]`




We take advantage of the symbolic nature of WL and use the function symbols themselves for tags:


a+b is parsed as:
InfixNode[Plus, {LeafNode[Symbol, "a", <||>], LeafNode[Token`Plus, "+", <||>], LeafNode[Symbol, "b", <||>]}, <||>]

and a::b is parsed as:
InfixNode[MessageName, {LeafNode[Symbol, "a", <||>], LeafNode[Token`ColonColon, "::", <||>], LeafNode[String, "b", <||>]





