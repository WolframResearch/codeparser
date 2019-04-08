
# AST Paclet

AST is a paclet for parsing Wolfram Language code.

## What is an AST?

AST stands for Abstract Syntax Tree.

ASTs are a symbolic representation of source code syntax.

WL expressions can be represented by various forms, such as FullForm. But FullForm does not capture
the syntax that was used in the original source code.

`FullForm[f /@ {1,2,3}]` is the same as `FullForm[Map[f, List[1, 2, 3]]]`, even though different source code syntax was used.

So FullForm is not sufficient for representing WL source code syntax.

The AST package introduces functions for parsing WL source code and returning a tree structure that represents the original source code.



## Nodes

All nodes have a uniform structure:

`Node[string or operator, children, data]`







## Layout

The AST source layout has the following structure.

```
ast/
  AST/
    Documentation/
    Kernel/
  cpp/
    include/
    src/
  scripts/
    Generate.wl
  tables/
  CMakeLists.txt
  README.md
```

The cpp directory contains C++ source code.

The scripts directory contains scripts for building AST.

Generate.wl generates C++ and WL source code.

The tables directory contains data for building AST.

The AST directory contains WL source code.

CMakeLists.txt is the CMake file for building AST.










## Installing AST

Call PacletInstall with the path to the built .paclet file.

```
In[1]:= PacletInstall["/path/to/built/AST.paclet"]

Out[2]= Paclet[AST,0.2,<>]

```

## Using AST

After AST is built and installed, it can be used.

```
In[1]:= Needs["AST`"]

In[2]:= ParseString["1+1"]

Out[2]= InfixNode[Plus, {IntegerNode[1, {}, <|Source -> {{1, 1}, {1, 1}}|>],

>     IntegerNode[1, {}, <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]

In[3]:=
```


A wl-ast command-line tool is also built and can be used.

```
$wl-ast
>>> 1+1
InfixNode[Plus, {IntegerNode["1", {}, <|Source->{{1, 1}, {1, 1}}|>], IntegerNode["1", {}, <|Source->{{1, 3}, {1, 3}}|>]}, <|Source->{{1, 1}, {1, 3}}|>]

>>>
```



## Troubleshooting

### Strange behavior on Windows

After installing the AST paclet on Windows, you may see strange behavior such as the kernel and front end crashing or the wl-ast executable disppearing.

This is most likely due to Antivirus software such as Bitdefender incorrectly flagging the wl-ast.exe executable as a virus.

Make sure to whitelist wl-ast.exe to prevent Antivirus software from flagging this paclet as a virus.

<!--- bug 236253 --->







