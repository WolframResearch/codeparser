# AST

AST is a paclet for parsing Wolfram Language code.


## Installing

Install AST from the public paclet server:
```
In[1]:= PacletUpdate["AST", "Site" -> "http://pacletserver.wolfram.com", "UpdateSites" -> True]

Out[1]= Paclet[AST,0.2,<>]
```


## Setup

Make sure that the paclet can be found on your system:
```
In[1]:= Needs["AST`"]
```


## What is an AST?

AST stands for Abstract Syntax Tree.

ASTs are a symbolic representation of source code syntax.

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

The `cpp` directory contains C++ source code.

The `scripts` directory contains scripts for building AST.

The `tables` directory contains data for building AST.

The `AST` directory contains WL source code.

`CMakeLists.txt` is the CMake file for building AST.


## Building

AST uses a Wolfram Language kernel to generate code at build time and a C++ compiler to compile an executable.

AST uses C++11 features and requires a compiler that can support at least C++11.

AST uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build AST:
```
cd ast
mkdir build
cd build
cmake ..
cmake --build . --target paclet
```

The result is a directory named `paclet` that contains the WL package source code and a built AST `.paclet` file for installing.

You may see an error because the default paths to `WolframKernel`, `MathLink`, or `WolframLibrary` may not be correct.

Here is the cmake command using supplied values for `WOLFRAMKERNEL`, `MATHLINK_LIB_DIR`, `MATHLINK_INCLUDE_DIR`, and `WOLFRAMLIBRARY_INCLUDE_DIR`:
```
cmake -DWOLFRAMKERNEL=/path/to/WolframKernel -DMATHLINK_LIB_DIR=/path/to/mathlink/lib/dir -DMATHLINK_INCLUDE_DIR=/path/to/mathlink/include/dir -DWOLFRAMLIBRARY_INCLUDE_DIR=/path/to/wolfram/library/dir ..
```

Here are typical values for the variables:
* `WOLFRAMKERNEL` `/Applications/Mathematica.app/Contents/MacOS/WolframKernel`
* `MATHLINK_LIB_DIR` `/Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions`
* `MATHLINK_INCLUDE_DIR` `/Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions`
* `WOLFRAMLIBRARY_INCLUDE_DIR` `/Applications/Mathematica.app/Contents/SystemFiles/IncludeFiles/C`

Here is the build directory layout after building AST:

```
paclet/
  AST/
    Kernel/
      AST.wl
    LibraryResources/
      <<SystemID>>
        AST.dll
    PacletInfo.m
    ...
```

### Windows

It is recommended to specify `wolfram.exe` instead of `WolframKernel.exe`.

`WolframKernel.exe` opens a new window while it is running. But `wolfram.exe` runs inside the window that started it.

Building with Mathematica 11.0 or 11.1: use VS2015, C runtime issue?


## Using AST

After AST is built and installed, it can be used.

```
In[1]:= Needs["AST`"]

In[2]:= ParseString["1+1"]

Out[2]= InfixNode[Plus, {IntegerNode[1, {}, <|Source -> {{1, 1}, {1, 1}}|>],

>     IntegerNode[1, {}, <|Source -> {{1, 3}, {1, 3}}|>]}, <|Source -> {{1, 1}, {1, 3}}|>]

In[3]:=
```

A `wl-ast` command-line tool is also built and can be used.

```
$wl-ast
>>> 1+1
InfixNode[Plus, {IntegerNode["1", {}, <|Source->{{1, 1}, {1, 1}}|>], IntegerNode["1", {}, <|Source->{{1, 3}, {1, 3}}|>]}, <|Source->{{1, 1}, {1, 3}}|>]

>>>
```


## Troubleshooting

### Strange behavior on Windows

After installing the AST paclet on Windows, you may see strange behavior such as the kernel and front end crashing or the `wl-ast` executable disppearing.

This is most likely due to Antivirus software such as Bitdefender incorrectly flagging the `wl-ast.exe` executable as a virus.

Make sure to whitelist `wl-ast.exe` to prevent Antivirus software from flagging this paclet as a virus.

<!--- bug 236253 --->
