# CodeParser

CodeParser is a package for parsing Wolfram Language code.

```
In[1]:= Needs["CodeParser`"]

In[2]:= CodeParse["1+1"]

Out[2]= InfixNode[Plus, {LeafNode[Integer, 1, <|Source -> {{1, 1}, {1, 2}}|>],

>     LeafNode[Integer, 1, <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]

In[3]:=
```

`CodeParser` is used by the `CodeTools` suite of packages for inspecting code, formatting code, and instrumenting code for coverage reporting and profiling.


[Parsing the Wolfram Language from WTC 2019: Watch Video](https://www.wolfram.com/broadcast/video.php?v=2908)

[Parsing the Wolfram Language from WTC 2019: Download Presentation](https://files.wolframcdn.com/pub/www.wolfram.com/technology-conference/2019/Thursday/2019BrentonBostickParsingTheWL.nb)


## Setup

Make sure that the paclet can be found on your system:
```
In[1]:= Needs["CodeParser`"]
```

[CodeParser on github.com](https://github.com/xxx)

Install CodeParser from the CodeTools paclet server:
```
In[1]:= PacletUpdate["CodeParser", "Site" -> "XXX", "UpdateSites" -> True]

Out[1]= PacletObject[CodeParser, 1.0, <>]
```


## Building

CodeParser uses a Wolfram Language kernel to generate code at build time and a C++ compiler to compile an executable.

CodeParser uses C++11 features and requires a compiler that can support at least C++11.

CodeParser uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build CodeParser:
```
cd codeparser
mkdir build
cd build
cmake ..
cmake --build . --target paclet
```
The result is a directory named `paclet` that contains the WL package source code and a built CodeParser `.paclet` file for installing.

Specify `INSTALLATION_DIRECTORY` if you have Mathematica installed in a non-default location:
```
cmake -DINSTALLATION_DIRECTORY=/Applications/Mathematica111.app/Contents/ ..
cmake --build . --target paclet
```


## Using CodeParser

After CodeParser is built and installed, it can be used.

```
In[1]:= Needs["CodeParser`"]

In[2]:= CodeParse["1+1"]

Out[2]= InfixNode[Plus, {LeafNode[Integer, 1, <|Source -> {{1, 1}, {1, 2}}|>],

>     LeafNode[Integer, 1, <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]

In[3]:=
```

A `codeparser` command-line tool is also built and can be used.

```
cmake -DBUILD_EXE=ON ..
cmake --build . --target codeparser-exe

$cpp/src/exe/codeparser
>>> 1+1
InfixNode[Plus, {LeafNode[Integer, "1", <|Source->{{1, 2}, {1, 2}}|>], LeafNode[Integer, 1, <|Source->{{1, 3}, {1, 4}}|>]}, <|Source->{{1, 1}, {1, 4}}|>]

>>>
```
