# CodeParser

CodeParser is a package for parsing Wolfram Language source code as abstract syntax trees (ASTs) or concrete syntax trees (CSTs).
CodeParser is useful for inspecting code, formatting code, and instrumenting code (for e.g., coverage reporting or profiling), and much more!

CodeParser has many key features:
* Understands practically entire Wolfram Language syntax.
* Fast native library implementation.
* Tested with combination of suite of hand-written tests and fuzz testing.
* Gracious error handling and recovery


```
Needs["CodeParser`"]

CodeParse["1+1"]
```
```
Out[2]= ContainerNode[String, {CallNode[LeafNode[Symbol, "Plus", <||>], {LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>], LeafNode[Integer, "1", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <||>]
```

["CodeParser and CodeInspector" on community.wolfram.com](https://community.wolfram.com/groups/-/m/t/1931315)

[Parsing the Wolfram Language from WTC 2019: Watch Video (youtube)](https://www.youtube.com/watch?v=rOa5IntICFA)

[Parsing the Wolfram Language from WTC 2019: Watch Video (wolfram.com)](https://www.wolfram.com/broadcast/video.php?v=2908)

[Parsing the Wolfram Language from WTC 2019: Download Presentation](https://files.wolframcdn.com/pub/www.wolfram.com/technology-conference/2019/Thursday/2019BrentonBostickParsingTheWL.nb)


## Setup

CodeParser is included in Mathematica 12.2 and above.

For older versions, install CodeParser paclet from the public paclet server:
```
PacletInstall["CodeParser"]
```

[Build and install the CodeParser paclet locally](HowToBuild.md)


## Using CodeParser

After CodeParser is installed, it can be used.

```
Needs["CodeParser`"]

CodeParse["1+1"]
```
```
Out[2]= ContainerNode[String, {CallNode[LeafNode[Symbol, "Plus", <||>], {LeafNode[Integer, "1", <|Source -> {{1, 1}, {1, 2}}|>], LeafNode[Integer, "1", <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]}, <||>]
```

The input to `CodeParse` may be a string, a `File`, or a list of bytes.


### Command-line tool (Optional)

An optional `codeparser` command-line tool is also built and can be used.

```
cmake -DBUILD_EXE=ON ..
cmake --build . --target codeparser-exe

$cpp/src/exe/codeparser
>>> 1+1
InfixNode[Plus, {LeafNode[Integer, "1", <|Source->{{1, 2}, {1, 2}}|>], LeafNode[Integer, 1, <|Source->{{1, 3}, {1, 4}}|>]}, <|Source->{{1, 1}, {1, 4}}|>]

>>>
```


## Troubleshooting

Make sure that the CodeParser can be found on your system:
```
Needs["CodeParser`"]
```

and try a basic example:
```
CodeParse["1+1"]
```

You may get `LibraryFunction` messages:
```
In[1]:= Needs["CodeParser`"]

In[2]:= CodeParse["1+1"]

During evaluation of In[2]:= LibraryFunction::version: The version number 7 of the library is not consistent with the current or any previous WolframLibraryVersion.

During evaluation of In[2]:= LibraryFunction::initerr: A nonzero error code 7 was returned during the initialization of the library /Users/user/Library/Mathematica/Paclets/Repository/CodeParser-1.4/LibraryResources/MacOSX-x86-64/CodeParser.dylib.

During evaluation of In[2]:= LibraryFunction::libload: The function ConcreteParseBytes_Listable_LibraryLink was not loaded from the file /Users/user/Library/Mathematica/Paclets/Repository/CodeParser-1.4/LibraryResources/MacOSX-x86-64/CodeParser.dylib.

Out[2]= $Failed
```

This means that CodeParser was built with a newer version of Wolfram System than your system supports.

To fix this, build CodeParser from source with the version of Wolfram System that you will use.
