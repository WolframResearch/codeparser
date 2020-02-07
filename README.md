# CodeParser

`CodeParser` is a paclet for parsing Wolfram Language code.

```
In[1]:= Needs["CodeParser`"]

In[2]:= CodeParse["1+1"]

Out[2]= InfixNode[Plus, {LeafNode[Integer, 1, <|Source -> {{1, 1}, {1, 2}}|>],

>     LeafNode[Integer, 1, <|Source -> {{1, 3}, {1, 4}}|>]}, <|Source -> {{1, 1}, {1, 4}}|>]

In[3]:=
```

`CodeParser` provides *symbolic* results.

XX



[Parsing the Wolfram Language from WTC 2019: Watch Video](https://www.wolfram.com/broadcast/video.php?v=2908)

[Parsing the Wolfram Language from WTC 2019: Download Presentation](https://files.wolframcdn.com/pub/www.wolfram.com/technology-conference/2019/Thursday/2019BrentonBostickParsingTheWL.nb)


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


## Troubleshooting

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

