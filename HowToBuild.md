
# Building AST

AST uses a WL kernel to generate code at build time and a C++ compiler to generate an executable.

AST uses C++11 features and requires a compiler that can support at least C++11.

AST uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build AST:

```
cd ast
mkdir build
cd build
cmake -DWOLFRAMKERNEL=/path/to/wolfram ..
cmake --build . --target paclet
```

The result is a directory named paclet that contains the WL package source code and a built AST .paclet file for installing.

Here is the build directory layout after building AST:

```
paclet/
  AST/
    ASTResources/
      <<SystemID>>
        wl-ast
    AST.wl
    PacletInfo.m
    ...
```




### Windows


It is recommended to specify wolfram.exe instead of WolframKernel.exe.

WolframKernel.exe opens a new window while it is running. But wolfram.exe runs inside the window that started it.


