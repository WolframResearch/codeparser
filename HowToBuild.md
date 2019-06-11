
# Building AST

AST uses a WL kernel to generate code at build time and a C++ compiler to compile an executable.

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

The result is a directory named paclet that contains the WL package source code and a built AST .paclet file for installing.

You may see an error because the default paths to WolframKernel, MathLink, or WolframLibrary may not be correct.

Here is the cmake command using supplied values for WOLFRAMKERNEL, MATHLINK_LIB_DIR, MATHLINK_INCLUDE_DIR, and WOLFRAMLIBRARY_INCLUDE_DIR.

```
cmake -DWOLFRAMKERNEL=/path/to/WolframKernel -DMATHLINK_LIB_DIR=/path/to/mathlink/lib/dir -DMATHLINK_INCLUDE_DIR=/path/to/mathlink/include/dir -DWOLFRAMLIBRARY_INCLUDE_DIR=/path/to/wolfram/library/dir ..
```

Here are typical values for the variables:
`WOLFRAMKERNEL` `/Applications/Mathematica.app/Contents/MacOS/WolframKernel`
`MATHLINK_LIB_DIR` `/Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions`
`MATHLINK_INCLUDE_DIR` `/Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions`
`WOLFRAMLIBRARY_INCLUDE_DIR` `/Applications/Mathematica.app/Contents/SystemFiles/IncludeFiles/C`



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


It is recommended to specify wolfram.exe instead of WolframKernel.exe.

WolframKernel.exe opens a new window while it is running. But wolfram.exe runs inside the window that started it.


Building with Mathematica 11.0 or 11.1: use VS2015, C runtime issue?

