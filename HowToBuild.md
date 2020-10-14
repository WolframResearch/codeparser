# Building

CodeParser uses a Wolfram Language kernel to generate code at build time and a C++ compiler to compile a native library.

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

Inside a kernel session you may then install the paclet by evaluating:
```
PacletInstall["/path/to/build/paclet/CodeParser-1.0.paclet"]
```

Specify `MATHEMATICA_INSTALL_DIR` if you have Mathematica installed in a non-default location:

```
cmake -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica121.app/Contents/ ..
cmake --build . --target paclet
```

On Windows:

```
cmake -DMATHEMATICA_INSTALL_DIR="C:/Program Files/Wolfram Research/Mathematica/12.1" ..
cmake --build . --target paclet
```
