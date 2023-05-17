# Building

CodeParser uses a Wolfram Language kernel to generate code at build time and a C++ compiler to compile a native library.

CodeParser uses C++17 features and requires a compiler that can support at least C++17.

CodeParser uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build CodeParser:
```
cd codeparser
mkdir build
cd build
cmake ..
cmake --build .
```

The result is a directory named `paclet` that contains the WL package source code and a built CodeParser `.paclet` file for installing.

Inside a kernel session you may then install the paclet by evaluating:
```
PacletInstall["/path/to/build/paclet/CodeParser-1.10.paclet"]
```

Specify `MATHEMATICA_INSTALL_DIR` if you have Wolfram System installed in a non-default location:
```
cmake -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica.app/Contents/ ..
cmake --build .
```

On Windows:
```
cmake -DMATHEMATICA_INSTALL_DIR="C:/Program Files/Wolfram Research/Mathematica/13.1" ..
cmake --build .
```

## Installing

You can install the paclet from CMake:
```
cmake --install .
```

This starts a kernel and calls `PacletInstall` with the built .paclet file.


## Troubleshooting

### "building for macOS-arm64 but attempting to link with file built for macOS-x86_64"

You see this error during building:
```
ld: warning: ignoring file /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions/mathlink.framework/mathlink, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
```

You most likely need to specify:
```
-DCMAKE_OSX_ARCHITECTURES=x86_64
```
