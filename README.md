# CodeParser-rs Quick Start Instructions

These are very quick instructions on how to try out the Rust CodeParser port.

The new Rust code is located in the [`./crate/`](./crate/) subdirectory of the
project. As each file was ported, I deleted it from the [`./cpp/`](./cpp)
directory; so the `cpp` directory contains the only files that haven't been
ported.

The ported code includes all of the lib .cpp files, as well as the testing .cpp
files.

Installation instructions for Rust itself
[can be found at rust-lang.org](https://www.rust-lang.org/tools/install).

The Rust port functions as a drop-in replacement for the C++ version:

* 100% of the .mt tests pass when run against the Rust dynamic library
* 100% of the C++ compiled tests were ported to Rust and all pass
* No changes to the `CodeParser/**.wl` or `*.mt` files were needed (other than the
  `Generate/*.wl`, which directly generate compiled code).

## Building

You can invoke CMake in the normal way:

```shell
cmake -S . -B build -DMATHEMATICA_INSTALL_DIR=/Applications/Wolfram/Mathematica-13.1.0.app/Contents/
```

Then build in the normal way. This will run the code generation scripts, which
have been updated to produce Rust instead of C++.

```shell
$ cmake --build build
```

CMake will automatically use [`cargo`](https://doc.rust-lang.org/cargo/) to
build the Rust code.

The built Rust artifacts will be placed in the `./crate/target/` directory.

CMakeLists.txt has been updated to copy the and rename the compiled dynamic
library out of the `./crate/target/` directory and into the appropriate built
paclet `LibraryResources` subdirectory.

The built paclet can be loaded and installed the same way as before the port.

### Build the .dylib manually

To build a dynamic library suitable for use via LibraryLink manually, run:

```shell
$ cargo build --features=USE_MATHLINK,CHECK_ABORT
```

> Note: The `USE_MATHLINK` and `CHECK_ABORT` features are opt-in because they
> enable functionality that only works when the resulting library is loaded via
> LibraryLink, which isn't the case when e.g. running tests via `cargo test`,
> or when the `wolfram-code-parser` is used as a dependency from other Rust
> crates.

## Run the compiled tests

Rust's `cargo` build tool has support for running tests built in, so the test
suite can be run by doing:

```shell
$ cd crate
$ cargo test
```

#### Run the Wolfram Language tests

The Wolfram Language tests can be run from the command line using the
[`wolfram-cli paclet test`](https://github.com/ConnorGray/wolfram-cli) tool:

```
$ wolfram-cli paclet test build/paclet/CodeParser Tests/TestSuite.mt
```


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

During evaluation of In[2]:= LibraryFunction::initerr: A nonzero error code 7 was returned during the initialization of the library /Users/user/Library/Mathematica/Paclets/Repository/CodeParser-1.6/LibraryResources/MacOSX-x86-64/CodeParser.dylib.

During evaluation of In[2]:= LibraryFunction::libload: The function ConcreteParseBytes_Listable_LibraryLink was not loaded from the file /Users/user/Library/Mathematica/Paclets/Repository/CodeParser-1.6/LibraryResources/MacOSX-x86-64/CodeParser.dylib.

Out[2]= $Failed
```

This means that CodeParser was built with a newer version of Wolfram System than your system supports.

To fix this, build CodeParser from source with the version of Wolfram System that you will use.

## Benchmarks

> Some of the benchmarks test large data files. Those files are tracked in this
> repository to ensure that benchmarks are always run against identical input.
> [Git LFS](https://git-lfs.github.com/) is used to ensure that a basic checkout
> of this repository remains small, which is important in CI/CD builds.

To run the benchmarks, first ensure that the large benchmark files have been
checked out locally using:

```shell
$ git lfs pull --exclude="" --include="*"
```

This will override the default settings in [`.lfsconfig`](./.lfsconfig).

## File Overview

* [Tests/files/large/](./Tests/files/large/) contains files
  managed by [`Git LFS`](https://git-lfs.github.com/). The files in this
  directory are used by the benchmarks. These files should never be modified, to
  ensure that benchmark comparisions between different revisions of this
  repository can be meaningfully compared.