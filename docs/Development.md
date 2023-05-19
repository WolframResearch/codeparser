# Development

## Quick Command Reference

#### Build the CodeParser paclet:

```shell
$ cmake -S . -B build -DMATHEMATICA_INSTALL_DIR=/Applications/Wolfram/Mathematica-13.1.0.app/Contents/
$ cmake --build build
```

Intermediate compiled library artifacts will be built into the `./crate/target`
directory, and a copy will be placed in the `CodeParser/LibraryResources/`
subdirectory of the built CodeParser paclet.

#### Run the compiled tests:

Run the compiled Rust library tests using `cargo`:

```shell
$ cd crate
$ cargo test
```

#### Run the Wolfram tests:

After building CodeParser, tests written in Wolfram can be run from the command
line using the
[`wolfram-cli paclet test`](https://github.com/ConnorGray/wolfram-cli) tool:

```shell
$ wolfram-cli paclet test build/paclet/CodeParser Tests/TestSuite.mt
```

#### Build the Wolfram-compatible dynamic library manually

To build a dynamic library suitable for use via LibraryLink manually, run:

```shell
$ cargo build --features=USE_MATHLINK,CHECK_ABORT
```

> Note: The `USE_MATHLINK` and `CHECK_ABORT` features are opt-in because they
> enable functionality that only works when the resulting library is loaded via
> LibraryLink, which isn't the case when e.g. running tests via `cargo test`,
> or when the `wolfram-code-parser` is used as a dependency from other Rust
> crates.

## Testing

CodeParser has two test suites:

1. Tests written in Rust, primarily located in [crate/src/tests/](../crate/src/tests/).
2. Tests written in Wolfram, primarily located in [Tests](../Tests/).



