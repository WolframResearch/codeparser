# Development

## Quick Command Reference

#### Build the CodeParser paclet:

```shell
$ cmake -S . -B build -DMATHEMATICA_INSTALL_DIR=/Applications/Wolfram/Mathematica-13.1.0.app/Contents/
$ cmake --build build
```

Intermediate compiled library artifacts will be built into the `./target`
directory, and a copy will be placed in the `CodeParser/LibraryResources/`
subdirectory of the built CodeParser paclet.

#### Run the compiled tests:

Run the compiled Rust library tests using `cargo`:

```shell
$ cargo test
```

#### Run the Wolfram tests:

After building CodeParser, tests written in Wolfram can be run from the command
line using the
[`wolfram-cli paclet test`](https://github.com/ConnorGray/wolfram-cli) tool:

```shell
$ wolfram-cli paclet test build/paclet/CodeParser Tests/TestSuite.mt
```

## Testing

CodeParser has two test suites:

1. Tests written in Rust, primarily located in [crates/wolfram-parser/src/tests/](../crates/wolfram-parser/src/tests/).
2. Tests written in Wolfram, primarily located in [Tests](../Tests/).



