
# Fuzz testing with AFL++

## Building

```
cmake -DUSE_MATHLINK=OFF -DBUILD_EXE=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=afl-clang-fast  -DCMAKE_CXX_COMPILER=afl-clang-fast++ -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica130.app/Contents ..

cmake --build . --target codeparser-exe
```

## Running

```
afl-fuzz -i /Users/brenton/development/stash/COD/codeparser/Tests/files/small -o afl_out/ -x ../Tests/wl.dict -D -- cpp/src/exe/codeparser -file @@
```
