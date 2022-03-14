
# Fuzz testing with AFL++

https://aflplus.plus/



## Building AFL++


### prerequisite: coreutils is installed

the install of AFL++ assumes to be using `install` command from coreutils


this is the bad `install`:
```
brenton@brenton2maclap github-development % which install
/usr/bin/install
```

```
brew install coreutils
```

you will see:
```
Commands also provided by macOS and the commands dir, dircolors, vdir have been installed with the prefix "g".
If you need to use these commands with their normal names, you can add a "gnubin" directory to your PATH with:
  PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
```

do what it says:
```
brenton@brenton2maclap github-development % export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
```

this is the good `install`:
```
brenton@brenton2maclap github-development % which install                                              
/usr/local/opt/coreutils/libexec/gnubin/install
```


### prerequisite: LLVM clang is installed

AFL++ assumes to be using LLVM clang and NOT Apple clang

this is the bad clang:
```
brenton@brenton2maclap AFLplusplus % which clang
/usr/bin/clang
```

```
brew install llvm
```

you will see:
```
If you need to have llvm first in your PATH, run:
  echo 'export PATH="/usr/local/opt/llvm/bin:$PATH"' >> ~/.zshrc
```

do what it says:
```
export PATH="/usr/local/opt/llvm/bin:$PATH"
```

this is the good `clang`:
```
brenton@brenton2maclap AFLplusplus % which clang                                
/usr/local/opt/llvm/bin/clang
```


### building AFL++

https://aflplus.plus/building/


```
git clone https://github.com/AFLplusplus/AFLplusplus

cd AFLplusplus

make clean

make distrib

sudo make install
```

Verify afl-fuzz is installed:
```
brenton@brenton2maclap build-afl % which afl-fuzz                                                                                                                                     
/usr/local/bin/afl-fuzz
```


## Building CodeParser

```
mkdir build-afl

cd build-afl

cmake -DUSE_MATHLINK=OFF -DBUILD_EXE=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=afl-clang-fast  -DCMAKE_CXX_COMPILER=afl-clang-fast++ -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica130.app/Contents ..

cmake --build . --target codeparser-exe
```


## Running CodeParser with AFL++

```
cd build-afl

rm -rf afl_out

afl-fuzz -i /Users/brenton/development/stash/COD/codeparser/Tests/files/small -o afl_out/ -x ../Tests/wl.dict -D -- cpp/src/exe/codeparser -file @@
```


## Troubleshooting

Might get this:
```
[-]  SYSTEM ERROR : shmget() failed, try running afl-system-config
    Stop location : afl_shm_init(), src/afl-sharedmem.c:252
       OS message : Invalid argument
```

do what it says and run:
```
sudo afl-system-config
```











