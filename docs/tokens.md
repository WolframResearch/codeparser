
# Tokens


## Terminology


trivia: whitespace, newlines, comments, line continuations [1]



## Philosophy

if a token is not something else, then it is prefix



## TokenEnum encoding

 There are currently ~429 tokens, so 9 bits are required to enumerate them

 16 bits:

```
 fedcba9876543210
        ^~~~~~~~~
        Enum bits (9 bits)
     ^~~
     Group 1 bits (3 bits)
   ^~
   Group 2 bits (2 bits)
 ^~
 Unused bits (2 bits)
```

 Group 1: These are all mutually exclusive categories: PossibleBeginning, Closer, Error, Trivia, Infix
 001 PossibleBeginning
 010 Closer
 011 Error
 100 Trivia
 101 Infix
 110 (unused)
 111 (unused)
 000 Anything Else



 Group 2: These are all mutually exclusive categories: Empty, DifferentialD
 01 Empty
 10 DifferentialD
 11 (unused)
 00 Anything Else





## References

[1] https://github.com/dotnet/roslyn/wiki/Roslyn-Overview#syntax-trivia








