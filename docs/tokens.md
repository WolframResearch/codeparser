
# Tokens


## Terminology


trivia: whitespace, newlines, comments, line continuations [1]



## Philosophy

if a token is not something else, then it is prefix



## TokenEnum encoding

 There are currently ~422 tokens, so 9 bits are required to enumerate them

 16 bits:

```
 fedcba9876543210
        ^~~~~~~~~
        Enum bits (9 bits)
     ^~~
     Group 1 bits (3 bits)
    ^
    Infix bit
   ^
   Empty bit
  ^
  DifferentialD bit
 ^
 Unused bit
```

 Group 1: These are all mutually exclusive categories: Possible, Closer, Error, Trivia, Inequality, VectorInequality
 000 Possible
 001 Closer
 010 Error
 011 Trivia
 100 Inequality
 101 VectorInequality
 110 (unused)
 111 Anything Else





## References

[1] https://github.com/dotnet/roslyn/wiki/Roslyn-Overview#syntax-trivia








