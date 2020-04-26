
# Tokens


## Terminology


trivia: whitespace, newlines, comments [1]



## Philosophy

if a token is not something else, then it is prefix



## TokenEnum encoding

 There are currently ~437 tokens, so 9 bits are required to enumerate them

 16 bits:

```
 fedcba9876543210
        ^~~~~~~~~
        Enum bits (9 bits)
      ^~
      Group 1 bits (2 bits)
    ^~
    Group 2 bits (2 bits)
 ^~~
 Unused bits (3 bits)
```


Within the set of trivia tokens, the values of the enum bits themselves are special because they are used for fast
testing.


Group 1: These are all mutually exclusive categories: PossibleBeginning, Closer, Error
01 PossibleBeginning
10 Closer
11 Error
00 Anything Else

Other possible categories for Group 1 are: Trivia, InfixOperator, etc. Everything in Group 1 would still
be mutually exclusive.



Group 2: These are all mutually exclusive categories: Empty, DifferentialD
01 Empty
10 DifferentialD
11 (unused)
00 Anything Else





## References

[1] https://github.com/dotnet/roslyn/wiki/Roslyn-Overview#syntax-trivia








