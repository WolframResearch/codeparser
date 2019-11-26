
# Characters


## Philosophy

if a character is not something else, then it is letterlike



## Character Encodings

TODO



## Raw

characters like \[RawReturn] are a way of escaping that character

Poorly understood

Perhaps essentially unused



A good philosophy that I follow is to treat the Raw characters as escaped versions of their normal characters

\[RawTab] is similar to \t, and is NOT the same as actual 0x09 character
\[NewLine] is similar to \n, and is NOT the same as actual 0x0a character
etc.



## SourceCharacter encoding

 32 bits:
```
 vutsrqponmlkjihgfedcba9876543210
            ^~~~~~~~~~~~~~~~~~~~~
            Character bits (21 bits)
           ^
           Sign bit
        ^~~
        Count bits (3 bits)
 ^~~~~~~
 Unused (7 bits)
```

Count bits: encodes the number of bytes used by this SourceCharacter
000: (unused)
001: 1 byte
010: 2 bytes
011: 3 bytes
100: 4 bytes
101: (unused)
110: (unused)
111: (unused)





## WLCharacter encoding

 32 bits:
```
 vutsrqponmlkjihgfedcba9876543210
            ^~~~~~~~~~~~~~~~~~~~~
            Character bits (21 bits)
           ^
           Sign bit
        ^~~
        EscapeStyle bits (3 bits)
 ^~~~~~~
 Unused (7 bits)
```




