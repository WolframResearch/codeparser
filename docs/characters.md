
# Characters


## Philosophy

if a character is not something else, then it is letterlike



## Character Encodings


UTF-8 input is assumed everywhere.

There is an API function SafeString that will accept an array of bytes and return a "safe" string, i.e., a string that has assumed UTF-8 input with these changes:

Any invalid byte sequences are converted into \[UnknownGlyph]

Any high or low surrogates are converted into \[UnknownGlyph]

BOM character 0xfeff is converted into 0xe001, to allow transferring through MathLink.
Related bugs: 366106






## Raw

characters like \[RawReturn] are a way of escaping that character

Poorly understood

Perhaps essentially unused



A good philosophy that I follow is to treat the Raw characters as escaped versions of their normal characters

\[RawTab] is similar to \t, and is NOT the same as actual 0x09 character
\[NewLine] is similar to \n, and is NOT the same as actual 0x0a character
etc.








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



## Private Use Area

No attempt will be made to define or describe characters in the PUA.

The FE defines a number of PUA characters for its own internal use.

This is not a binding contract and usage, values, behavior, and stability is subject to change at any moment.



