

concretifying is:
given abstract syntax
choosing operators +
    parenthesizing where needed +
    (removing implicit tokens where possible) +
    (stringifying where possible) +
    (compounding where possible) +
    (crazy stuff like convert `-1*a` to `-a`)
make different choices about operators:
e.g. CompoundExpression[] or ; ?
f[x] or f@x ?


pretty-printing is:
given abstract syntax
concretifying[operators that look nice] then formatting

this is better InputForm


minifying is:
given abstract syntax
concretifying[operators that minimize space], no formatting








