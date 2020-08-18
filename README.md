# Lisp Scheme interpreter

`Scheme` interpreter implemented in `Haskell`, interpreting a large subset of R5RS. 

Largely based on the book, [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

## To run
```bash
ghc --make scmi.hs
./scmi "stdlib.scm"
```
And you'll have yourself a `Scheme` interpreter!

## Files
`Haskell` source code is in the`scmi.hs` file.
The standard library is written in `Scheme`, and in `stdlib.scm`.
