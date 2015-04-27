# Drawing Straws in Haskell

This command-line program simulates the [drawing of straws][wikistraws] over a series of random draws,
and counts how many times each position drew the short straw.

Each position should have an equal probability to draw the short straw.

## Usage

The DrawingStraws executable takes 2 parameters:

 1. The number of straws in each bunch of straws.
 2. How many random draws we should count.

```bash
$ .cabal-sandbox/bin/DrawingStraws 10 1000000
Counting short straw occurrences over 1000000 draws, with 10 straws in each bunch.

Short straw position counts:
1:	100179
2:	99487
3:	99837
4:	100021
5:	100044
6:	100124
7:	99939
8:	100383
9:	100160
10:	99826

Total: 1000000
```

## Building

```bash
$ cabal install -j
```

The binary is placed in this repository checkout under *.cabal-sandbox/bin/DrawingStraws*

[wikistraws]: https://en.wikipedia.org/wiki/Drawing_straws