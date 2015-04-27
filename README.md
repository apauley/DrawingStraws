# Drawing Straws in Haskell

This command-line program simulates the [drawing of straws][wikistraws] over a series of random draws,
and counts how many times each position drew the short straw.

Each position should have an equal probability to draw the short straw.

## Usage

The DrawingStraws executable takes 2 parameters:

 1. The number of straws in each bunch of straws.
 2. How many random draws we should count.

```bash
$ .cabal-sandbox/bin/DrawingStraws 5 5000000
Counting short straw occurrences over 5000000 draws, with 5 straws in each bunch.

Performed 5000000 draws.
Counting position occurrences...

Short straw position counts:
1:	999978
2:	999848
3:	999511
4:	999367
5:	1001296

Total: 5000000
```

## Building

```bash
$ cabal install -j
```

The binary is placed in this repository checkout under *.cabal-sandbox/bin/DrawingStraws*

[wikistraws]: https://en.wikipedia.org/wiki/Drawing_straws