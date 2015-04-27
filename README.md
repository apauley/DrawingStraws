# Drawing Straws in Haskell

This command-line program simulates the [drawing of straws][wikistraws] over a series of random draws,
and counts how many times each position drew the short straw.

Each position should have an equal probability to draw the short straw.

## Usage

The DrawingStraws executable takes 2 parameters:

 1. The number of straws in each bunch of straws.
 2. How many random draws we should count.

```
$ .cabal-sandbox/bin/DrawingStraws 5 5000000
2015-04-27 19:42:06.782093 UTC | Counting short straw occurrences over 5000000 draws, with 5 straws in each bunch.

2015-04-27 19:42:06.783498 UTC | Performed 5000000 draws.
2015-04-27 19:42:08.207945 UTC | Counting position occurrences...

Short straw position counts:
1:	1000379
2:	999752
3:	1002413
4:	999431
5:	998025

Total: 5000000

2015-04-27 19:42:17.242081 UTC | Done.
```

## Building

```bash
$ cabal install -j
```

The binary is placed in this repository checkout under *.cabal-sandbox/bin/DrawingStraws*

[wikistraws]: https://en.wikipedia.org/wiki/Drawing_straws