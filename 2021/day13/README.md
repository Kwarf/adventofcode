# --- Day 13: Transparent Origami ---

The first one where I had to implement proper debug printout in order
to understand what happened.

Part two was a real mess because I initially made the assumption that
I didn't have to store the paper size. That it could be derived from
the largest X/Y points at any state. This is incorrect and resulted
in broken folds.

There's lots of code duplication, but I really don't want to look at
any of this any longer now.

```
% cabal run -v0
The answer to the first part is: 678
The answer to the second part is:
####..##..####.#..#.#....#..#.####.####
#....#..#.#....#..#.#....#..#....#.#...
###..#....###..####.#....####...#..###.
#....#....#....#..#.#....#..#..#...#...
#....#..#.#....#..#.#....#..#.#....#...
####..##..#....#..#.####.#..#.####.#
```
