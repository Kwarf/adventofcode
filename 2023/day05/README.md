# --- Day 5: If You Give A Seed A Fertilizer ---

As expected I ran into time complexity problems for part 2, where it would finish in about 3 min 8 seconds, but I'm
solving late today, so instead of properly figuring out a solution I just pulled in `rayon` and threw more threads at
it. Now it finishes in "only" 43 seconds.

```
% cargo run --release -q
The answer to the first part is: 382895070
The answer to the second part is: 17729182
```
