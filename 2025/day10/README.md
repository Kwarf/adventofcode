# --- Day 10: Factory ---

Buttons go click click.

All credits for the solution of part 2 goes to tenthmascot on Reddit.

When I was looking for potential ways to solve the second part, everything I
found pointed to linear algebra, ILP solvers, and other algebraic things that I
honestly don't have the education to understand. I don't want to just pull in
some library to do all the work for me, and I don't want to LLM my way to a
solution that I don't fully understand either, because what would be the point
of that? So I had basically given up on solving this, until I reached day 12
part 2, with the only _fun_ requirement was that you had to have solved all
other days. So I went back and did some more digging, and that's where I found
[this great
explanation](https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/)
of a way to solve it without _any_ algebra! So I wrote an implementation of
that, which I actually understand, and I can be fine with that, even though I
did not come up with the actual idea myself.

```
% cargo run --release -q
The answer to the first part is: 461
The answer to the second part is: 16315
```
