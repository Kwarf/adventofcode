# --- Day 2: I Was Told There Would Be No Math ---
For this challenge I decided to use C. I'm using vector multiplication when
calculating the box size, this is the first time I've used SIMD.

It expects _one_ argument, a path to the puzzle input, it is provided as
`input.txt` and is passed by the Makefile, just run `make run`. The answer to
both parts of the puzzle will be written to stdout using `printf`.

Requires a 64-bit Linux machine with GCC to build.