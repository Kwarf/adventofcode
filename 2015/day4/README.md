# --- Day 4: The Ideal Stocking Stuffer ---
This problem was pretty interesting since it seemed easy to parallelize. I used
C++11 since it comes with built-in threading support that I've never really
tried using. I looked at using OpenCL so I could utilize the GPU, but the
learning curve seemed a bit steep, I don't want to spend too much time on each
problem.

It expects _two_ arguments, the number of zeroes that the hash has to start
with, and the secret key, both are provided and passed by the Makefile, just run
`make run`. The program will run twice, once for each part of the problem (5 or
6 zeros).

The first part (5 zeros on ckczppom) takes 0.112279408 seconds to finish on my
i7-4710MQ, measured using `perf stat`. The second part (6 zeros on the same key)
takes 3.678525108 seconds.

Requires `g++` and `openssl` to build.
