# --- Day 6: Probably a Fire Hazard ---
I decided to use C++11 for this problem, because I felt that using `std::bitset`
for the lights would be nice, since they're only on or off. Having fixed bounds
on the grid size also meant that I could use `std::array` for some increased
performance.

However _Part Two_ came, and like a slap in the face the lights could now be
dimmed, meaning that I had to use integers to store each light. I still wanted
to keep my bitset solution, so I split this problem into two separate source
files/executables.

The executables expects _one_ argument, a path to the puzzle input, it is
provided as `input.txt`. This is passed for you when you run `make run`, both
executables will be compiled and run.

Requires `g++` to compile.
