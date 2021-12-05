from collections import defaultdict

part1 = defaultdict(int)
part2 = defaultdict(int)

def step(d): return 1 if d > 0 else -1 if d < 0 else 0

for line in open("input.txt"):
    [x1, y1, x2, y2] = map(int, line.replace(" -> ", ",").split(","))
    dx = x2 - x1
    dy = y2 - y1
    for i in range(max(abs(dx), abs(dy)) + 1):
        x = x1 + step(dx) * i
        y = y1 + step(dy) * i
        if dx == 0 or dy == 0:
            part1[(x, y)] += 1
        part2[(x, y)] += 1

print("The answer to the first part is: " +
      str(len([x for x in part1 if part1[x] > 1])))
print("The answer to the second part is: " +
      str(len([x for x in part2 if part2[x] > 1])))
