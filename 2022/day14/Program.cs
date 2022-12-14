IEnumerable<int> Range(int from, int to) => Enumerable.Range(0, Math.Abs(to - from) + 1).Select(x => from + (x * Math.Sign(to - from)));

IEnumerable<(int, int)> Interpolate((int, int) from, (int, int) to) => (from, to) switch
{
    ((var x1, var y1), (var x2, var y2)) when x1 != x2 && y1 == y2 => Range(x1, x2).Select(x => (x, y1)),
    ((var x1, var y1), (var x2, var y2)) when x1 == x2 && y1 != y2 => Range(y1, y2).Select(y => (x1, y)),
    _ => new[] {from, to},
};

IEnumerable<(int X, int Y)> ParseLine(string line) => line
    .Split(new[] { ",", " -> " }, StringSplitOptions.RemoveEmptyEntries)
    .Select(Int32.Parse)
    .Chunk(2)
    .Select(x => (x[0], x[1]))
    .Aggregate(new List<(int, int)>(), ((acc, x) => acc.Count switch
    {
        0 => new List<(int, int)> { x },
        _ => acc.Concat(Interpolate(acc[acc.Count - 1], x)).ToList(),
    }));

ISet<(int, int)> Simulate(Func<(int, int Y), bool> isDone, ISet<(int, int Y)> rocks, bool hasFloor)
{
    var sand = new HashSet<(int, int)>();
    var floor = hasFloor ? 2 + rocks.Select(x => x.Y).Max() : int.MaxValue;
    while (true)
    {
        var s = (X: 500, Y: 0);
        do
        {
            s = s with { Y = s.Y + 1};
            if (!rocks.Contains(s) && !sand.Contains(s) && s.Y < floor) continue;
            s = s with { X = s.X - 1};
            if (!rocks.Contains(s) && !sand.Contains(s) && s.Y < floor) continue;
            s = s with { X = s.X + 2};
            if (!rocks.Contains(s) && !sand.Contains(s) && s.Y < floor) continue;
            s = s with { X = s.X - 1, Y = s.Y - 1};
            sand.Add(s);
            break;
        } while (!isDone(s));
        if (isDone(s)) return sand;
    }
}

var input = File.ReadAllLines("input.txt")
    .SelectMany(ParseLine)
    .ToHashSet();

Console.WriteLine($"The answer to the first part is: {Simulate(x => x.Y > input.Select(x => x.Y).Max(), input, false).Count()}");
Console.WriteLine($"The answer to the second part is: {Simulate(x => x == (500, 0), input, true).Count()}");
