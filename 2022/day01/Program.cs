var elves = File.ReadAllText("input.txt")
    .Split($"{Environment.NewLine}{Environment.NewLine}")
    .Select(x => x.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries).Select(int.Parse).Sum())
    .OrderDescending()
    .ToArray();

Console.WriteLine($"The answer to the first part is: {elves[0]}");
Console.WriteLine($"The answer to the second part is: {elves.Take(3).Sum()}");
