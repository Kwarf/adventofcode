var elves = File.ReadAllText("input.txt")
    .Split($"{Environment.NewLine}{Environment.NewLine}")
    .Select(x => new Elf(x.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries).Select(int.Parse).ToList()));

Console.WriteLine($"The answer to the first part is: {elves.Select(x => x.Food.Sum()).Max()}");

record struct Elf(List<int> Food);
