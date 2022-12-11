Func<long, long, long> Op(char op) => op switch
{
    '+' => (long a, long b) => a + b,
    '*' => (long a, long b) => a * b,
};

List<Monkey> Monkeys() => File.ReadAllLines("input.txt")
    .Chunk(7)
    .Select(line =>
    {
        Func<long, long> op = line[2].Substring(19).Split(' ') switch
        {
            ["old", var x, "old"] => (long old) => Op(x[0])(old, old),
            ["old", var x, var n] => (long old) => Op(x[0])(old, Int32.Parse(n)),
        };

        return new Monkey(new Queue<long>(line[1].Substring(18).Split(", ").Select<string, long>(long.Parse))
            , (monkeys, item, worryOp) =>
            {
                var worry = worryOp(op(item));
                var target = worry % Int32.Parse(line[3].Substring(21)) == 0
                    ? Int32.Parse(line[4].Substring(29))
                    : Int32.Parse(line[5].Substring(30));
                monkeys[target].Items.Enqueue(worry);
            }
            , 0
            , Int32.Parse(line[3].Substring(21)));
    })
    .ToList();

long Part(long part)
{
    var monkeys = Monkeys();
    Func<long, long> worry = part == 1
        ? (long x) => x / 3
        : (long x) => x % monkeys.Aggregate(1L, (acc, x) => acc * x.Check);

    for (var round = 0; round < (part == 1 ? 20 : 10000); round++)
    {
        for (var i = 0; i < monkeys.Count; i++)
        {
            while (monkeys[i].Items.TryDequeue(out var item))
            {
                monkeys[i].Action(monkeys, item, worry);
                monkeys[i] = monkeys[i] with { Inspected = monkeys[i].Inspected + 1 };
            }
        }
    }
    return monkeys.Select(x => x.Inspected).OrderDescending().Take(2).Aggregate(1L, (acc, x) => acc * x);
}

Console.WriteLine($"The answer to the first part is: {Part(1)}");
Console.WriteLine($"The answer to the second part is: {Part(2)}");

record struct Monkey(Queue<long> Items, Action<List<Monkey>, long, Func<long, long>> Action, long Inspected, long Check);