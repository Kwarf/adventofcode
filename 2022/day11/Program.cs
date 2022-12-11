Func<int, int, int> Op(char op) => op switch
{
    '+' => (int a, int b) => a + b,
    '*' => (int a, int b) => a * b,
};

var monkeys = File.ReadAllLines("input.txt")
    .Chunk(7)
    .Select(line =>
    {
        Func<int, int> op = line[2].Substring(19).Split(' ') switch
        {
            ["old", var x, "old"] => (int old) => Op(x[0])(old, old),
            ["old", var x, var n] => (int old) => Op(x[0])(old, Int32.Parse(n)),
        };

        return new Monkey(new Queue<int>(line[1].Substring(18).Split(", ").Select<string, int>(int.Parse))
            , (monkeys, item) =>
            {
                var worry = op(item) / 3;
                var target = worry % Int32.Parse(line[3].Substring(21)) == 0
                    ? Int32.Parse(line[4].Substring(29))
                    : Int32.Parse(line[5].Substring(30));
                monkeys[target].Items.Enqueue(worry);
            }
            , 0);
    })
    .ToList();

for (var round = 0; round < 20; round++)
{
    for (var i = 0; i < monkeys.Count; i++)
    {
        while (monkeys[i].Items.TryDequeue(out var item))
        {
            monkeys[i].Action(monkeys, item);
            monkeys[i] = monkeys[i] with { Inspected = monkeys[i].Inspected + 1 };
        }
    }
}

Console.WriteLine($"The answer to the first part is: {monkeys.Select(x => x.Inspected).OrderDescending().Take(2).Aggregate(1, (acc, x) => acc * x)}");

record struct Monkey(Queue<int> Items, Action<List<Monkey>, int> Action, int Inspected);