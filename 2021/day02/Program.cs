using System;
using System.IO;
using System.Linq;

var commands = File.ReadAllLines("input.txt")
	.Select(Command.FromString)
	.ToArray();

Console.WriteLine($"The answer to the first part is: {commands.Aggregate(default(State), Command.ApplyForPartOne, x => x.Position * x.Depth)}");
Console.WriteLine($"The answer to the second part is: {commands.Aggregate(default(State), Command.ApplyForPartTwo, x => x.Position * x.Depth)}");

enum Direction { Horizontal, Vertical }

record struct State(int Position = default, int Depth = default, int Aim = default);

record struct Command(Direction Direction, int Delta)
{
	public static Command FromString(string line)
	{
		var parts = line.Split(' ');
		var delta = int.Parse(parts[1]);

		return new Command(parts[0] == "forward" ? Direction.Horizontal : Direction.Vertical
			, parts[0] == "up" ? -delta : delta
			);
	}

	public static State ApplyForPartOne(State state, Command command) => command.Direction switch
	{
		Direction.Horizontal => state with { Position = state.Position + command.Delta },
		Direction.Vertical => state with { Depth = state.Depth + command.Delta },
		_ => throw new InvalidOperationException(),
	};

	public static State ApplyForPartTwo(State state, Command command) => command.Direction switch
	{
		Direction.Horizontal => state with
		{
			Position = state.Position + command.Delta,
			Depth = state.Depth + state.Aim * command.Delta,
		},
		Direction.Vertical => state with { Aim = state.Aim + command.Delta },
		_ => throw new InvalidOperationException(),
	};
}
