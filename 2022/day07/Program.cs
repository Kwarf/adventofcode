var tree = System.IO.File.ReadAllLines("input.txt").Aggregate<string, Node>(new Directory(null, "/"), (node, line) => Parse(node, line)).Root;

Console.WriteLine($"The answer to the first part is: {tree.Directories.Where(x => x.TotalSize <= 100000).Sum(x => x.TotalSize)}");

Node Parse(Node node, string data) => data.Split(' ') switch
{
    ["$", "cd", "/"] => node.Root,
    ["$", "cd", ".."] => node.Parent,
    ["$", "cd", var dir] => node.Directory(dir),
    ["$", "ls"] => node,
    ["dir", var name] => node.With(new Directory(node, name)),
    [var size, var name] => node.With(new File(node, name, Int32.Parse(size))),
};

record class Node(Node Parent, string Name, int Size, List<Node> Children)
{
    public Node Root => this.Parent?.Root ?? this;

    public int TotalSize => this.Children?.Sum(x => x.TotalSize) ?? this.Size;

    public IEnumerable<Node> Directories => this.Children?.OfType<Directory>().Concat(this.Children.SelectMany(x => x.Directories)) ?? Enumerable.Empty<Node>();

    public Node With(Node child)
    {
        this.Children.Add(child);
        return this;
    }

    public Node Directory(string name)
    {
        var d = this.Children?.FirstOrDefault(x => x.Name == name);
        if (d == null)
        {
            d = new Directory(this, name);
            this.Children.Add(d);
        }
        return d;
    }
}

record class Directory(Node Parent, string Name) : Node(Parent, Name, 0, new List<Node>());
record class File(Node Parent, string Name, int Size) : Node(Parent, Name, Size, null);