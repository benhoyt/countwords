using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static IEnumerable<string> ReadLines()
    {
        string line;
        while ((line = Console.ReadLine()) != null)
        {
            yield return line;
        }
    }

    static void Main(string[] args)
    {
        var iter = from line in ReadLines()
                   from word in line.Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(x => x.ToLowerInvariant())
                   group word by word into wordGroup
                   let entry = new KeyValuePair<string, int>(wordGroup.Key, wordGroup.Count())
                   orderby entry.Value descending
                   select entry;

        foreach (var entry in iter)
        {
            Console.WriteLine($"{entry.Key} {entry.Value}");
        }
    }
}