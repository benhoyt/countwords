// Original version by John Taylor

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        var counts = new Dictionary<string, int>();
        string line;
        while ((line = Console.ReadLine()) != null)
        {
            line = line.ToLower();
            var words = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            foreach (string word in words)
            {
                counts[word] = counts.GetValueOrDefault(word, 0) + 1;
            }
        }
        var ordered = counts.OrderByDescending(pair => pair.Value);
        foreach (var entry in ordered)
        {
            Console.WriteLine("{0} {1}", entry.Key, entry.Value);
        }
    }
}
