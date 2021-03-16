// Original version by John Taylor

using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

class Program
{
    public sealed class Ref<T> {
        public Ref(T initialValue) {
            Value = initialValue;
        }
        public T Value { get; set; }
    }

    static void Main(string[] args)
    {
        var counts = new Dictionary<string, Ref<int>>();
        string line;
        while ((line = Console.ReadLine()) != null)
        {
            line = line.ToLower();
            var words = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            string word;
            for (int i = 0; i < words.Length; i++)
            {
                word = words[i];
                if (!counts.TryGetValue(word, out var count)) {
                    counts.Add(word, new Ref<int>(1));
                } else {
                    count.Value += 1;
                }
            }
        }
        var ordered = counts.OrderByDescending(pair => pair.Value.Value);
        foreach (var entry in ordered)
        {
            Console.WriteLine("{0} {1}", entry.Key, entry.Value.Value);
        }
    }
}
