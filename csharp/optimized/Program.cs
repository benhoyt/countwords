using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

var wordCount = new Dictionary<string, StrongBox<int>>(StringComparer.OrdinalIgnoreCase);

using var stream = Console.OpenStandardInput();
using var streamReader = new StreamReader(stream, System.Text.Encoding.UTF8, false, 64 * 1024);

string line;
while ((line = streamReader.ReadLine()) != null)
{
    int lastPos = 0;
    for (int index = 0; index <= line.Length; index++)
    {
        if (index == line.Length || line[index] == ' ')
        {
            if (lastPos < index)
            {
                // Substring takes ~15% of the time
                var word = line.Substring(lastPos, index - lastPos);
                // TryGetValue takes ~55% of the time
                if (!wordCount.TryGetValue(word, out var count)) {
                    wordCount.Add(word, new StrongBox<int>(1));
                } else {
                    ++count.Value;
                }
            }
            lastPos = index + 1;
        }
    }
}

// minimal performance impact
var ordered = wordCount.OrderByDescending(pair => pair.Value.Value);
foreach (var entry in ordered)
{
    Console.WriteLine("{0} {1}", entry.Key.ToLower(), entry.Value.Value.ToString());
}

