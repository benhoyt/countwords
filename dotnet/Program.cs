using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace CountWords
{
    internal static class Program
    {
        private static void Main()
        {
            Dictionary<string, int> counts = new(StringComparer.Ordinal);

            using Stream inStream = Console.OpenStandardInput();
            using StreamReader inStreamReader = new(inStream, Encoding.ASCII, bufferSize: 64 * 1024);

            string line;
            while ((line = inStreamReader.ReadLine()) != null)
            {
                Span<char> s = new char[line.Length];
                line.AsSpan().ToLowerInvariant(s);

                int index;
                while ((index = s.IndexOf(' ')) != -1)
                {
                    ReadOnlySpan<char> word = s[..index];

                    s = s[(index + 1)..];

                    if (word.IsEmpty)
                    {
                        continue;
                    }

                    string wordStr = word.ToString();

                    counts[wordStr] = counts.GetValueOrDefault(wordStr, 0) + 1;
                }

                if (!s.IsEmpty)
                {
                    string wordStr = s.ToString();

                    counts[wordStr] = counts.GetValueOrDefault(wordStr, 0) + 1;
                }
            }

            using Stream outStream = Console.OpenStandardOutput();
            using StreamWriter outStreamWriter = new(outStream, Encoding.ASCII);

            foreach ((string word, int count) in counts.OrderByDescending(pair => pair.Value))
            {
                outStreamWriter.WriteLine("{0} {1}", word, count);
            }
        }
    }
}
