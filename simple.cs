
// to compile:
// csc.exe -o+ simple.cs

using System;
using System.IO;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Linq;

namespace freq
{
    class Program
    {
        static void Main(string[] args)
        {
            Dictionary<string, int> tbl = new Dictionary<string, int>(StringComparer.Ordinal);
            int value;


            string line;
            string[] words;
            while ((line = Console.ReadLine()) != null)
            {
                words = line.Split((char[])null);
                foreach (string w in words)
                {
                    if(w.Length == 0) {
                        continue;
                    }
                    string lower = w.ToLower();
                    tbl[lower] = (tbl.TryGetValue(lower, out value)) ? tbl[lower] = value + 1 : tbl[lower] = 1;
                }
            }

            // sort results
            // https://stackoverflow.com/questions/21411384/sort-dictionary-string-int-by-value
            var tbl_sorted = tbl.OrderByDescending(pair => pair.Value).ThenBy(pair => pair.Key);

            foreach (KeyValuePair<string, int> entry in tbl_sorted)
            {
                Console.WriteLine("{0} {1}", entry.Key, entry.Value);
            }
        }
    }
}
