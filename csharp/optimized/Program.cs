// Original version by John Taylor
// Optimized version by Yuriy Ostapenko and Osman Turan

using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    private const int BufferSize = 65536;

    public sealed class Ref<T>
    {
        public Ref(T initialValue)
        {
            Value = initialValue;
        }
        public T Value { get; set; }
    }

    static void Main(string[] args)
    {
        var counts = new Dictionary<string, Ref<int>>();
        var buffer = new char[BufferSize];
        var offset = 0;

        while (true)
        {
            // Read from stdin in chars
            var bytesRead = Console.In.Read(buffer, offset, BufferSize - offset);
            if (bytesRead == 0)
            {
                break;
            }

            // Determine last delimiter position
            var bufferLength = offset + bytesRead;
            var lastDelimiterIndex = -1;
            for (var k = bufferLength - 1; k >= 0; --k)
            {
                if (buffer[k] <= ' ')
                {
                    lastDelimiterIndex = k;
                    break;
                }
            }

            var count = lastDelimiterIndex >= 0 ? lastDelimiterIndex : bufferLength;
            var i = 0;

            while (true)
            {
                // Skip whitespaces before each word
                for (; i < count; ++i)
                {
                    var c = buffer[i];
                    if (c > ' ')
                    {
                        break;
                    }
                }

                // Find word boundary and make it lowercase at the same time.
                var start = i;
                for (; i < count; ++i)
                {
                    var c = buffer[i];
                    if (c <= ' ')
                    {
                        break;
                    }

                    // Set 6th bit to make char lowercase
                    buffer[i] = (char)(buffer[i] | 0x20);
                }

                if (i <= start)
                {
                    break;
                }

                // Collect word statistics
                var word = new String(buffer, start, i - start);
                if (!counts.TryGetValue(word, out var wordCountRef))
                {
                    counts.Add(word, new Ref<int>(1));
                }
                else
                {
                    ++wordCountRef.Value;
                }
            }

            // Copy remaining part to the beginning of the buffer. 
            if (lastDelimiterIndex >= 0)
            {
                offset = (offset + bytesRead - 1) - lastDelimiterIndex;
                Array.Copy(buffer, lastDelimiterIndex + 1, buffer, 0, offset);
            }
            else
            {
                offset = 0;
            }
        }

        // Finally list all word frequencies.
        var ordered = counts.OrderByDescending(pair => pair.Value.Value);
        foreach (var entry in ordered)
        {
            Console.WriteLine($"{entry.Key} {entry.Value.Value}");
        }
    }
}
