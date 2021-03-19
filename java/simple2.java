import java.io.*;
import java.util.*;

/**
 * Should work in JDK11+.
 * 
 * Test: 
 *  $ javac simple2.java; echo "one two three one two two" | java simple2
 * 
 * This looks simple but it's slow:
 * - regexp is slower than StringTokenizer
 * - streams is slower than simple Collections.sort
 * - String.format allocates a lot of new strings just for printing
 *
 * using x10 original benchmark:
    Timing Java 1.20 1.68

    Language      | Simple | Simple2   | Notes
    ------------- | ------ | --------- | -----
    Java          |   1.20 |      1.68 | by Iulian Plesoianu
 * 
 * using x20 benchmark (double the load):
    Timing Java 15.92 25.26

    Language      | Simple | Optimized | Notes
    ------------- | ------ | --------- | -----
    Java          |  15.92 |     25.26 | by Iulian Plesoianu
 */
public class simple2 {

    public static void main(String[] args) throws IOException {
    var reader = new BufferedReader(new InputStreamReader(System.in));

    var counts = new HashMap<String, Integer>();
    String line;
    while ((line = reader.readLine()) != null) {
      line = line.toLowerCase();
      var words = line.split("\\s");
      for (var word : words) {
        if (!word.isEmpty()) {
          counts.put(word, counts.getOrDefault(word, 0) + 1);
        }
      }
    }

    counts.entrySet().stream()
      .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
      .forEach(e -> System.out.printf("%s %d\n", e.getKey(), e.getValue()));
    }
}
