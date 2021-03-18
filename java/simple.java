import java.io.*;
import java.util.*;

/**
 * Should work in JDK8+.
 * 
 * Test: 
 *  $ javac simple.java; echo "one two three one two two" | java simple
 * 
 */
public class simple {
    public static void main(String[] args) throws IOException {
        Map<String, Integer> counts = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String line;
            while ((line = reader.readLine()) != null) {
                StringTokenizer st = new StringTokenizer(line);
                while (st.hasMoreTokens()) {
                    String word = st.nextToken().toLowerCase();
                    if (!word.isEmpty()) {
                        counts.put(word, counts.getOrDefault(word, 0) + 1);
                    }
                }
            }
        }

        List<Map.Entry<String, Integer>> countsAsList = new ArrayList<>(counts.entrySet());
        Collections.sort(countsAsList, Map.Entry.comparingByValue(Comparator.reverseOrder()));

        for (Map.Entry<String, Integer> e : countsAsList) {
            System.out.print(e.getKey());
            System.out.print(' ');
            System.out.println(e.getValue());
        }
    }
}
