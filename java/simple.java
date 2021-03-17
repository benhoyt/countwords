import java.io.*;
import java.util.*;

/**
 * Should work in JDK8+.
 * Test: 
 *  $ javac simple.java; echo "one two three one two two" | java simple
 * 
 */
public class simple {

    public static void main(String[] args) {
        solve(System.in, System.out);
    }

    static void solve(InputStream in, OutputStream out) {
        Map<String, Integer> wc = new HashMap<>();

        try (BufferedReader scan = new BufferedReader(new InputStreamReader(in))) {
            for (String line = scan.readLine(); line != null; line = scan.readLine()) {
                StringTokenizer st = new StringTokenizer(line);
                while (st.hasMoreTokens()) {
                    String w = st.nextToken();
                    if (w.length() == 0) {
                        continue;
                    }
                    wc.compute(w.toLowerCase(), (k, v) -> (v == null) ? 1 : v + 1);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        List<Map.Entry<String, Integer>> listWC = new ArrayList<>(wc.entrySet());
        Collections.sort(listWC, (e1, e2) -> e2.getValue().compareTo(e1.getValue()));

        try (PrintWriter outWriter = new PrintWriter(out)) {
            for (Map.Entry<String, Integer> e : listWC) {
                outWriter.print(e.getKey());
                outWriter.print(" ");
                outWriter.println(e.getValue());
            }
        }
    }
}
