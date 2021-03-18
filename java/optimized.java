import java.io.*;
import java.util.*;

/**
 * 
 * Should work in JDK8+.
 * 
 *  * Test: 
 *  $ javac optimized.java; echo "one two three one two two" | java optimized
 *
 * This is using custom buffering to cache read characters up to a new word.
 * Using OpenJDK11/OracleJDK11 it actually seems to be slower than StringTokenizer, so I left it out of the benchmark for now.
 * 
 * using x10 original benchmark:
    Timing Java 1.13 1.23

    Language      | Simple | Optimized | Notes
    ------------- | ------ | --------- | -----
    Java          |   1.13 |      1.23 | by Iulian Plesoianu

 * using x20 benchmark (doubled the original load):
    Timing Java 16.15 19.46

    Language      | Simple | Optimized | Notes
    ------------- | ------ | --------- | -----
    Java          |  16.15 |     19.46 | by Iulian Plesoianu

 * 
 * I only got marginally better results than simple version using Graal VM which uses a new JIT compiler, which:
 * - I understand it does more aggresive optimizations
 * - it's not widely adopted yet, so I wouldn't consider it relevant for this benchmark
 * https://www.infoq.com/articles/Graal-Java-JIT-Compiler/
 * 
 * using x10 original benchmark:
    Timing Java 1.23 1.19

    Language      | Simple | Optimized | Notes
    ------------- | ------ | --------- | -----
    Java          |   1.23 |      1.19 | by Iulian Plesoianu
 * 
 * using x20 benchmark (doubled the original load):
    Timing Java 17.47 17.16

    Language      | Simple | Optimized | Notes
    ------------- | ------ | --------- | -----
    Java          |  17.47 |     17.16 | by Iulian Plesoianu
 * 
 */
public class optimized {
    private static final char LINE_END = '\n';
    private static final int LINE_BUFFER_SIZE = 64 * 1024;
    private static final int CHAR_BUFFER_SIZE = 200;

    public static void main(String[] args) throws IOException {
        Map<String, Integer> counts = new HashMap<>();

        try (BufferedInputStream reader = new BufferedInputStream(System.in, LINE_BUFFER_SIZE)) {
            byte[] lineBuffer = new byte[LINE_BUFFER_SIZE];
            StringBuilder wordBuffer = new StringBuilder(CHAR_BUFFER_SIZE);
            int nrChars;
            while((nrChars = reader.read(lineBuffer)) != -1) {
                for (int i = 0; i < nrChars; i++) {
                    byte currentChar = lineBuffer[i];
                    if (Character.isLetter(currentChar)) {
                        wordBuffer.append(Character.toLowerCase(currentChar));
                    } else if (Character.isSpaceChar(currentChar) || currentChar == LINE_END) {
                        if (wordBuffer.length() > 0) {
                            String word = wordBuffer.toString();
                            counts.put(word, counts.getOrDefault(word, 0) + 1);
                            // reset char buffer
                            wordBuffer.setLength(0);
                        }
                    } else {
                        wordBuffer.append(currentChar);
                    }
                }
            }

            // last word
            if (wordBuffer.length() > 0) {
                String word = wordBuffer.toString();
                counts.put(word, counts.getOrDefault(word, 0) + 1);
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
