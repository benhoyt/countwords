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
 * It's marginally faster than StringTokenizer implementation (probably because I still allocate same number of Strings on tokenization, I just use different buffer size).
 * 
 * x10 original benchmark:
    Language      | Simple | Optimized | Notes
    ------------- | ------ | --------- | -----
    Java          |   1.11 |      1.08 | by Iulian Plesoianu

 * using x20 benchmark (doubled the original load):
    Language      | Simple | Optimized | Notes
    ------------- | ------ | --------- | -----
    Java          |  16.86 |     16.48 | by Iulian Plesoianu

 */
public class optimized {
    private static final char LINE_END = '\n';
    private static final int LINE_BUFFER_SIZE = 64 * 1024;
    private static final int CHAR_BUFFER_SIZE = 200;

    public static void main(String[] args) throws IOException {
        Map<String, Integer> counts = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in), LINE_BUFFER_SIZE)) {
            char[] lineBuffer = new char[LINE_BUFFER_SIZE];
            StringBuilder wordBuffer = new StringBuilder(CHAR_BUFFER_SIZE);
            int nrChars;
            while((nrChars =  reader.read(lineBuffer)) != -1) {
                for (int i = 0; i < nrChars; i++) {
                    char currentChar = lineBuffer[i];
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
