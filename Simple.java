import java.io.*;
import java.util.*;

class Simple {
    static HashMap<String, Integer> countMap = new HashMap<>();

    public static void main(String[] args) throws IOException {
        String str;
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(System.out));
        // read from stdin
        while ((str = br.readLine()) != null) {
            // count.
            var st = new StringTokenizer(str.toLowerCase(), " ");
            while (st.hasMoreTokens()) {
                String e = st.nextToken();
                countMap.put(e, countMap.getOrDefault(e, 0) + 1);
            }
        }
        // once all counted.
        countMap.entrySet().stream()
                // Sort the HashMap
                .sorted(new Comparator<>() {
                    public int compare(Map.Entry<String, Integer> o1,
                                       Map.Entry<String, Integer> o2) {
                        return (o2.getValue()).compareTo(o1.getValue());
                    }
                })
                // Now write it to stdout.
                .forEach(e -> {
                    try {
                        bw.write(e.getKey());
                        bw.write(" ");
                        bw.write(e.getValue().toString());
                        bw.newLine();
                    } catch (IOException ioException) {
                        ioException.printStackTrace();
                    }
                });
        // flush all output.
        bw.flush();
    }
}