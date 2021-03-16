import java.util.*
import kotlin.collections.HashMap


fun main() {
    val counts = HashMap<String, Int>()

    val input = Scanner(System.`in`)
    while (input.hasNextLine()) {
        val tokenizer = StringTokenizer(input.nextLine())
        while (tokenizer.hasMoreTokens()) {
            val word = tokenizer.nextToken()
            counts[word] = (counts[word] ?: 0) + 1
        }
    }

    val pairs = counts.toList().sortedByDescending { pair -> pair.second }
    for ((word, count) in pairs) {
        print("$word $count\n")
    }
}
