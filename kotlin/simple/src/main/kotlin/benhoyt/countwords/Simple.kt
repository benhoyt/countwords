package benhoyt.countwords

import java.util.*
import kotlin.collections.HashMap

class Simple {
    fun solve() {
        val counts = HashMap<String, Int>()

        val input = Scanner(System.`in`)
        while (input.hasNextLine()) {
            val tokenizer = StringTokenizer(input.nextLine())
            while (tokenizer.hasMoreTokens()) {
                val word = tokenizer.nextToken()
                counts[word] = (counts[word] ?: 0) + 1
            }
        }

        val pairs = counts.toList().sortedBy { pair -> pair.second }.reversed()
        for ((word, count) in pairs) {
            print("$word $count\n")
        }
    }
}


fun main() {
    Simple().solve()
}