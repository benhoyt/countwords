package benhoyt.countwords

import java.util.*
import kotlin.collections.HashMap

class Bonus {
    fun solve() {
        val counts = HashMap<String, Int>()

        val input = Scanner(System.`in`)
        while (input.hasNextLine()) {
            val line = input.nextLine()
            if (line.isEmpty()) {
                break
            }

            val words = line.toLowerCase().split("\\s+".toRegex())
            for (word in words) {
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
    Bonus().solve()
}