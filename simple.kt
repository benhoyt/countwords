fun main() = generateSequence { readLine() }
        .flatMap { it.toLowerCase().splitToSequence(' ') }
        .filter { it.isNotBlank() }
        .groupingBy { it }
        .eachCount()
        .asSequence()
        .sortedByDescending { it.value }
        .forEach { println("${it.key} ${it.value}") }
