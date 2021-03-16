fun main() = generateSequence { readLine() }
        .flatMap { it.toLowerCase().splitToSequence(' ') }
        .filter { !it.isBlank() }
        .groupingBy { it }
        .eachCount()
        .asSequence()
        .sortedByDescending { it.value }
        .forEach { println("${it.key} ${it.value}") }
