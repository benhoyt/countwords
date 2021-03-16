import 'dart:io';

void main() {
  final dict = <String, int>{};

  while (true) {
    String? input = stdin.readLineSync();
    if (input == null) break;
    input.split(" ").forEach((word) {
      if (word.isEmpty) return;
      final String lowerCaseWord = word.toLowerCase();
      dict[lowerCaseWord] = (dict[lowerCaseWord] ?? 0) + 1;
    });
  }

  var sortedKeys = dict.keys.toList()
    ..sort((a, b) => dict[b]!.compareTo(dict[a]!));
  sortedKeys.forEach((key) => print('$key ${dict[key]}'));
}
