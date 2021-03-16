import 'dart:async';
import 'dart:io';
import 'dart:convert';

void main() {
  final dict = <String, int>{};

  Stream<String> readLine() =>
      stdin.transform(utf8.decoder).transform(const LineSplitter());

  StreamSubscription? cmdLineSubscription;
  cmdLineSubscription = readLine().listen((processLine) {
    processLine.split(" ").forEach((word) {
      if (word.isEmpty) return;
      final String lowerCaseWord = word.toLowerCase();
      dict[lowerCaseWord] = (dict[lowerCaseWord] ?? 0) + 1;
    });
  }, onDone: () {
    var sortedKeys = dict.keys.toList()
      ..sort((a, b) => dict[b]!.compareTo(dict[a]!));
    sortedKeys.forEach((key) => print('$key ${dict[key]}'));
    cmdLineSubscription?.cancel();
  });
}
