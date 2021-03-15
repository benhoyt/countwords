import std.stdio;
import std.string;
import std.algorithm.sorting : schwartzSort;

void main() {
  int[string] freq;
  while (! stdin.eof) {
    string line = stdin.readln().toLower();
    foreach (string word; line.split) {
      freq[word]++;
    }
  }

  auto ordered = schwartzSort!(k => freq[k], "a > b")(freq.keys);
  foreach (k; ordered){
    writeln(k, " ", freq[k]);
  }
}
