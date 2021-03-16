import std.stdio;
import std.string;
import ctype = core.stdc.ctype;
import std.algorithm.iteration : each, splitter;
import std.algorithm.sorting : schwartzSort;

void main() {
  int[string] freq;
  char[] buf;
  while (stdin.readln(buf) && !stdin.eof()) {
    foreach (word; splitter(buf)) {
      for (ptrdiff_t idx = 0; idx < word.length; idx++) {
        //word[idx] = cast(char)word[idx].toLower();
        word[idx] = cast(char)ctype.tolower(word[idx]);
      }
      freq[ cast(immutable char[])word.dup ]++;
    }
  }

  auto ordered = schwartzSort!(k => freq[k], "a > b")(freq.keys);
  foreach (k; ordered) {
    writeln(k," ", freq[k]);
  }
}
