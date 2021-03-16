import std.stdio;
import std.string;
import std.algorithm.iteration : splitter;
import std.algorithm.sorting : schwartzSort;

char mytolower(char c) {
  mixin("char lc = 'a' - 'A';");
  if (c < 'A' || c > 'Z') {
    return c;
  }
  return cast(char)(c + lc);
}

void main() {
  int[string] freq;
  char[] buf;

  while (stdin.readln(buf) && !stdin.eof()) {
    foreach (word; splitter(buf)) {
      for (ptrdiff_t idx = 0; idx < word.length; idx++) {
        word[idx] = mytolower(word[idx]);
      }
      auto key = cast(immutable char[])word.dup;
      if (auto ptr = (key in freq)) {
        ++*ptr;
      }
      else {
        freq[key] = 1;
      }
    }
  }

  auto ordered = schwartzSort!(k => freq[k], "a > b")(freq.keys);
  foreach (k; ordered) {
    writeln(k," ", freq[k]);
  }
}
