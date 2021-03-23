class Entry {
	public var word:String;
	public var count:Int;

	public function new(word, count) {
		this.word = word;
		this.count = count;
	}
}

class Simple {
	static function main() {
		var dict = new Map<String, Int>();

		try {
			while (true) {
				var line = Sys.stdin().readLine();
				if (line.length == 0) {
					continue;
				}

				var words = line.split(' ');
				for (word in words) {
					if (word.length == 0) {
						continue;
					}

					var key = word.toLowerCase();
					var count = dict[key];
					dict[key] = count != null ? count + 1 : 1;
				}
			}
		} catch (e:haxe.io.Eof) {
			var entries = new Array<Entry>();
			for (word in dict.keys()) {
				entries.push(new Entry(word, dict[word]));
			}

			entries.sort(function(a, b) return b.count - a.count);

			for (entry in entries) {
				Sys.println('${entry.word} ${entry.count}');
			}
		}
	}
}
