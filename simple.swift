import Foundation

let input = FileHandle.standardInput

if let text = String(data: input.readDataToEndOfFile(), encoding: .utf8) {

  let words = text.split { $0.isWhitespace || $0.isNewline }

  var counts = [String:Int]()

  for word in words {
    let canon = word.lowercased()
    counts[canon, default: 0] += 1
  }

  let ordered = counts.sorted { $0.1 > $1.1 }

  ordered.forEach { entry in
    print(entry.key, entry.value.description)
  }
}
