const std = @import("std");
const WordMap = std.StringHashMap(u32);

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const ally = &arena.allocator;

    var words = WordMap.init(ally);
    const stdin = std.io.bufferedReader(std.io.getStdIn().reader()).reader();
    var buf: [1024]u8 = undefined;
    while (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        for (line) |*c| c.* = std.ascii.toLower(c.*);
        var it = std.mem.tokenize(line, " ");
        while (it.next()) |word| {
            const ret = try words.getOrPut(word);
            if (ret.found_existing) {
                ret.entry.value += 1;
            } else {
                ret.entry.key = try ally.dupe(u8, word);
                ret.entry.value = 1;
            }
        }
    }

    const words_slice = try ally.alloc(WordMap.Entry, words.count());
    var i: usize = 0;
    var it = words.iterator();
    while (it.next()) |entry| : (i += 1) {
        words_slice[i] = entry.*;
    }
    std.sort.sort(WordMap.Entry, words_slice, {}, compare);

    var stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
    for (words_slice) |entry| {
        try stdout.writer().print("{s} {d}\n", .{ entry.key, entry.value });
    }
    try stdout.flush();
}

fn compare(_: void, a: WordMap.Entry, b: WordMap.Entry) bool {
    return a.value > b.value;
}
