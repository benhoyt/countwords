const std = @import("std");
const hash_map = std.hash_map;
const murmur = std.hash.murmur;
const WordMap = std.HashMap([]const u8, u32, hashString, hash_map.eqlString, 80);

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const ally = &arena.allocator;

    var words = WordMap.init(ally);
    const stdin = std.io.getStdIn().reader();
    comptime const buf_len: usize = 65536;
    var buf: [buf_len + 8]u8 align(@alignOf(u64)) = undefined;
    var numread: usize = try stdin.read(buf[0..buf_len]);

    while (numread > 0) {
        // margin word to stop the loop without bound checks
        buf[numread] = ' ';
        buf[numread + 1] = 'a';
        buf[numread + 2] = ' ';

        // find words and count them
        var wstart: usize = 0;
        while (true) {
            while (buf[wstart] <= ' ') : (wstart += 1) {}
            var wend = wstart;
            while (buf[wend] > ' ') : (wend += 1) {
                buf[wend] = std.ascii.toLower(buf[wend]);
            }

            if (wend >= numread) break;

            const word = buf[wstart..wend];
            const ret = try words.getOrPut(word);
            if (ret.found_existing) {
                ret.entry.value += 1;
            } else {
                ret.entry.key = try ally.dupe(u8, word);
                ret.entry.value = 1;
            }

            wstart = wend + 1;
        }

        // copy the unprocessed chars to the front of buffer
        var remain: usize = 0;
        if (wstart < numread) {
            std.mem.copy(u8, &buf, buf[wstart..numread]);
            remain = numread - wstart;
        }

        // fill the buffer again
        numread = try stdin.read(buf[remain..buf_len]);
        if (numread > 0) numread += remain;
    }

    // extract unique words and sort them based on counts
    const words_slice = try ally.alloc(WordMap.Entry, words.count());
    var i: usize = 0;
    var it = words.iterator();
    while (it.next()) |entry| : (i += 1) {
        words_slice[i] = entry.*;
    }
    std.sort.sort(WordMap.Entry, words_slice, {}, compare);

    // print sorted values in words_slice
    var stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
    for (words_slice) |entry| {
        try stdout.writer().print("{s} {d}\n", .{ entry.key, entry.value });
    }
    try stdout.flush();
}

fn compare(_: void, a: WordMap.Entry, b: WordMap.Entry) bool {
    return a.value > b.value;
}

pub fn hashString(s: []const u8) u64 {
    return std.hash.Murmur2_32.hash(s);
}
