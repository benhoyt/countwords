"use strict";
const fs = require("fs");

const BUFFER_SIZE = 64 * 1024;
const FNV_OFFSET = 2166136261;
const FNV_PRIME = 16777619;

class Entry {
    constructor(word) {
        this.num = 1;
        this.word = word;
    }
    toString() {
        return `${this.word} ${this.num}\n`;
    }
}

const toLower = (c) => (c <= 90/* Z */ && c >= 65/* A */) ? c + 32/* a - A */ : c;
const isDelimiter = c => c === 10/* \n */ || c === 32/* " " */;

const wordCounts = new Map();
const count = (buffer, size) => {
    let wordStart = 0;
    for (let i = 0, hash = FNV_OFFSET; i < size; i++) {
        const char = toLower(buffer[i]);
        if (!isDelimiter(char)) {
            hash = (hash * FNV_PRIME) ^ char;
        } else {
            if (i !== wordStart) {
                const item = wordCounts.get(hash);
                if (item !== undefined) {
                    item.num++;
                } else {
                    wordCounts.set(hash, new Entry(buffer.toString("ascii", wordStart, i).toLowerCase()));
                }
            }
            hash = FNV_OFFSET;
            wordStart = i + 1;
        }
    }
    return wordStart;
}

const buffer = Buffer.allocUnsafe(BUFFER_SIZE);
const fd = process.stdin.fd;
let offset = 0;
let num = 0;
while ((num = fs.readSync(fd, buffer, 0, BUFFER_SIZE, offset)) > 0) {
    offset += count(buffer, num);
}

process.stdout.write(
    [...wordCounts.values()]
        .sort((a, b) => b.num - a.num)
        .join("")
);
