"use strict";
const fs = require("fs");
const dict = {};

// Calls itemHandler on each substring terminated by the token.
// Returns any leftover characters in the string
const forEachTerminated = (str, token, itemHandler) => {
    let lastIndex = 0;

    for (;;) {
        const index = str.indexOf(token, lastIndex);

        if (index === -1) break;
        itemHandler(str.slice(lastIndex, index));

        lastIndex = index + 1;
    }

    return str.slice(lastIndex);
};

const wordHandler = (word) => {
    if (word.length === 0) return;
    const item = dict[word];
    if (item !== undefined) {
        item.num++;
    } else {
        dict[word] = {num:1};
    }
};

const lineHandler = (line) => {
    if (line.length === 0) return;
    wordHandler(forEachTerminated(line, " ", wordHandler));
};

const endHandler = () => {
    const keys = Object.keys(dict);
    keys.sort((a, b) => dict[b].num - dict[a].num);
    process.stdout.write(keys.map((key) => `${key} ${dict[key].num}\n`).join(""));
};

let buffer = "";
const BUFSIZE = 64 * 1024;
const encoding = "utf-8";
const buf = Buffer.alloc(BUFSIZE, "", encoding);
let bytesRead;
const lineToken = "\n";
for (;;) {
    bytesRead = fs.readSync(process.stdin.fd, buf, 0, BUFSIZE);
    if (bytesRead > 0) {
        buffer = forEachTerminated(buffer + buf.toString(encoding, 0, bytesRead).toLowerCase(), lineToken, lineHandler);
    } else {
        if (buffer.length > 0) lineHandler(buffer);
        endHandler();
        break;
    }
}
