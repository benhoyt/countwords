"use strict";

const rd = require("readline");

const dict = new Map();

function RefNum(num) {
  this.num = num;
}
RefNum.prototype.toString = function () {
  return this.num.toString();
};

const wordHandler = (word) => {
  if (word.length === 0) return;
  word = word.toLowerCase();
  const item = dict.get(word);
  if (item !== undefined) {
    item.num++;
  } else {
    dict.set(word, new RefNum(1));
  }
};

const lineHandler = (line) => {
  let lastIndex = 0;
  while (true) {
    const index = line.indexOf(" ", lastIndex);
    if (index === -1) {
      wordHandler(line.slice(lastIndex));
      return;
    }

    wordHandler(line.slice(lastIndex, index));
    lastIndex = index + 1;
  }
};

const endHandler = () => {
  const keys = Array.from(dict.keys());
  keys.sort((a, b) => dict.get(b) - dict.get(a));
  const len = keys.length;
  for (let i = 0; i < len; ++i) {
    const key = keys[i];
    process.stdout.write(`${key} ${dict.get(key).num}\n`);
  }
};

rd.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
})
  .on("line", lineHandler)
  .on("close", endHandler);
