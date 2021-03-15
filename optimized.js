"use strict";

const rd = require("readline");

const dict = {};

function RefNum(num) {
  this.num = num;
}

RefNum.prototype.toString = function () {
  return this.num.toString();
};

const wordHandler = (word) => {
  if (word.length === 0) return;
  word = word.toLowerCase();
  const item = dict[word];
  if (item !== undefined) {
    item.num++;
  } else {
    dict[word] = new RefNum(1);
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
  const keys = Object.keys(dict);
  keys.sort((a, b) => dict[b].num - dict[a].num);

  process.stdout.write(keys.map((key) => `${key} ${dict[key].num}\n`).join(""));
};

rd.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
})
  .on("line", lineHandler)
  .on("close", endHandler);
