"use strict";

const rd = require("readline");

const dict = new Map();

const wordHandler = (word) => {
  if (word.length === 0) return;
  word = word.toLowerCase();
  const item = dict.get(word);

  if (item !== undefined) {
    dict.set(word, item + 1);
  } else {
    dict.set(word, 1);
  }
};

const lineHandler = (line) => {
  line.split(" ").forEach(wordHandler);
};

const endHandler = () => {
  const keys = Array.from(dict.keys());
  keys.sort((a, b) => dict.get(b) - dict.get(a));
  const len = keys.length;
  for (let i = 0; i < len; ++i) {
    const key = keys[i];
    console.log(`${key} ${dict.get(key)}`);
  }
};

rd.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
})
  .on("line", lineHandler)
  .on("close", endHandler);
