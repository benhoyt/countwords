"use strict";

const rd = require("readline");

const dict = new Map();

const wordHandler = (word) => {
  if (word.length === 0) return;
  const lowerCaseWord = word.toLowerCase();
  const value = (dict.get(lowerCaseWord) || 0) + 1;
  dict.set(lowerCaseWord, value);
};

const lineHandler = (line) => {
  line.split(" ").forEach(wordHandler);
};

const endHandler = () => {
  const keys = Array.from(dict.keys());
  keys.sort((a, b) => dict.get(b) - dict.get(a));
  keys.forEach((key) => console.log(`${key} ${dict.get(key)}`));
};

rd.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
})
  .on("line", lineHandler)
  .on("close", endHandler);
