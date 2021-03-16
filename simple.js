"use strict";

const rd = require("readline");

const dict = {};

const wordHandler = word => {
  if (!word) return;
  dict[word] = (dict[word] || 0) + 1;
};

const lineHandler = line => {
  if (!line) return;
  line.toLowerCase().split(' ').forEach(wordHandler);
};

const endHandler = () => {
  const entries = Object.entries(dict);
  entries.sort((a, b) =>  b[1] - a[1]);
  entries.forEach(entry => console.log(`${entry[0]} ${entry[1]}`));
};

rd.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
})
  .on("line", lineHandler)
  .on("close", endHandler);
