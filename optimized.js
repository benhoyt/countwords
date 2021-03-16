"use strict";

const dict = {};

// Calls itemHandler on each substring terminated by the token.
// Returns any leftover characters in the string
const forEachTerminated = (str, token, itemHandler) => {
  let lastIndex = 0;

  while (true) {
    const index = str.indexOf(token, lastIndex);

    if (index === -1) break;
    if(index > lastIndex) itemHandler(str.slice(lastIndex, index));

    lastIndex = index + 1;
  }

  if(lastIndex < str.length - 1) return str.slice(lastIndex);
  return "";
};

const wordHandler = word => {
  dict[word] = (dict[word] || 0) + 1;
};

const lineHandler = line => {
  wordHandler(forEachTerminated(line, " ", wordHandler));
};

const endHandler = () => {
  const entries = Object.entries(dict);
  entries.sort((a, b) =>  b[1] - a[1]);
  const output = entries.map((entry) => `${entry[0]} ${entry[1]}\n`).join("");
  process.stdout.write(output);
};

let buffer = "";
process.stdin.setEncoding("utf-8");
process.stdin.resume();

process.stdin.on("data", (data) => {
  buffer = forEachTerminated(buffer + data.toLowerCase(), "\n", lineHandler);
});

process.stdin.on("end", () => {
  if(buffer.length > 0) lineHandler(buffer);
  endHandler();
});
