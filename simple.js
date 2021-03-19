const wordCounter = Object.create(null); //{} has protected and prepopulated keys, Map() is slower

require("readline").createInterface({input: process.stdin})
  .on("line", line => {
    line.toLowerCase().split(' ').forEach(word => {
      if (!word) return;
      wordCounter[word] = (wordCounter[word] || 0) + 1;
    });
  })
  .on("close", () => {
    let result = Object.entries(wordCounter)
      .sort((a, b) =>  b[1] - a[1])
      .map(entry => `${entry[0]} ${entry[1]}`)
      .join("\n");

    console.log(result);
  });
