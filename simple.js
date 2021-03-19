const wordCounter = Object.create(null); //{} has protected and prepopulated keys, Map() is slower

require("readline").createInterface({input: process.stdin})
  .on("line", line => {
    line.toLowerCase().trim().split(' ').forEach(word => {
      wordCounter[word] = (wordCounter[word] || 0) + 1;
    });
  })
  .on("close", () => {
    let result = Object.entries(wordCounter)
      .sort((a, b) =>  b[1] - a[1])
      .map(entry => `${entry[0]} ${entry[1]}\n`)
      .join("");

    console.log(result);
  });
