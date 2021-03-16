const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  // output: process.stdout
});

const counts = {};

rl.on('line', line => {
  if (!line) {
    return;
  }

  line.toLowerCase()
      .split(' ')
      .forEach(word => {
        if (!word) {
          return;
        }

        counts[word] = typeof counts[word] === 'undefined' ? 1 : counts[word] + 1;
      });
});

rl.on('close', () => {
  Object.entries(counts)
        .sort((a, b) =>  b[1] - a[1])
        .forEach(item => {
          console.log(`${item[0]} ${item[1]}`)
        });
});
