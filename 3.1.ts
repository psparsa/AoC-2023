const input = `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`;

const lines = input.split("\n");

const isSymbol = (char: string | undefined) =>
  char === undefined ? false : !/^\d|\.$/.test(char);

var regex = /\d+/g;

let total = 0;
let matchedNumbers: number[] = [];

for (let i = 0; i < lines.length; ++i) {
  const currentLine = lines[i];

  const matches = Array.from(currentLine.matchAll(regex), (match) => ({
    start: match.index!,
    end: match.index! + match[0].length - 1,
    number: parseInt(match[0], 10),
  }));

  for (let y = 0; y < matches.length; ++y) {
    const { start, end, number } = matches[y];

    const left = currentLine[start - 1];
    const right = currentLine[end + 1];

    // Check left & right
    if (isSymbol(left) || isSymbol(right)) {
      // Found
      total += number;
      matchedNumbers.push(number);
    } else {
      let found = false;
      // Check upper row
      if (i > 0) {
        for (let u = start - 1; u <= end + 1; ++u) {
          const currentCell = lines[i - 1][u];
          if (isSymbol(currentCell)) {
            total += number;
            matchedNumbers.push(number);
            found = true;
            break;
          }
        }
      }

      if (found) continue;

      // Check bottom row
      if (i !== lines.length - 1) {
        console.log(number);
        for (let u = start - 1; u <= end + 1; ++u) {
          const currentCell = lines[i + 1][u];
          if (isSymbol(currentCell)) {
            total += number;
            matchedNumbers.push(number);
            break;
          }
        }
      }
    }
  }
}

// Answer: 4361
console.log(total);
