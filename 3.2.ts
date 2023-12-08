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
  char === undefined ? false : char === "*";

const gearRegex = /\*/g;
const numberRegex = /\d+/g;

function isB(number: number, point1: number, point2: number): boolean {
  const min = Math.min(point1, point2);
  const max = Math.max(point1, point2);

  return number >= min && number <= max;
}

let ans = 0;

for (let i = 0; i < lines.length; ++i) {
  const currentLine = lines[i];

  const matches = Array.from(currentLine.matchAll(gearRegex), (match) => ({
    start: match.index!,
    end: match.index! + match[0].length - 1,
  }));

  for (let y = 0; y < matches.length; ++y) {
    const currentMatch = matches[y];
    const adjacent: any[] = [];

    // Exclude first and last row
    if (i > 0 && i !== lines.length - 1) {
      const previousRowMatches = Array.from(
        lines[i - 1].matchAll(numberRegex),
        (match) => ({
          start: match.index!,
          end: match.index! + match[0].length - 1,
          number: parseInt(match[0], 10),
        })
      );
      const nextRowMatches = Array.from(
        lines[i + 1].matchAll(numberRegex),
        (match) => ({
          start: match.index!,
          end: match.index! + match[0].length - 1,
          number: parseInt(match[0], 10),
        })
      );

      previousRowMatches.forEach((x) => {
        if (isB(x.end, currentMatch.start - 1, currentMatch.end + 1))
          adjacent.push(x.number);
        else if (isB(x.start, currentMatch.start - 1, currentMatch.end + 1))
          adjacent.push(x.number);
      });

      nextRowMatches.forEach((x) => {
        if (isB(x.end, currentMatch.start - 1, currentMatch.end + 1)) {
          adjacent.push(x.number);
          console.log(x.number);
        } else if (isB(x.start, currentMatch.start - 1, currentMatch.end + 1)) {
          adjacent.push(x.number);
          console.log(x.number);
        }
      });
    }

    const clm = Array.from(currentLine.matchAll(numberRegex), (match) => ({
      start: match.index!,
      end: match.index! + match[0].length - 1,
      number: parseInt(match[0], 10),
    }));

    clm.forEach((x) => {
      if (x.end === currentMatch.start - 1 || x.start === currentMatch.end + 1)
        adjacent.push(x.number);
    });

    if (adjacent.length === 2) {
      ans += adjacent[0] * adjacent[1];
    }

    console.log(adjacent);
  }
}

// Answer: 467835
console.log(ans);
