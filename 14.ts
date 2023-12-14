const input = `O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`;

const matrix = input.split("\n").map((row) => row.split(""));

const transpose = <T>(matrix: T[][]) =>
  matrix[0].map((_, i) => matrix.map((row) => row[i]));

const cellToInt = (cell: string) => {
  if (cell === ".") return 1;
  if (cell === "O") return 2;
  return 3;
};

const intToCell = (int: number) => {
  if (int === 1) return ".";
  if (int === 2) return "O";
  return "#";
};

const intMatrix = matrix.map((row) => row.map(cellToInt));

const sortMatrix = (matrix: number[][]) => {
  const sortLine = (_row: number[]) => {
    const row = [..._row];
    let sorted = true;

    for (let i = 1; i < row.length; i++) {
      const p = row[i - 1];
      const c = row[i];
      if (c !== 3) {
        if (c > p) {
          sorted = false;
          const tmp = row[i];
          row[i] = row[i - 1];
          row[i - 1] = tmp;
        }
      }
    }

    return sorted ? row : sortLine(row);
  };

  return matrix.map(sortLine);
};

const tilt = (matrix: number[][], direction: "N" | "W" | "S" | "E") => {
  if (direction === "N")
    return transpose(sortMatrix(transpose(matrix))) as number[][];

  if (direction === "W") return sortMatrix(matrix) as number[][];

  if (direction === "S")
    return transpose(
      sortMatrix(transpose(matrix).map((line) => line.reverse())).map((row) =>
        row.reverse()
      )
    ) as number[][];

  return sortMatrix(matrix.map((row) => row.reverse())).map((row) =>
    row.reverse()
  ) as number[][];
};

const getTotalLoadOnNorth = (matrix: number[][]) =>
  matrix
    .map(
      (row, index) =>
        row.filter((cell) => cell === 2).length * (matrix.length - index)
    )
    .reduce((a, b) => a + b);

const answer1 = (() => {
  // Answer: 136
  console.log(getTotalLoadOnNorth(tilt(intMatrix, "N")));
})();

const formatMatrix = (matrix: number[][]) =>
  matrix.map((line) => line.map(intToCell).join(""));

const cycle = (matrix: number[][]) => {
  const a = tilt(matrix, "N");
  const b = tilt(a, "W");
  const c = tilt(b, "S");
  const d = tilt(c, "E");
  return d;
};

const answer2 = (() => {
  const totalCycle = 1_000_000_000;
  let matrix = intMatrix as number[][];
  const cache = new Map();

  let start = 0;
  let end = 0;
  let currentCycle = 0;
  while (true) {
    const key = JSON.stringify(matrix);
    if (cache.has(key)) {
      start = cache.get(key)!;
      end = currentCycle;
      break;
    }

    cache.set(key, currentCycle);
    matrix = cycle(matrix);
    ++currentCycle;
  }

  const endCycleIndex = ((totalCycle - start) % (end - start)) + start;

  const northLoad = getTotalLoadOnNorth(
    JSON.parse(
      Array.from(cache).find(
        ([key, cycleNumber]) => cycleNumber === endCycleIndex
      )![0]
    )
  );

  // Answer: 64
  console.log(northLoad);
})();
