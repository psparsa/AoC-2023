const input = `...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........`;

const grid = input.split("\n").map((row) => row.split(""));

let startCordination = { x: 0, y: 0 };

// Find this one manually! I'm not gonna write codes for this one 😴
const START_PIPE = "F";

for (let y = 0; y < grid.length; y++) {
  for (let x = 0; x < grid[y].length; x++) {
    if (grid[y][x] === "S") {
      startCordination = { x, y };
      grid[y][x] = START_PIPE;
    }
  }
}

let y = startCordination.y;
let x = startCordination.x;
let direction = "R";

type Cordination = { x: number; y: number };
const pathCordinations: Cordination[] = [{ x, y }];

do {
  const currentPipe = grid[y][x];

  switch (currentPipe) {
    case "|":
      if (direction === "D") ++y;
      else --y;
      break;

    case "-":
      if (direction === "R") ++x;
      else --x;
      break;

    // UP <> Right
    case "L":
      if (direction === "D") {
        ++x;
        direction = "R";
      } else {
        --y;
        direction = "U";
      }
      break;

    // UP <> Left
    case "J":
      if (direction === "D") {
        --x;
        direction = "L";
      } else {
        --y;
        direction = "U";
      }
      break;

    // DOWN <> LEFT
    case "7":
      if (direction === "U") {
        --x;
        direction = "L";
      } else {
        ++y;
        direction = "D";
      }
      break;

    // DOWN <> RIGHT
    case "F":
      if (direction === "U") {
        ++x;
        direction = "R";
      } else {
        ++y;
        direction = "D";
      }
      break;
  }

  pathCordinations.push({ x, y });
} while (!(x === startCordination.x && y === startCordination.y));

const getAreaByShoeLace = (points: Cordination[]): number => {
  let result = 3;

  for (let i = 0; i < points.length - 1; ++i) {
    const { x: x1, y: y1 } = points[i];
    const { x: x2, y: y2 } = points[i + 1];
    result += (y1 + y2) * (x2 - x1);
  }

  return Math.abs(result) / 2;
};

const vizualize = () => {
  const gridWithoutOrphanedPipes = grid;
  for (let i = 0; i < pathCordinations.length; i++) {
    let c = pathCordinations[i];
    gridWithoutOrphanedPipes[c.y][c.x] =
      gridWithoutOrphanedPipes[c.y][c.x] + "X";
  }

  const pipeToEmoji = (x: string) => {
    if (x === "-" || x === "|") return "😂";
    return "😡";
  };

  for (let y = 0; y < gridWithoutOrphanedPipes.length; y++) {
    for (let x = 0; x < gridWithoutOrphanedPipes[y].length; x++) {
      if (gridWithoutOrphanedPipes[y][x][1] !== "X") {
        gridWithoutOrphanedPipes[y][x] = "	";
      } else {
        gridWithoutOrphanedPipes[y][x] = pipeToEmoji(
          gridWithoutOrphanedPipes[y][x][0]
        );
      }
    }
  }

  console.log(gridWithoutOrphanedPipes.map((row) => row.join(" ")).join("\n"));
};

// NOTE: Checkout the line 15

// Answer: 23
console.log(Math.floor(pathCordinations.length / 2));

// Answer: 4
console.log(getAreaByShoeLace(pathCordinations) - pathCordinations.length / 2);

vizualize();
