const input = `R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)`;

type ParsedInput = {
  dir: string;
  meter: number;
};

const parseInput = (rawInput: string): ParsedInput[] =>
  rawInput.split("\n").map((line) => {
    const [dir, meter] = line.split(" ");
    return {
      dir,
      meter: parseInt(meter),
    };
  });

const parseInputBasedOnColor = (rawInput: string): ParsedInput[] =>
  rawInput.split("\n").map((line) => {
    const colorHex = line.split(" ").slice(-1)[0].slice(2, -1);
    const meter = parseInt(colorHex.slice(0, -1), 16);
    const dir = (() => {
      const dirIndicatorNum = colorHex.slice(-1);
      switch (dirIndicatorNum) {
        case "0":
          return "R";
        case "1":
          return "D";
        case "2":
          return "L";
        case "3":
          return "U";
      }
    })()!;

    return {
      dir,
      meter,
    };
  });

type Coordination = {
  x: number;
  y: number;
};

const getArea = (coordinations: Coordination[]) => {
  let area = 0;

  for (let i = 0; i < coordinations.length - 1; i++) {
    const { x: x1, y: y1 } = coordinations[i];
    const { x: x2, y: y2 } = coordinations[i + 1];
    area += (y1 + y2) * (x2 - x1);
  }

  return Math.abs(area) / 2;
};

const toCoordinations = (input: ParsedInput[]) => {
  let x = 0;
  let y = 0;
  let coordinations: Coordination[] = [{ x, y }];

  input.forEach(({ dir, meter }) => {
    switch (dir) {
      case "U": {
        y -= meter;
        break;
      }
      case "R": {
        x += meter;
        break;
      }
      case "D": {
        y += meter;
        break;
      }
      case "L": {
        x -= meter;
        break;
      }
    }

    coordinations.push({
      x,
      y,
    });
  });

  return coordinations;
};

const getDiggedArea = (coordinations: Coordination[]) => {
  let diggedArea = 0;
  for (let i = 1; i < coordinations.length; i++) {
    const p = coordinations[i - 1];
    const c = coordinations[i];
    diggedArea += Math.abs(c.x - p.x);
    diggedArea += Math.abs(c.y - p.y);
  }
  diggedArea /= 2;
  ++diggedArea;
  return diggedArea;
};

const getAnswer = (parsedInput: ParsedInput[]) => {
  const coordinations = toCoordinations(parsedInput);
  const diggableArea = getArea(coordinations);
  const diggedArea = getDiggedArea(coordinations);

  return diggableArea + diggedArea;
};

const main = (() => {
  const parsedInput = parseInput(input);
  const parsedInputBasedOnColor = parseInputBasedOnColor(input);

  // Answer: 62
  console.log("Answer 1:", getAnswer(parsedInput));

  // Answer: 952408144115
  console.log("Answer 2:", getAnswer(parsedInputBasedOnColor));
})();
