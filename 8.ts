type Move = "L" | "R";

type NodeType = {
  nodeName: string;
  leftNode: {
    nodeName: string;
    position: number;
  };
  rightNode: {
    nodeName: string;
    position: number;
  };
};

type ParsedInput = {
  moves: Move[];
  nodes: NodeType[];
};

const parseInput = (input: string): ParsedInput => {
  const [_moves, _nodes] = input.split("\n\n");

  const moves = _moves.split("") as Move[];

  const __nodes = _nodes.split("\n").map((x) => {
    const a = x.split(" = ");
    const b = a[1].split(", ");
    return {
      nodeName: a[0],
      leftNode: {
        nodeName: b[0].slice(1),
      },
      rightNode: {
        nodeName: b[1].slice(0, 3),
      },
    };
  });

  const nodes: NodeType[] = __nodes.map((ins) => ({
    ...ins,
    leftNode: {
      ...ins.leftNode,
      position: __nodes.findIndex((x) => x.nodeName === ins.leftNode.nodeName)!,
    },
    rightNode: {
      ...ins.leftNode,
      position: __nodes.findIndex(
        (x) => x.nodeName === ins.rightNode.nodeName
      )!,
    },
  }));

  return { moves, nodes };
};

const lessGoo = (parsedInput: ParsedInput) => (startNode: NodeType) => {
  const { moves, nodes } = parsedInput;
  let ans = 0;
  let currentNode = startNode;
  let currentMovePointer = 0;

  const goToNextMove = () => {
    currentMovePointer = (currentMovePointer + 1) % moves.length;
  };

  while (currentNode.nodeName[2] !== "Z") {
    const whereToGo =
      moves[currentMovePointer] === "L"
        ? currentNode.leftNode
        : currentNode.rightNode;

    currentNode = nodes[whereToGo.position];

    ++ans;
    goToNextMove();
  }

  return ans;
};

const firstAnswerInput = parseInput(`LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)`);

const firstAnswer = lessGoo(firstAnswerInput)(
  firstAnswerInput.nodes.find((node) => node.nodeName === "AAA")!
);

const secondAnswerInput = parseInput(`LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)`);

const numberOfGhostMoves = secondAnswerInput.nodes.filter(
  ({ nodeName }) => nodeName[2] === "A"
);

const gcd = (a: number, b: number) => (a % b === 0 ? b : gcd(b, a % b));

const leastCommonMultiple = (a: number, b: number) => (a * b) / gcd(a, b);

const secondAnswerLessGoo = lessGoo(secondAnswerInput);

const secondAnswer = numberOfGhostMoves
  .map(secondAnswerLessGoo)
  .reduce(leastCommonMultiple);

// Answer: 6
console.log(firstAnswer);

// Answer: 6
console.log(secondAnswer);
