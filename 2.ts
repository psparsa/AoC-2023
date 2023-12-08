type Input = string[];
type ColorName = "red" | "green" | "blue";
type SetType = { value: number; colorName: ColorName }[];

type IdentityFunctor<A> = {
  map: <B>(f: (x: A) => B) => IdentityFunctor<B>;
  fold: <B>(f: (x: A) => B) => B;
};

const identityFunctor = <A>(value: A): IdentityFunctor<A> => ({
  map: <B>(f: (x: A) => B) => identityFunctor<B>(f(value)),
  fold: <B>(f: (x: A) => B) => f(value),
});

const extractSets = (record: string) =>
  record
    .split(":")[1]
    .split(";")
    .map((setStr) => setStr.trim());

const parseSet = (str: string): SetType => {
  const tokens = str.split(", ");
  return tokens.map((token) => {
    const [value, colorName] = token.split(" ") as [string, ColorName];
    return { value: parseInt(value, 10), colorName };
  });
};

const isSatisfying = ({ colorName, value }: SetType[0]) => {
  switch (colorName) {
    case "red":
      return value <= 12;
    case "green":
      return value <= 13;
    case "blue":
      return value <= 14;
    default:
      return false;
  }
};

const getBiggestColorValue = (set: SetType) => (colorName: ColorName) =>
  Math.max(
    ...set
      .filter((token) => token.colorName === colorName)
      .map((value) => value.value)
  );

const input = [
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
];

const boxes = identityFunctor(input).map((input) =>
  input.map((record, index) => ({
    id: index + 1,
    sets: extractSets(record),
  }))
);

const boxesWithParsedSets = boxes.map((bags) =>
  bags.map((bag) => ({
    ...bag,
    sets: bag.sets.flatMap((set) => parseSet(set)),
  }))
);

const satisfiableBoxes = boxesWithParsedSets.map((boxes) =>
  boxes.filter((box) => box.sets.every(isSatisfying))
);

const answer1 = satisfiableBoxes.fold((box) =>
  box.reduce((p, c) => p + c.id, 0)
);

const boxesWithBiggestColorValue = boxesWithParsedSets.map((bags) =>
  bags.map((bag) => ({
    ...bag,
    biggestColorValues: {
      red: getBiggestColorValue(bag.sets)("red"),
      green: getBiggestColorValue(bag.sets)("green"),
      blue: getBiggestColorValue(bag.sets)("blue"),
    },
  }))
);

const answer2 = boxesWithBiggestColorValue.fold((data) =>
  data.reduce(
    (p, { biggestColorValues }) =>
      p +
      biggestColorValues.red *
        biggestColorValues.green *
        biggestColorValues.blue,
    0
  )
);

// Answer: 8
console.log("Answer 1:", answer1);
// Answer: 2286
console.log("Answer 2:", answer2);
