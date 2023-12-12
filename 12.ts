const splitByComma = (s: string): string[] =>
  s === "" ? [] : s.split(",").flatMap((word) => [word, ...splitByComma(word)]);

interface SpringRow {
  springs: string;
  springGroups: number[];
}

const parseInput = (rawInput: string): SpringRow[] =>
  rawInput.split("\n").map((row) => {
    const [springs, rawSpringsGroups] = row.split(" ");
    return {
      springs,
      springGroups: rawSpringsGroups.split(",").map((group) => parseInt(group)),
    };
  });

const cache = new Map();
const getPossibleArrangements = ({
  springs,
  springGroups,
}: SpringRow): number => {
  const cacheKey = springs.concat(springGroups.join("."));
  if (cache.has(cacheKey)) return cache.get(cacheKey);

  if (springs.length === 0) {
    const tmp = springGroups.length === 0 ? 1 : 0;
    cache.set(cacheKey, tmp);
    return tmp;
  }

  const firstSpring = springs[0];
  const firstSpringGroup = springGroups[0];

  const isOkay = firstSpring === ".";
  const isBroken = firstSpring === "#";
  const isUnknown = firstSpring === "?";

  let answer = 0;

  if (isOkay || isUnknown) {
    answer += getPossibleArrangements({
      springs: springs.slice(1),
      springGroups,
    });
  }

  const noDotTillEndOfGroup = !springs.slice(0, firstSpringGroup).includes(".");

  const lastSpringIsBroken = springs[firstSpringGroup] === "#";
  const springLenthIsSameAsGroup = firstSpringGroup === springs.length;

  if (isBroken || isUnknown) {
    if (firstSpringGroup <= springs.length) {
      if (noDotTillEndOfGroup)
        if (!lastSpringIsBroken || springLenthIsSameAsGroup) {
          const remainingSprings = springLenthIsSameAsGroup
            ? ""
            : springs.substring(firstSpringGroup + 1);

          answer += getPossibleArrangements({
            springs: remainingSprings,
            springGroups: springGroups.slice(1),
          });
        }
    }
  }

  cache.set(cacheKey, answer);
  return answer;
};

const unfold = ({ springs, springGroups }: SpringRow): SpringRow => {
  return {
    springs: Array.from({ length: 5 }).fill(springs).join("?"),
    springGroups: Array.from({ length: 5 })
      .fill(springGroups)
      .flat() as number[],
  };
};

const main = (() => {
  const input = `???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1`;

  const add = (a: number, b: number) => a + b;
  const parsedInput = parseInput(input);

  const answer1 = parsedInput.map(getPossibleArrangements).reduce(add);
  console.log(answer1); // Answer: 21

  const unfoldedInput = parsedInput.map(unfold);
  const answer2 = unfoldedInput.map(getPossibleArrangements).reduce(add);
  console.log(answer2); // Answer: 525152
})();
