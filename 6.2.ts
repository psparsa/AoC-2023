const input = `Time:      7  15   30
Distance:  9  40  200`;

const lines = input.split("\n");

const times = parseInt(
  lines[0]
    .split(":")[1]
    .trim()
    .split(" ")
    .map((x) => x.trim())
    .filter((x) => x.length !== 0)
    .join(""),
  10
);
const records = parseInt(
  lines[1]
    .split(":")[1]
    .trim()
    .split(" ")
    .map((x) => x.trim())
    .filter((x) => x.length !== 0)
    .join(""),
  10
);

//////////////////////////////////////////////////////

const doIWin = (holdTime: number) => (record: number) => (time: number) => {
  const tmp = (time - holdTime) * holdTime;

  if (holdTime >= time || holdTime === 0) return false;
  return tmp > record;
};

let ans = 1;
for (let i = 0; i < 1; i++) {
  let tmp = 0;
  const t = times;
  const r = records;

  for (let j = 1; j <= t; j++) {
    if (doIWin(j)(r)(t)) {
      ++tmp;
    }
  }

  ans *= tmp === 0 ? 1 : tmp;
}

// 71503
console.log(ans);
