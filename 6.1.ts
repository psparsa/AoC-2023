const input = `Time:      7  15   30
Distance:  9  40  200`;

const lines = input.split("\n");

const times = lines[0]
  .split(":")[1]
  .trim()
  .split(" ")
  .map((x) => x.trim())
  .filter((x) => x.length !== 0)
  .map(Number);
const records = lines[1]
  .split(":")[1]
  .trim()
  .split(" ")
  .map((x) => x.trim())
  .filter((x) => x.length !== 0)
  .map(Number);

//////////////////////////////////////////////////////

const doIWin = (holdTime: number) => (record: number) => (time: number) => {
  const tmp = (time - holdTime) * holdTime;

  if (holdTime >= time || holdTime === 0) return false;
  return tmp > record;
};

let ans = 1;
for (let i = 0; i < records.length; i++) {
  let tmp = 0;
  const t = times[i];
  const r = records[i];

  for (let j = 1; j <= t; j++) {
    if (doIWin(j)(r)(t)) {
      ++tmp;
    }
  }

  ans *= tmp === 0 ? 1 : tmp;
}

// Answer: 288
console.log(ans);
