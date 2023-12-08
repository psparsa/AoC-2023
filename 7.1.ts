const input = `32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483`;

const lines = input.split("\n");

function findMostRepeatedCharacter(str: string): {
  character: string | undefined;
  frequency: number;
} {
  const charFrequency: { [key: string]: number } = {};

  for (let char of str) {
    charFrequency[char] = (charFrequency[char] || 0) + 1;
  }

  let mostRepeatedChar: string | undefined;
  let highestFrequency = 0;

  for (let char in charFrequency) {
    if (charFrequency[char] > highestFrequency) {
      mostRepeatedChar = char;
      highestFrequency = charFrequency[char];
    }
  }

  return {
    character: mostRepeatedChar,
    frequency: highestFrequency,
  };
}

type Kind =
  | "Five of a kind"
  | "High card"
  | "Four of a kind"
  | "Full house"
  | "Three of a kind"
  | "Two pair"
  | "One pair";
const getKind = (hand: string): Kind => {
  const tmp = findMostRepeatedCharacter(hand);

  if (tmp.frequency === 5) return "Five of a kind";
  if (tmp.frequency === 4) return "Four of a kind";
  if (tmp.frequency === 3) {
    const [a, b] = hand.split("").filter((c) => c !== tmp.character);
    return a !== b ? "Three of a kind" : "Full house";
  }

  if (tmp.frequency === 1) return "High card";

  if (tmp.frequency === 2) {
    const h = hand.split("").filter((c) => c !== tmp.character);
    const x = new Set(h);
    if (x.size === 3) return "One pair";
  }

  return "Two pair";
};

type Weight = Record<Kind, number>;
const weight: Weight = {
  "Five of a kind": 7,
  "Four of a kind": 6,
  "Full house": 5,
  "Three of a kind": 4,
  "Two pair": 3,
  "One pair": 2,
  "High card": 1,
};

type Hand = {
  hand: string;
  bid: number;
  weight: number;
};
const hands: Hand[] = lines.map((line) => {
  const [hand, bid] = line.split(" ");

  return { hand, bid: parseInt(bid, 10), weight: weight[getKind(hand)] };
});

const listCards = [
  "A",
  "K",
  "Q",
  "J",
  "T",
  "9",
  "8",
  "7",
  "6",
  "5",
  "4",
  "3",
  "2",
].reverse();

const compareCard = (cardA: string, cardB: string) => {
  for (let i = 0; i < cardA.length; i++) {
    const a = listCards.findIndex((x) => x === cardA[i]);
    const b = listCards.findIndex((x) => x === cardB[i]);

    if (a === b) continue;
    else {
      if (a < b) return "A";
      return "B";
    }
  }

  return "A";
};

const sort = (handA: Hand, handB: Hand) => {
  if (handA.weight === handB.weight) {
    const tmp = compareCard(handA.hand, handB.hand);
    return tmp === "A" ? -1 : 1;
  }

  return handA.weight - handB.weight;
};

const sorted = hands.sort(sort);

console.log(sorted);

const ans = sorted
  .map((x, index) => x.bid * (index + 1))
  .reduce((a, b) => a + b);

// Answer: 6440
console.log(ans);
