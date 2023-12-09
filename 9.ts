const getPyramid = (
  numbers: number[],
  pyramid: number[][] = [numbers]
): number[][] => {
  const currentRow = numbers.slice(1).map((n, index) => n - numbers[index]);
  const allRowValuesAreZero = numbers.every((n) => n === 0);

  return allRowValuesAreZero
    ? [...pyramid, currentRow].reverse()
    : getPyramid(currentRow, [...pyramid, currentRow]);
};

const predict =
  (shouldPredictPreviousValue: boolean) => (rawNumbersLine: string) => {
    const numbers = rawNumbersLine.split(" ").map(Number);
    const pyramid = shouldPredictPreviousValue
      ? getPyramid(numbers).map((x) => x.reverse())
      : getPyramid(numbers);

    return pyramid.reduce((acc, row) => {
      const lastValue = row[row.length - 1];
      return shouldPredictPreviousValue ? lastValue - acc : lastValue + acc;
    }, 0);
  };

const predictNext = predict(false);
const predictPrevious = predict(true);

const input = `0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45`.split("\n");

const firstAnswer = input.map(predictNext).reduce((p, c) => p + c);
const secondAnswer = input.map(predictPrevious).reduce((p, c) => p + c);

// Answer: 114
console.log(firstAnswer);

// Answer: 2
console.log(secondAnswer);
