const input = `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`;

const rows = input.split("\n");

type NumberMetadata = {
  startPosition: number;
  endPosition: number;
  numberValue: number;
};
const rowsNumbers: NumberMetadata[][] = rows.map((row) =>
  Array.from(row.matchAll(/\d+/g), (match) => ({
    startPosition: match.index!,
    endPosition: match.index! + match[0].length - 1,
    numberValue: parseInt(match[0], 10),
  }))
);

type SymbolMetadata = { position: number; symbolValue: string };
const rowsSymbols: SymbolMetadata[][] = rows.map((row) =>
  Array.from(row.matchAll(/[^0-9.]/g), (match) => ({
    position: match.index!,
    symbolValue: match[0],
  }))
);

const isInRange = (number: number) => (point1: number) => (point2: number) => {
  const min = Math.min(point1, point2);
  const max = Math.max(point1, point2);

  return number >= min && number <= max;
};

const isPartNumber =
  (number: NumberMetadata) => (symbols: SymbolMetadata[]) => {
    return symbols.some((symbol) => {
      const isAdjacentInCurrentLine =
        symbol.position === number.startPosition - 1 ||
        symbol.position === number.endPosition + 1;

      const isAdjacentInAnotherLine = isInRange(symbol.position)(
        number.startPosition - 1
      )(number.endPosition + 1);

      return isAdjacentInCurrentLine || isAdjacentInAnotherLine;
    });
  };

const partNumbers = rowsNumbers.flatMap((row, rowIndex) => {
  return row.filter((numberMetadata) => {
    const isFirstRow = rowIndex === 0;
    const isLastRow = rowIndex === rowsNumbers.length - 1;

    const hasAnyAdjecentSymbolInPrevLine =
      !isFirstRow && isPartNumber(numberMetadata)(rowsSymbols[rowIndex - 1]);

    const hasAnyAdjecentSymbolInCurrentLine = isPartNumber(numberMetadata)(
      rowsSymbols[rowIndex]
    );

    const hasAnyAdjecentSymbolInNextLine =
      !isLastRow && isPartNumber(numberMetadata)(rowsSymbols[rowIndex + 1]);

    return (
      hasAnyAdjecentSymbolInPrevLine ||
      hasAnyAdjecentSymbolInCurrentLine ||
      hasAnyAdjecentSymbolInNextLine
    );
  });
});

const result = partNumbers
  .map(({ numberValue }) => numberValue)
  .reduce((a, b) => a + b);

// Answer: 4361
console.log(result);
