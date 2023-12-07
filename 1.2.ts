const input = [
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen",
];

const numbersInWords = [
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
];

function getLastDigit(str: string) {
  let tmp = "";
  for (let i = str.length - 1; i > -1; i--) {
    tmp = `${str[i]}${tmp}`;
    const j = numbersInWords.findIndex((num) => num === tmp);
    if (j !== -1) return String(j + 1);
  }
}

function getAnswer(inputContent: string[]) {
  let total = 0;
  inputContent.forEach((line) => {
    let list: string[] = [];
    let tmp = "";

    for (let i = 0; i < line.length; i++) {
      tmp += line[i];

      if (/^\d$/.test(line[i])) {
        list.push(line[i]);
        tmp = "";
      } else {
        const hah = getLastDigit(tmp);
        if (hah) list.push(hah);
      }
    }

    if (list.length === 1) list.push(list[0]);
    total += parseInt(`${list[0]}${list[list.length - 1]}`);
  });

  return total;
}

// Answer: 281
console.log(getAnswer(input));
