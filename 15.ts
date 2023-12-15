const input = `rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7`;

const hash = (str: string) =>
  str
    .split("")
    .map((c) => c.charCodeAt(0))
    .reduce((acc, current) => ((acc + current) * 17) % 256, 0);

const answer1 = input
  .split(",")
  .map(hash)
  .reduce((a, b) => a + b);

type Slot = {
  label: string;
  focalLength: number;
};
type Box = Slot[];
const boxes = Array.from({ length: 256 }).fill([]) as Box[];

type Command = {
  labelHash: number;
  label: string;
} & (
  | { type: "remove"; focalLength?: never }
  | { type: "add"; focalLength: number }
);
const commands: Command[] = input.split(",").map((command) => {
  if (command.endsWith("-")) {
    const label = command.slice(0, command.length - 1);
    return {
      type: "remove",
      labelHash: hash(label),
      label,
    };
  } else {
    const [label, focalLength] = command.split("=");
    return {
      type: "add",
      labelHash: hash(label),
      focalLength: parseInt(focalLength),
      label,
    };
  }
});

for (let i = 0; i < commands.length; i++) {
  const current = commands[i];
  if (current.type === "remove") {
    boxes[current.labelHash] = boxes[current.labelHash].filter(
      (slot) => slot.label !== current.label
    );
  } else {
    const foundIndex = boxes[current.labelHash].findIndex(
      (slot) => slot.label === current.label
    );
    if (foundIndex === -1) {
      boxes[current.labelHash] = [
        ...boxes[current.labelHash],
        {
          label: current.label,
          focalLength: current.focalLength!,
        },
      ];
    } else {
      boxes[current.labelHash][foundIndex].focalLength = current.focalLength!;
    }
  }
}

let answer2 = boxes
  .flatMap((box, boxIndex) =>
    box.map(
      (slot, slotIndex) => (boxIndex + 1) * (slotIndex + 1) * slot.focalLength
    )
  )
  .reduce((a, b) => a + b);

// Answer: 1320
console.log(answer1);
// Answer: 145
console.log(answer2);
