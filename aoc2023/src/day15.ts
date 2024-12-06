const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day15.txt").text();

function hash(s: string): number {
    let h = 0;
    for (let i = 0; i < s.length; ++i)
        h = (h + s.charCodeAt(i)) * 17 % 256;
    return h;
}

const boxes: Map<string, number>[] = [];
for (let i = 0; i < 256; ++i)
    boxes[i] = new Map();

for (const p of text.trim().split(",")) {
    solution.part1 += hash(p);

    if (p.endsWith("-")) {
        const label = p.slice(0, p.length - 1);
        boxes[hash(label)].delete(label);
        continue;
    }

    const [label, ns] = p.split("=");
    boxes[hash(label)].set(label, parseInt(ns));
}

for (let box = 0; box < boxes.length; ++box) {
    let i = 1;
    for (const fl of boxes[box].values()) {
        solution.part2 += (box + 1) * i * fl;
        ++i;
    }
}

console.log(solution);
