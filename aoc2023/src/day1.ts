const numbers = [
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

const solution = { part1: 0, part2: 0 };

for (const l of (await Bun.file("input/day1.txt").text()).trim().split("\n")) {
    let d0 = Infinity;
    let d1 = -1;
    for (let i = 0; i < l.length; ++i) {
        if (isNaN(parseInt(l.charAt(i))))
            continue;

        d0 = Math.min(d0, i);
        d1 = i;
    }

    solution.part1 += parseInt(l[d0] + l[d1]);

    let ds0 = { i: Infinity, n: Infinity };
    let ds1 = { i: -1, n: -1 };

    for (let ni = 0; ni < numbers.length; ++ni) {
        const i = l.indexOf(numbers[ni]);
        const j = l.lastIndexOf(numbers[ni]);

        if (i >= 0 && i < ds0.i)
            ds0 = { i, n: ni + 1 };

        if (j >= 0 && j > ds1.i)
            ds1 = { i: j, n: ni + 1 };
    }

    solution.part2 += parseInt(
        (ds0.i < d0 ? ds0.n.toString() : l[d0]) +
        (ds1.i > d1 ? ds1.n.toString() : l[d1]),
    );
}

console.log("solution", solution);
