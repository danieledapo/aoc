
function gcd(a: number, b: number): number {
    return b === 0 ? a : gcd(b, a % b);
}

function lcm(a: number, b: number) {
    return (a * b) / gcd(a, b);
}

const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day8.txt").text();

const [dirs, locLines] = text.trim().split("\n\n");

const locs = new Map(locLines.split("\n").map((l) => {
    const matches = l.match(/(\w+) = \((\w+), (\w+)\)/)!;
    return [matches[1], [matches[2], matches[3]]]
}));

function calcSteps(loc: string, isEnd: (loc: string) => boolean): number {
    let steps = 0;

    for (let i = 0; !isEnd(loc); i = (i + 1) % dirs.length) {
        if (dirs[i] === "L")
            loc = locs.get(loc)![0];
        else
            loc = locs.get(loc)![1];

        ++steps;
    }

    return steps;
}

solution.part1 = calcSteps("AAA", (e) => e === "ZZZ");

const counts: number[] = [];
for (const [k, _] of locs) {
    if (k.endsWith("A"))
        counts.push(calcSteps(k, (e) => e.endsWith("Z")));
}

solution.part2 = counts.reduce(lcm);

console.log(solution);
