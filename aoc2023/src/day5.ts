const solution = {
    part1: 0,
    part2: 0,
};

interface RangeMap {
    output: string;
    ranges: { input_start: number; output_start: number; len: number }[];
}

const text = (await Bun.file("input/day5.txt").text()).trim();

function resolveLocation(maps: Map<string, RangeMap>, seed: number): number {
    let value = seed;
    let input = "seed";
    while (input !== "location") {
        const m = maps.get(input)!;

        let i = 0;
        let j = m.ranges.length - 1;
        while (i <= j) {
            const mid = Math.floor((i + j) / 2);
            const ran = m.ranges[mid]!;

            if (value >= ran.input_start && value < ran.input_start + ran.len) {
                value = ran.output_start + (value - ran.input_start);
                break;
            }

            if (value < ran.input_start)
                j = mid - 1;
            else
                i = mid + 1;
        }

        input = m.output;
    }
    return value;
}

const seeds: number[] = [];
const maps: Map<string, RangeMap> = new Map();

let last_input = "";
for (const l of text.split("\n")) {
    if (l.length === 0)
        continue;

    if (l.startsWith("seeds: ")) {
        for (const n of l.split("seeds: ")[1].split(" "))
            seeds.push(parseInt(n.trim()));
        continue;
    }

    if (l.endsWith("map:")) {
        const [inp, _, output] = l.split(" ")[0].split("-");
        last_input = inp;
        maps.set(last_input, { output, ranges: [] });
        continue;
    }

    const [output_start, input_start, len] = l.split(" ").map((s) => parseInt(s));
    maps.get(last_input)!.ranges.push({ input_start, output_start, len });
}

for (const r of maps.values()) {
    r.ranges.sort((r0, r1) => {
        return r0.input_start + r0.len <= r1.input_start ? -1 : 1;
    });
}


solution.part1 = Infinity;
for (const seed of seeds) {
    solution.part1 = Math.min(solution.part1, resolveLocation(maps, seed));
}

solution.part2 = Infinity;
for (let i = 0; i < seeds.length; i += 2) {
    for (let dc = 0; dc < seeds[i + 1]; ++dc) {
        solution.part2 = Math.min(
            solution.part2,
            resolveLocation(maps, seeds[i] + dc),
        );
    }
}

console.log(solution);
