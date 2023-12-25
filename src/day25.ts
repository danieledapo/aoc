const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day25.txt").text();

const vertices = new Set<string>();
const edges = new Map<string, [string, string]>();

for (const l of text.trim().split("\n")) {
    const vs = l.replace(":", "").split(" ");

    for (const v of vs) {
        vertices.add(v);
        if (v !== vs[0])
            edges.set(`${vs[0]}-${v}`, [vs[0], v]);
    }
}

const keys = [...edges.keys()];

while (true) {
    const subsets = [...vertices].map(v => new Set([v]));

    const findSubset = (v: string): Set<string> => {
        for (const s of subsets)
            if (s.has(v))
                return s;
        throw "bug";
    };

    while (subsets.length > 2) {
        const [s1, s2] = edges.get(keys[Math.floor(Math.random() * keys.length)])!.map(findSubset);
        if (s1 === s2)
            continue;

        for (const v of s2)
            s1.add(v);

        subsets.splice(subsets.indexOf(s2), 1);
    }

    let c = 0;
    for (const [a, b] of edges.values()) {
        if (findSubset(a) !== findSubset(b))
            ++c;
        if (c >= 4)
            break;
    }

    if (c >= 4)
        continue;

    solution.part1 = subsets.reduce((a, b) => a * b.size, 1);
    break;
}


console.log(solution)
