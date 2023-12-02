const solution = {
    part1: 0,
    part2: 0,
};

const games = new Map<number, Map<string, number>[]>();

for (const l of (await Bun.file("input/day2.txt").text()).trim().split("\n")) {
    const parts = l.split(":");

    const id = parseInt(parts[0].split(" ")[1]);

    games.set(
        id,
        parts[1].split(";").map((round) => {
            return new Map(
                round.split(",").map((c) => {
                    const p = c.trim().split(" ");
                    return [p[1], parseInt(p[0])];
                }),
            );
        }),
    );
}

const max_qty = new Map<string, number>([
    ["red", 12],
    ["green", 13],
    ["blue", 14],
]);

for (const [id, rounds] of games) {
    let possible = true;

    const minimum = new Map<string, number>();

    for (const round of rounds) {
        for (const [k, max] of max_qty) {
            if (!round.has(k))
                continue;

            if (round.get(k)! > max)
                possible = false;

            if (!minimum.has(k))
                minimum.set(k, -1);

            minimum.set(k, Math.max(minimum.get(k)!, round.get(k)!));
        }
    }

    if (possible)
        solution.part1 += id;

    solution.part2 += [...minimum.values()].reduce((a, b) => a * b, 1);
}

console.log(solution);
