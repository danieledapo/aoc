import FastPriorityQueue from "fastpriorityqueue";

const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day17.txt").text();

const grid = text.trim().split("\n").map(s => s.split("").map(n => parseInt(n)));
const [w, h] = [grid[0].length, grid.length];

function drive(minSteps: number, maxSteps: number): number {
    interface Pos {
        hl: number,
        r: number,
        c: number,
        dr: number,
        dc: number,
        step: number,
    }

    const seen = new Set<string>();

    const q = new FastPriorityQueue<Pos>((a, b) => a.hl < b.hl);
    q.add({
        hl: 0,
        r: 0,
        c: 0,
        dr: 0,
        dc: 0,
        step: 1
    })

    while (!q.isEmpty()) {
        const pos = q.poll()!;

        if (pos.c === w - 1 && pos.r === h - 1 && pos.step > minSteps)
            return pos.hl;

        const k = `${pos.r * w + pos.c}-${pos.dr}-${pos.dc}-${pos.step}`;
        if (seen.has(k))
            continue;
        seen.add(k);

        for (const [dr, dc] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
            if (dr === -pos.dr && dc === -pos.dc)
                continue;

            if (dr === pos.dr && dc === pos.dc && pos.step >= maxSteps)
                continue;

            if (!(pos.dc === 0 && pos.dr === 0)
                && (dr !== pos.dr || dc !== pos.dc)
                && pos.step < minSteps) {
                continue;
            }

            const step = (dr === pos.dr && dc === pos.dc) ? (pos.step + 1) : 1;

            const [rr, cc] = [pos.r + dr, pos.c + dc];
            if (rr >= 0 && rr < w && cc >= 0 && cc < w)
                q.add({
                    hl: pos.hl + grid[rr][cc],
                    r: rr,
                    c: cc,
                    dr,
                    dc,
                    step,
                });
        }
    }

    throw "no solution"
}

solution.part1 = drive(0, 3);
solution.part2 = drive(4, 10);
console.log(solution)
