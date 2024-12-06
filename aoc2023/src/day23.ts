const solution = {
    part1: 0,
    part2: 0,
};

type AdjMap = Map<string, [number, number][]>;

const text = await Bun.file("input/day23.txt").text();

const grid = text.trim().split("\n");
const [w, h] = [grid[0].length, grid.length];

function* openNeighboringPaths(r: number, c: number, adj: AdjMap): Generator<[number, number]> {
    if (grid[r][c] === '#')
        return;

    for (const [dr, dc] of adj.get(grid[r][c])!) {
        const [rr, cc] = [r + dr, c + dc];
        if (rr >= 0 && rr < h && cc >= 0 && cc < w && grid[rr][cc] !== "#")
            yield [rr, cc];
    }
}

function run(adj: AdjMap): number {
    const [sr, sc] = [0, grid[0].indexOf(".")];
    const [er, ec] = [h - 1, grid[h - 1].indexOf(".")];

    const splitPoints = new Set<number>([
        sr * w + sc,
        er * w + ec,
    ]);

    for (let r = 0; r < h; ++r) {
        for (let c = 0; c < w; ++c) {
            const free_neighbors = [...openNeighboringPaths(r, c, adj)].length;
            if (free_neighbors > 2)
                splitPoints.add(r * w + c);
        }
    }

    const dists = new Map<number, [number, number, number][]>(); // row, col, dist
    for (const k of splitPoints) {
        dists.set(k, []);

        const [r, c] = [Math.floor(k / w), k % w];

        let q = [[r, c]];
        const seen = new Set<number>([k]);
        let dist = 0;

        while (q.length > 0) {
            const nq = [];
            ++dist;

            for (const neighbor of q) {
                for (const [rr, cc] of openNeighboringPaths(neighbor[0], neighbor[1], adj)) {
                    const kk = rr * w + cc;
                    if (seen.has(kk))
                        continue;

                    if (splitPoints.has(kk)) {
                        dists.get(k)!.push([rr, cc, dist]);
                    } else {
                        nq.push([rr, cc]);
                    }

                    seen.add(kk);
                }
            }

            q = nq;
        }
    }

    function dfs(r: number, c: number, inPath = new Set<number>(), totalDist = 0): number {
        if (r === er && c === ec)
            return totalDist;

        let maxDist = 0;
        for (const [rr, cc, dist] of dists.get(r * w + c)!) {
            if (inPath.has(rr * w + cc))
                continue;

            inPath.add(rr * w + cc);
            maxDist = Math.max(maxDist, dfs(rr, cc, inPath, dist + totalDist));
            inPath.delete(rr * w + cc);
        }

        return maxDist;
    }

    return dfs(sr, sc);
}


solution.part1 = run(new Map([
    [".", [[-1, 0], [1, 0], [0, -1], [0, 1]]],
    ["v", [[1, 0]]],
    ["^", [[-1, 0]]],
    ["<", [[0, -1]]],
    [">", [[0, 1]]],
]));
solution.part2 = run(
    new Map(".v^<>".split("").map(c => [c, [[-1, 0], [1, 0], [0, -1], [0, 1]]]))
);

console.log(solution)
