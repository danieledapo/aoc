const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day14.txt").text();

function north(grid: string[][]) {
    const [w, h] = [grid[0].length, grid.length];
    for (let r = 0; r < h; ++r) {
        for (let c = 0; c < w; ++c) {
            if (grid[r][c] !== "O")
                continue;

            for (let dr = -1; r + dr >= 0; --dr) {
                if (grid[r + dr][c] !== ".")
                    break;

                grid[r + dr + 1][c] = ".";
                grid[r + dr][c] = "O";
            }
        }
    }
}

function rot(grid: string[][]): string[][] {
    const [w, h] = [grid[0].length, grid.length];
    const res: string[][] = [];
    for (let r = 0; r < h; ++r) {
        for (let c = 0; c < w; ++c) {
            if (res[c] === undefined)
                res[c] = [];
            res[c][h - r - 1] = grid[r][c];
        }
    }
    return res;
}

function totalLoad(grid: string[][]): number {
    const [w, h] = [grid[0].length, grid.length];
    let res = 0;
    for (let r = 0; r < h; ++r) {
        for (let c = 0; c < w; ++c) {
            if (grid[r][c] !== "O")
                continue;

            res += h - r;
        }
    }
    return res;
}

let grid = text.trim().split("\n").map((s) => s.split(""));

north(grid);
solution.part1 = totalLoad(grid);

grid = text.trim().split("\n").map((s) => s.split(""));
const seen = new Map<string, number>();
const key = (g: string[][]) => g.map((r) => r.join("")).join("\n");
let force = false;
for (let cycle = 0; cycle < 1000000000;) {
    const k = key(grid);
    if (!force && seen.has(k)) {
        const sz = cycle - seen.get(k)!;
        while (cycle + sz < 1000000000)
            cycle += sz;
        force = true;
        continue;
    }
    seen.set(k, cycle);

    for (let i = 0; i < 4; ++i) {
        north(grid);
        grid = rot(grid);
    }

    ++cycle;
}

solution.part2 = totalLoad(grid);

console.log(solution);
