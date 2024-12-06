const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day21.txt").text();

const grid = text.trim().split("\n");
const [w, h] = [grid[0].length, grid.length];

function fill(sr: number, sc: number, steps: number): number {
    const ans = new Set<number>();
    const seen = new Set([sr * w + sc]);
    const q = [[sr, sc, steps]];

    let i = 0;
    while (i < q.length) {
        const [r, c, s] = q[i++];

        if (s % 2 === 0)
            ans.add(r * w + c);

        if (s === 0)
            continue;

        for (const [rr, cc] of [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]]) {
            if (rr < 0 || rr >= h || cc < 0 || cc >= w)
                continue;

            if (grid[rr][cc] === '#')
                continue;

            if (seen.has(rr * w + cc))
                continue;

            seen.add(rr * w + cc);
            q.push([rr, cc, s - 1]);
        }
    }

    return ans.size;
}

let [sr, sc] = [-1, -1];
for (let r = 0; r < h; ++r)
    for (let c = 0; c < w; ++c)
        if (grid[r][c] === "S")
            [sr, sc] = [r, c];

solution.part1 = fill(sr, sc, 64);


const steps = 26501365;
const size = w;

const grid_width = Math.floor(steps / size) - 1;

const odd = (Math.floor(grid_width / 2) * 2 + 1) ** 2;
const even = (Math.floor((grid_width + 1) / 2) * 2) ** 2;

const odd_points = fill(sr, sc, size * 2 + 1);
const even_points = fill(sr, sc, size * 2);

const corner_t = fill(size - 1, sc, size - 1);
const corner_r = fill(sr, 0, size - 1);
const corner_b = fill(0, sc, size - 1);
const corner_l = fill(sr, size - 1, size - 1);

const small_tr = fill(size - 1, 0, Math.floor(size / 2) - 1);
const small_tl = fill(size - 1, size - 1, Math.floor(size / 2) - 1);
const small_br = fill(0, 0, Math.floor(size / 2) - 1);
const small_bl = fill(0, size - 1, Math.floor(size / 2) - 1);

const large_tr = fill(size - 1, 0, Math.floor(size * 3 / 2) - 1);
const large_tl = fill(size - 1, size - 1, Math.floor(size * 3 / 2) - 1);
const large_br = fill(0, 0, Math.floor(size * 3 / 2) - 1);
const large_bl = fill(0, size - 1, Math.floor(size * 3 / 2) - 1);

solution.part2 = (
    odd * odd_points
    + even * even_points
    + corner_t
    + corner_r
    + corner_b
    + corner_l
    + (grid_width + 1) * (small_tr + small_tl + small_br + small_bl)
    + grid_width * (large_tr + large_tl + large_br + large_bl)
);


console.log(solution);
