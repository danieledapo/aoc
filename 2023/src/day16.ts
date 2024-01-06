const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day16.txt").text();

const grid = text.trim().split("\n").map(s => s.split(""));
const [w, h] = [grid[0].length, grid.length];

function score(r0: number, c0: number, dr: number, dc: number): number {
    const energized = new Set<number>();
    const seen = new Set<string>();

    let pos = [[r0, c0, dr, dc]];
    while (pos.length > 0) {
        const newPos: typeof pos = [];

        for (const [r, c, dr, dc] of pos) {
            if (r < 0 || r >= h || c < 0 || c >= w)
                continue;


            const k = `${r * w + c}-${dr}-${dc}`;
            if (seen.has(k))
                continue;

            seen.add(k);
            energized.add(r * w + c);

            if (grid[r][c] === "/") {
                newPos.push([r - dc, c - dr, -dc, -dr]);
            } else if (grid[r][c] === "\\") {
                newPos.push([r + dc, c + dr, dc, dr]);
            } else if (grid[r][c] === "|" && dr === 0) {
                newPos.push([r - 1, c, -1, 0], [r + 1, c, 1, 0]);
            } else if (grid[r][c] === "-" && dc === 0) {
                newPos.push([r, c - 1, 0, -1], [r, c + 1, 0, 1]);
            } else {
                newPos.push([r + dr, c + dc, dr, dc]);
            }
        }

        pos = newPos;
    }

    return energized.size;
}

solution.part1 = score(0, 0, 0, 1);

for (let c = 0; c < w; ++c)
    solution.part2 = Math.max(solution.part2, score(0, c, 1, 0), score(h - 1, c, -1, 0));
for (let r = 0; r < h; ++r)
    solution.part2 = Math.max(solution.part2, score(r, 0, 0, 1), score(r, w - 1, 0, -1));


console.log(solution);
