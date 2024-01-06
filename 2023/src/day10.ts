const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day10.txt").text();

const maze = text.trim().split("\n");
const [width, height] = [maze[0].length, maze.length];
const hash = (r: number, c: number): number => { return r * width + c; }

const starts: { row: number, col: number, dx: number, dy: number }[] = [];
for (let row = 0; row < height; ++row) {
    for (let col = 0; col < width; ++col) {
        if (maze[row][col] !== "S")
            continue;

        const up = row <= 0 ? false : "|7F".includes(maze[row - 1][col]);
        const down = row + 1 >= height ? false : "|LJ".includes(maze[row + 1][col]);
        const left = col <= 0 ? false : "-LF".includes(maze[row][col - 1]);
        const right = col + 1 >= width ? false : "-7J".includes(maze[row][col + 1]);

        if (up)
            starts.push({ row, col, dx: 0, dy: -1 });

        if (down)
            starts.push({ row, col, dx: 0, dy: 1 });

        if (left)
            starts.push({ row, col, dx: -1, dy: 0 });

        if (right)
            starts.push({ row, col, dx: 1, dy: 0 });
    }
}

const visited = new Map<number, number>();

let loop: [number, number][] = [];

for (let { row, col, dx, dy } of starts) {
    visited.delete(hash(row, col));

    const curLoop: [number, number][] = [];

    for (let pass = 0; /* */; ++pass) {
        const c = visited.get(hash(row, col));
        if (c && pass >= c)
            break;

        curLoop.push([row, col]);
        visited.set(hash(row, col), pass);

        row += dy;
        col += dx;

        if (maze[row][col] === "S") {
            curLoop.push([row, col]);
            loop = curLoop;
            break;
        }

        if (maze[row][col] === "L" || maze[row][col] === "7")
            [dx, dy] = [dy, dx];
        else if (maze[row][col] === "J" || maze[row][col] === "F")
            [dx, dy] = [-dy, -dx];
        else if (maze[row][col] === "-")
            dy = 0;
        else if (maze[row][col] === "|")
            dx = 0;
    }
}

// make loop run clockwise
let area = 0;
for (let i = 0; i < loop.length - 1; ++i)
    area += (loop[i + 1][1] - loop[i][1]) * (loop[i + 1][0] + loop[i][0]);
if (area > 0)
    loop.reverse();

const stack: [number, number][] = [];
for (let i = 1; i < loop.length; ++i) {
    const [r0, c0] = loop[i - 1];
    const [r1, c1] = loop[i];
    const [dx, dy] = [c1 - c0, r1 - r0];

    stack.push([r0 + dx, c0 - dy], [r1 + dx, c0 - dy]);
}

const loopIx = new Set<number>(loop.map((a) => hash(a[0], a[1])));
const inside = new Set<number>();
while (stack.length) {
    const [r, c] = stack.pop()!;
    const h = hash(r, c);

    if (loopIx.has(h))
        continue;

    if (inside.has(h))
        continue;

    inside.add(h);
    if (r > 0)
        stack.push([r - 1, c]);
    if (r + 1 < height)
        stack.push([r + 1, c]);
    if (c > 0)
        stack.push([r, c - 1]);
    if (c + 1 < width)
        stack.push([r, c + 1]);
}

solution.part1 = Math.max(...visited.values());
solution.part2 = inside.size;

console.log(solution);
