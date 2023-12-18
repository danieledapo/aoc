const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day18.txt").text();

const DELTA = new Map([
    ["R", [1, 0]],
    ["L", [-1, 0]],
    ["D", [0, 1]],
    ["U", [0, -1]],
]);

const loop1: [number, number][] = [[0, 0]];
const loop2: [number, number][] = [[0, 0]];
for (const l of text.trim().split("\n")) {
    let [dir, step_s, color] = l.split(" ");
    color = color.slice(2, color.length - 1);

    let [dx, dy] = DELTA.get(dir)!;
    let step = parseInt(step_s);
    loop1.push([
        loop1.at(-1)![0] + dx * step,
        loop1.at(-1)![1] + dy * step,
    ]);

    [dx, dy] = DELTA.get("RDLU"[parseInt(color.at(-1)!, 16)])!;
    step = parseInt(color.slice(0, color.length - 1), 16);
    loop2.push([
        loop2.at(-1)![0] + dx * step,
        loop2.at(-1)![1] + dy * step,
    ]);
}

function area(loop: [number, number][]): number {
    let n = 0;
    let a = 0;
    for (let i = 0; i < loop.length; ++i) {
        const [_, py] = loop.at(i - 1)!;
        const [xx, yy] = loop[i];
        const [nx, ny] = loop[(i + 1) % loop.length];

        n += Math.abs(nx - xx) + Math.abs(ny - yy);
        a += xx * (py - ny);
    }

    a = Math.floor(Math.abs(a) / 2);
    const ins = a - Math.floor(n / 2) + 1;

    return ins + n;
}

solution.part1 = area(loop1);
solution.part2 = area(loop2);

console.log(solution);
