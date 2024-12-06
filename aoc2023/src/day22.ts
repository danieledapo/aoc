const solution = {
    part1: 0,
    part2: 0,
};

interface Brick {
    x0: number,
    y0: number,
    z0: number,
    x1: number,
    y1: number,
    z1: number,
}

function brickOverlaps(b0: Brick, b1: Brick): boolean {
    return Math.max(b0.x0, b1.x0) <= Math.min(b0.x1, b1.x1) &&
        Math.max(b0.y0, b1.y0) <= Math.min(b0.y1, b1.y1);
}

const text = await Bun.file("input/day22.txt").text();

const bricks: Brick[] = [];
for (const l of text.trim().split("\n")) {
    const [p0, p1] = l.split("~");
    const [x0, y0, z0] = p0.split(",").map(s => parseInt(s));
    const [x1, y1, z1] = p1.split(",").map(s => parseInt(s));

    bricks.push({ x0, y0, z0, x1, y1, z1 });
}

bricks.sort((b0, b1) => b0.z0 - b1.z0);

for (let i = 0; i < bricks.length; ++i) {
    let z = 1;
    for (let j = 0; j < i; ++j) {
        if (brickOverlaps(bricks[i], bricks[j]))
            z = Math.max(z, bricks[j].z1 + 1);
    }

    bricks[i].z1 -= bricks[i].z0 - z;
    bricks[i].z0 = z;
}

bricks.sort((b0, b1) => b0.z0 - b1.z0);

const supporting = new Map<Brick, Set<Brick>>(bricks.map(b => [b, new Set()]));
const supportedBy = new Map<Brick, Set<Brick>>(bricks.map(b => [b, new Set()]));

for (let j = 0; j < bricks.length; ++j) {
    for (let i = 0; i < j; ++i) {
        if (brickOverlaps(bricks[j], bricks[i]) && bricks[j].z0 === bricks[i].z1 + 1) {
            supporting.get(bricks[i])!.add(bricks[j]);
            supportedBy.get(bricks[j])!.add(bricks[i]);
        }
    }
}

for (const brick of bricks) {
    let only_supporting = false;
    for (const b of supporting.get(brick)!) {
        if (supportedBy.get(b)!.size < 2) {
            only_supporting = true;
            break;
        }
    }

    if (!only_supporting)
        ++solution.part1;
}

for (const brick of bricks) {
    const stack = [brick];
    const falling = new Set([brick]);

    while (stack.length > 0) {
        const removed = stack.pop()!;

        for (const b of supporting.get(removed)!) {
            if (falling.has(b))
                continue;

            let no_support = true;
            for (const bb of supportedBy.get(b)!) {
                if (!falling.has(bb)) {
                    no_support = false;
                    break;
                }
            }

            if (no_support) {
                stack.push(b);
                falling.add(b);
            }
        }

    }

    solution.part2 += falling.size - 1;
}

console.log(solution);
