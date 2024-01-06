import { init as z3Init } from "z3-solver";

const { Context } = await z3Init();

const Z3 = Context("day24");

const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day24.txt").text();

const hailstones = text.trim().split("\n").map(l => {
    return l.replace(" @", ", ").split(", ").map(n => parseInt(n.trim()));
});

for (let i = 0; i < hailstones.length; ++i) {
    const [x1, y1, _z1, vx1, vy1, _vz1] = hailstones[i];
    const [x2, y2] = [x1 + vx1, y1 + vy1];

    for (let j = 0; j < i; ++j) {
        const [x3, y3, _z3, vx3, vy3, _vz3] = hailstones[j];
        const [x4, y4] = [x3 + vx3, y3 + vy3];

        const den = ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
        if (den === 0) {
            continue;
        }

        const px = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / den;
        const py = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / den;
        const aInside = (px > x1) == (x2 > x1);
        const bInside = (px > x3) == (x4 > x3);

        if (!aInside || !bInside)
            continue;

        if (px < 200000000000000 || px > 400000000000000)
            continue;
        if (py < 200000000000000 || py > 400000000000000)
            continue;

        solution.part1++;
    }
}

const [X, Y, Z, VX, VY, VZ] = Z3.Int.consts(["x", "y", "z", "vx", "vy", "vz"]);

const solver = new Z3.Solver();
for (let i = 0; i < 3; ++i) {
    const [x, y, z, vx, vy, vz] = hailstones[i];

    const T = Z3.Int.const(`t${i}`);

    solver.add(Z3.Eq(
        T.mul(vx).add(x).sub(T.mul(VX).add(X)),
        0
    ));
    solver.add(Z3.Eq(
        T.mul(vy).add(y).sub(T.mul(VY).add(Y)),
        0
    ));
    solver.add(Z3.Eq(
        T.mul(vz).add(z).sub(T.mul(VZ).add(Z)),
        0
    ));
}

console.log(await solver.check());

solution.part2 = parseInt(solver.model().eval(X.add(Y).add(Z)).toString());

console.log(solution);
