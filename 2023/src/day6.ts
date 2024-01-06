function run(time: number, distance: number): number {
    let win = 0;
    for (let hold_t = 0; hold_t <= time; ++hold_t) {
        const vel = hold_t;
        const mm = (time - hold_t) * vel;
        if (mm > distance)
            ++win;
    }
    return win;
}

const solution = {
    part1: [[38, 241], [94, 1549], [79, 1074], [70, 1091]].reduce((acc, e) => acc * run(e[0], e[1]), 1),
    part2: run(38947970, 241154910741091)
};

console.log(solution);
