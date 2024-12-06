const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day12.txt").text();

function possibleStates(puzzle: string[], counts: number[], i = 0, ci = 0, cache = new Map<number, number>()): number {
    const save = (n: number) => {
        cache.set(i * counts.length + ci, n);
        return n;
    };

    while (i < puzzle.length) {
        const v = cache.get(i * counts.length + ci);
        if (v !== undefined)
            return v;

        if (puzzle[i] === ".") {
            ++i;
            continue;
        }

        if (puzzle[i] === "#") {
            if (ci >= counts.length || i + counts[ci] - 1 >= puzzle.length)
                return save(0);

            for (let j = 0; j < counts[ci]; ++j) {
                if (puzzle[i] === ".")
                    return save(0);
                puzzle[i++] = "#";
            }

            if (i < puzzle.length) {
                if (puzzle[i] === "#")
                    return save(0);
                puzzle[i] = '.';
            }

            ++ci;
            continue;
        }

        if (puzzle[i] === "?") {
            let damaged = 0;
            if (ci < counts.length) {
                for (let j = 0; i + j < puzzle.length && puzzle[i + j] !== "." && j < counts[ci]; ++j)
                    ++damaged;
            }

            const a = [...puzzle];
            a[i] = '.';
            let res = possibleStates(a, counts, i, ci, cache);
            if (damaged > 0 && damaged === counts[ci] && puzzle[i + damaged] !== "#") {
                const b = [...puzzle];
                for (let j = 0; j < damaged; ++j)
                    b[i + j] = '#';
                if (i + damaged < puzzle.length)
                    b[i + damaged] = '.';
                res += possibleStates(b, counts, i + damaged, ci + 1, cache);
            }

            return save(res);
        }

        throw "bug: unhandled char";
    }

    if (ci !== counts.length)
        return save(0);

    return save(1);
}

for (const l of text.trim().split("\n")) {
    const [puzzle, counts_] = l.split(" ");
    const counts = counts_.split(",").map((s) => parseInt(s));

    let puzzle2 = puzzle;
    const counts2: number[] = [...counts];
    for (let i = 0; i < 4; ++i) {
        puzzle2 += `?${puzzle}`;
        counts2.push(...counts);
    }

    solution.part1 += possibleStates(puzzle.split(''), counts);
    solution.part2 += possibleStates(puzzle2.split(''), counts2);
}


console.log(solution);
