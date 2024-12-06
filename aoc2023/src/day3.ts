const solution = {
    part1: 0,
    part2: 0,
};

interface Entry {
    data: string,
    line: number,
    start_column: number,
}

const numbers: Entry[] = [];
const symbols: Entry[] = [];

const lines = (await Bun.file("input/day3.txt").text()).trim().split("\n");

for (let i = 0; i < lines.length; ++i) {
    const line = lines[i] + '.';

    let num = '';
    for (let col = 0; col < line.length; ++col) {
        if (line[col] >= '0' && line[col] <= '9') {
            num += line[col];
            continue;
        }

        if (num.length > 0) {
            numbers.push({
                data: num,
                line: i,
                start_column: col - num.length,
            });
            num = '';
        }

        if (line[col] !== '.') {
            symbols.push({
                data: line[col],
                line: i,
                start_column: col,
            });
        }
    }
}

const part_numbers = new Set<Entry>();

for (const sym of symbols) {
    const neighbors = [];
    for (const num of numbers.values()) {
        if (Math.abs(num.line - sym.line) > 1)
            continue;

        const xs0 = sym.start_column - 1, xe0 = sym.start_column + sym.data.length;
        const xs1 = num.start_column, xe1 = num.start_column + num.data.length - 1;

        if ((xs0 <= xs1 && xe0 >= xs1) || (xs0 <= xe1 && xe0 >= xe1))
            neighbors.push(num);
    }

    for (const n of neighbors) {
        if (!part_numbers.has(n)) {
            solution.part1 += parseInt(n.data);
            part_numbers.add(n);
        }
    }

    if (sym.data === "*" && neighbors.length === 2)
        solution.part2 += parseInt(neighbors[0].data) * parseInt(neighbors[1].data);

}

console.log(solution);
