const solution = {
  part1: 0,
  part2: 0,
};

const text = await Bun.file("input/day11.txt").text();

const grid = text.trim().split("\n");
const [width, height] = [grid[0].length, grid.length];

const emptyRows = new Set<number>();
for (let r = 0; r < height; ++r) {
  let empty = true;
  for (let c = 0; c < width; ++c) {
    if (grid[r][c] !== ".") {
      empty = false;
      break;
    }
  }
  if (empty)
    emptyRows.add(r);
}

const emptyCols = new Set<number>();
for (let c = 0; c < width; ++c) {
  let empty = true;
  for (let r = 0; r < height; ++r) {
    if (grid[r][c] !== ".") {
      empty = false;
      break;
    }
  }
  if (empty)
    emptyCols.add(c);
}

const galaxies: [number, number][] = [];
for (let r = 0; r < height; ++r) {
  for (let c = 0; c < width; ++c) {
    if (grid[r][c] === "#")
      galaxies.push([r, c]);
  }
}

function solve(expand: number): number {
  let res = 0;

  for (let i = 0; i < galaxies.length; ++i) {
    const [r0, c0] = galaxies[i];
    for (let j = i + 1; j < galaxies.length; ++j) {
      const [r1, c1] = galaxies[j];

      for (let c = c0; c !== c1; c += Math.sign(c1 - c0))
        res += emptyCols.has(c) ? expand : 1;

      for (let r = r0; r !== r1; r += Math.sign(r1 - r0))
        res += emptyRows.has(r) ? expand : 1;
    }
  }

  return res;
}

solution.part1 = solve(2);
solution.part2 = solve(1000000);

console.log(solution);
