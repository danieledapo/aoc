const solution = {
  part1: 0,
  part2: 0,
};

const text = await Bun.file("input/day13.txt").text();

const grids = text.trim().split("\n\n").map((s) => s.split("\n"));

function reflectionPoint(grid: string[], maxDiff = 0, rr = -1, cc = -1): [numer, number] {
  const [w,h] = [grid[0].length, grid.length];

  let mc = 0;
  for (let c = 1; c < w; ++c) {
    if (c === cc)
      continue;

    let mirrored = true;
    let diff = maxDiff;
    for (let dc = 0; mirrored && c - 1 - dc >= 0 && c + dc < w; ++dc) {
      for (let r = 0; r < h; ++r) {
        if (grid[r][c-1-dc] !== grid[r][c+dc] && --diff < 0) {
          mirrored = false;
          break;
        }
      }
    }

    if (mirrored) {
      mc = c;
      break;
    }
  }

  let mr = 0;
  for (let r = 1; r < h; ++r) {
    if (r === rr)
      continue;

    let mirrored = true;
    let diff = maxDiff;
    for (let dr = 0; mirrored && r - 1 - dr >= 0 && r + dr < h; ++dr) {
      for (let c = 0; c < w; ++c) {
        if (grid[r-1-dr][c] !== grid[r+dr][c] && --diff < 0) {
          mirrored = false;
          break;
        }
      }
    }

    if (mirrored) {
      mr = r;
      break;
    }
  }

  return [mr, mc];
}

for (const grid of grids) {
  const [mr1, mc1] = reflectionPoint(grid);
  solution.part1 += mc1 + mr1 * 100;

  const [mr2, mc2] = reflectionPoint(grid, 1, mr1, mc1);
  solution.part2 += mc2 + mr2 * 100;
}

console.log(solution);
