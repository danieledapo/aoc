const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day9.txt").text();

function extrapolate(nums: number[], first: boolean): number {
    let all_zero = true;
    const deltas = [];

    for (let i = 1; i < nums.length; ++i) {
        deltas.push(nums[i] - nums[i - 1]);
        if (deltas.at(-1) !== 0)
            all_zero = false;
    }

    if (all_zero)
        return first ? nums.at(0)! : nums.at(-1)!;

    if (first)
        return nums.at(0)! - extrapolate(deltas, first);

    return nums.at(-1)! + extrapolate(deltas, first);
}

for (const l of text.trim().split("\n")) {
    const nums = l.split(" ").map((s) => parseInt(s));
    solution.part1 += extrapolate(nums, false);
    solution.part2 += extrapolate(nums, true);
}

console.log(solution);
