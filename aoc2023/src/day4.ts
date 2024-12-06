const solution = {
    part1: 0,
    part2: 0,
};

interface ScratchCard {
    winning: Set<number>,
    found: Set<number>,
    matches: number,
}

const text = (await Bun.file("input/day4.txt").text()).trim();

const scratchcards: ScratchCard[] = [];
const stack: number[] = [];

for (const l of text.split("\n")) {
    const nums = l.split(": ")[1].split(" | ");

    const winning = new Set(nums[0].trim().split(/\s+/).map((s) => parseInt(s)));
    const found = new Set(nums[1].trim().split(/\s+/).map((s) => parseInt(s)));

    let matches = 0;
    for (const n of found)
        if (winning.has(n))
            ++matches;

    scratchcards.push({ winning, found, matches });

    if (matches > 0) {
        solution.part1 += matches > 0 ? Math.pow(2, matches - 1) : 0;
        stack.push(scratchcards.length - 1);
    } else
        ++solution.part2;
}

while (stack.length > 0) {
    const cardid = stack.pop()!;
    const card = scratchcards[cardid];
    ++solution.part2;
    for (let i = 0; i < card.matches; ++i)
        stack.push(cardid + i + 1);
}

console.log(solution);
