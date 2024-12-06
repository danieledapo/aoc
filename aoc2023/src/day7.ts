const solution = {
    part1: 0,
    part2: 0,
};

function handType(hand: string, joker: boolean): number {
    const counts = new Map<string, number>();
    let jokers = 0;

    for (const c of hand) {
        if (joker && c === "J")
            ++jokers;
        else
            counts.set(c, (counts.get(c) || 0) + 1);
    }

    const values = [...counts.values()];
    values.sort((a, b) => b - a);

    values[0] = (values[0] || 0) + jokers;

    if (values[0] === 5)
        return 6;

    if (values[0] === 4)
        return 5;

    if (values[0] === 3 && values[1] === 2)
        return 4;

    if (values[0] === 3 && values[1] === 1)
        return 3;

    if (values[0] === 2 && values[1] === 2)
        return 2;

    if (values[0] === 2 && values[1] === 1)
        return 1;

    return 0;
}


function winnings(cardValues: Map<string, number>, hands: { hand: string, bid: number }[], joker: boolean): number {
    hands.sort((a, b) => {
        const ta = handType(a.hand, joker);
        const tb = handType(b.hand, joker);
        if (ta == tb) {
            for (let i = 0; i < 5; ++i) {
                const d = cardValues.get(a.hand[i])! - cardValues.get(b.hand[i])!;
                if (d !== 0)
                    return d;
            }
        }

        return ta - tb;
    });

    return hands.reduce((w, hand, rank) => {
        return w + (rank + 1) * hand.bid;
    }, 0);
}

const text = await Bun.file("input/day7.txt").text();

const hands = text.trim().split("\n").map((l) => {
    const [hand, bid] = l.split(" ");
    return { hand: hand, bid: parseInt(bid) };
});

const cardValues = new Map<string, number>();
for (const card of 'AKQJT98765432')
    cardValues.set(card, -cardValues.size);

solution.part1 = winnings(cardValues, hands, false);

cardValues.set("J", -100);
solution.part2 = winnings(cardValues, hands, true);

console.log(solution);
