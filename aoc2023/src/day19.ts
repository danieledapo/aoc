const solution = {
    part1: 0,
    part2: 0,
};

const text = await Bun.file("input/day19.txt").text();

const [rawWorkflows, rawParts] = text.trim().split("\n\n");

interface Workflow {
    rules: {
        k: string,
        op: string,
        n: number,
        dst: string
    }[]
    def: string
}

type Part = Map<string, { start: number, end: number }>;

function copyPart(p: Part): Part {
    const m = new Map();
    for (const [k, v] of p)
        m.set(k, { start: v.start, end: v.end });
    return m;
}

function acceptedCount(workflows: Map<string, Workflow>, part: Part, workflow = "in"): number {
    if (workflow === "R")
        return 0;

    if (workflow === "A") {
        return [...part.values()].reduce((a, r) => a * (r.end - r.start + 1), 1);
    }

    const w = workflows.get(workflow)!;

    let res = 0;
    let empty = false;
    for (const rule of w.rules) {
        let { start, end } = part.get(rule.k)!;
        let [tstart, tend] = [0, 0];

        if (rule.op === ">") {
            tstart = Math.max(start, rule.n + 1);
            tend = end;

            end = Math.min(rule.n, end);
        } else if (rule.op === "<") {
            tstart = start;
            tend = Math.min(rule.n - 1, end);

            start = Math.max(start, rule.n);
        } else
            throw "invalid op";

        if (tstart <= tend) {
            const newPart = copyPart(part);
            newPart.set(rule.k, { start: tstart, end: tend });
            res += acceptedCount(workflows, newPart, rule.dst);
        }

        if (start > end) {
            empty = true;
            break;
        }

        part = copyPart(part);
        part.set(rule.k, { start, end });
    }

    if (!empty)
        res += acceptedCount(workflows, copyPart(part), w.def);

    return res;
}

const workflows = new Map<string, Workflow>();
for (const w of rawWorkflows.split("\n")) {
    const [n, preds] = w.split("{");

    const workflow: Workflow = { rules: [], def: "" };

    for (const p of preds.slice(0, preds.length - 1).split(",")) {
        if (!p.includes(":")) {
            workflow.def = p;
            continue;
        }

        const [rule, dst] = p.split(":");
        workflow.rules.push({
            k: rule[0],
            op: rule[1],
            n: parseInt(rule.slice(2)),
            dst,
        })
    }

    workflows.set(n, workflow);
}

for (const p of rawParts.split("\n")) {
    const part: Part = new Map();
    for (const c of p.slice(1, p.length - 1).split(",")) {
        const [cat, n] = c.split("=");
        part.set(cat, { start: parseInt(n), end: parseInt(n) });
    }

    if (acceptedCount(workflows, part) > 0)
        solution.part1 += [...part.values()].reduce((a, b) => a + b.start, 0);
}

solution.part2 = acceptedCount(workflows, new Map([
    ["x", { start: 1, end: 4000 }],
    ["m", { start: 1, end: 4000 }],
    ["a", { start: 1, end: 4000 }],
    ["s", { start: 1, end: 4000 }],
]));

console.log(solution);
