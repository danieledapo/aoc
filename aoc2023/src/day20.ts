const solution = {
    part1: 0,
    part2: 0,
};

function gcd(a: number, b: number): number {
    return b === 0 ? a : gcd(b, a % b);
}

function lcm(a: number, b: number) {
    return (a * b) / gcd(a, b);
}

interface Conj {
    type: "&",
    name: string,
    outputs: string[],
    memory: Map<string, boolean>,
}

interface FlipFlop {
    type: "%",
    name: string,
    outputs: string[],
    memory: boolean,
}

type Module = Conj | FlipFlop;

interface Pulse {
    from: string,
    signal: boolean,
    to: string
}

function propagatePulse(modules: Map<string, Module>, pulses: Pulse[], i: number) {
    const pulse = pulses[i];
    const mod = modules.get(pulse.to);
    if (!mod)
        return;

    let newSignal = false;

    if (mod.type === "&") {
        mod.memory.set(pulse.from, pulse.signal);

        for (const s of mod.memory.values()) {
            if (!s) {
                newSignal = true;
                break;
            }
        }
    }

    if (mod.type === "%") {
        if (pulse.signal)
            return;
        mod.memory = !mod.memory;
        newSignal = mod.memory;
    }


    for (const output of mod.outputs)
        pulses.push({ from: mod.name, signal: newSignal, to: output });
}

const modules: Map<string, Module> = new Map();
let broadcasted: string[] = [];

const text = await Bun.file("input/day20.txt").text();

for (const l of text.trim().split("\n")) {
    const [mod, o] = l.split(" -> ");
    const [type, name] = [mod[0], mod.slice(1)];
    const outputs = o.split(", ");

    if (mod === "broadcaster")
        broadcasted = outputs;
    else if (type === "&")
        modules.set(name, { type: "&", name, memory: new Map(), outputs });
    else
        modules.set(name, { type: "%", name, memory: false, outputs });
}

for (const [name, mod] of modules) {
    for (const out of mod.outputs) {
        const outMod = modules.get(out);
        if (outMod && outMod.type === "&")
            outMod.memory.set(name, false);
    }
}

let low = 0;
let hig = 0;
for (let j = 0; j < 1000; ++j) {
    low += 1;

    const pulses = broadcasted.map(n => { return { from: "broadcaster", signal: false, to: n } });
    let i = 0;
    while (i < pulses.length) {
        if (pulses[i].signal)
            ++hig;
        else
            ++low;

        propagatePulse(modules, pulses, i);
        ++i;
    }
}

solution.part1 = low * hig;

for (const mod of modules.values()) {
    if (mod.type === "&")
        for (const p of mod.memory.keys())
            mod.memory.set(p, false);
    else
        mod.memory = false;
}

const [{ name: feed }] = [...modules.values()].filter(m => m.outputs.includes("rx"));
const cycles = new Map([...modules.values()].filter(m => m.outputs.includes(feed)).map(m => [m.name, 0]));

let presses = 0;
while (solution.part2 === 0) {
    ++presses;

    const pulses = broadcasted.map(n => { return { from: "broadcaster", signal: false, to: n } });
    let i = 0;
    while (i < pulses.length) {
        if (pulses[i].signal && pulses[i].to === feed) {
            if (cycles.get(pulses[i].from) === 0)
                cycles.set(pulses[i].from, presses);

            let seenAll = true;
            for (const v of cycles.values()) {
                if (v === 0) {
                    seenAll = false;
                    break;
                }
            }

            if (seenAll) {
                solution.part2 = [...cycles.values()].reduce(lcm);
                break;
            }
        }

        propagatePulse(modules, pulses, i);
        ++i;
    }
}

console.log(solution);
