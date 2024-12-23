#!/usr/bin/env python3

import sys

from itertools import combinations
from collections import defaultdict


def bron_kerbosch(connections):
    """
    Bron-Kerbosch algorithm to find all maximal cliques in an undirected graph.
    """

    def recurse(vertices, current_clique, tried):
        if not vertices and not tried:
            yield current_clique
            return

        for v in list(vertices):
            yield from recurse(
                vertices & connections[v],
                current_clique | {v},
                tried & connections[v],
            )
            vertices.remove(v)
            tried.add(v)

    yield from recurse(set(connections.keys()), set(), set())


connections = defaultdict(set)
for l in sys.stdin.readlines():
    l = l.strip()
    src, dst = l.split("-")

    connections[src].add(dst)
    connections[dst].add(src)


print(
    len(
        {
            tuple(sorted((src, b, c)))
            for src, dsts in connections.items()
            for b, c in combinations(dsts, 2)
            if any(pc.startswith("t") for pc in (src, b, c)) and c in connections[b]
        }
    )
)

max_clique = max(bron_kerbosch(connections), key=len)
print(",".join(sorted(max_clique)))
