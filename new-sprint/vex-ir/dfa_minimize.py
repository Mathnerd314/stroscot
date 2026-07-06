#!/usr/bin/env python3
"""
Python port of Antti Valmari's fast DFA minimization.
  Information Processing Letters 112 (2012) 213-217

Usage: echo "nn mm q0 ff\n<transitions>\n<final states>" | python3 dfa_minimize.py
Input format:
  Line 1: nn mm q0 ff  (states, transitions, initial state, # final states)
  Next mm lines: tail label head  (one transition per line)
  Next ff lines: final state numbers
"""

import sys

# ---------------------------------------------------------------------------
# Global arrays shared by partition methods (mirrors C++ globals M, W, w)
# ---------------------------------------------------------------------------
M = []       # marked count per block/cord
W = []       # worklist of blocks/cords to split (used as stack)
w = 0        # top of W stack (we'll use a list and pop instead)


class Partition:
    def __init__(self):
        self.z = 0
        self.E = None  # elements
        self.L = None  # location of each element in E
        self.S = None  # set (block/cord) id of each element
        self.F = None  # first index in E for set s
        self.P = None  # past-last index in E for set s

    def init(self, n):
        self.z = 1 if n else 0
        self.E = list(range(n))
        self.L = list(range(n))
        self.S = [0] * n
        self.F = [0] * n
        self.P = [0] * n
        if self.z:
            self.F[0] = 0
            self.P[0] = n

    def mark(self, e):
        global M, W, w
        s = self.S[e]
        i = self.L[e]
        j = self.F[s] + M[s]
        # swap E[i] and E[j]
        self.E[i] = self.E[j]
        self.L[self.E[i]] = i
        self.E[j] = e
        self.L[e] = j
        if M[s] == 0:
            W[w] = s
            w += 1
        M[s] += 1

    def split(self):
        global M, W, w
        while w:
            w -= 1
            s = W[w]
            j = self.F[s] + M[s]
            if j == self.P[s]:
                M[s] = 0
                continue
            if M[s] <= self.P[s] - j:
                self.F[self.z] = self.F[s]
                self.P[self.z] = j
                self.F[s] = j
            else:
                self.P[self.z] = self.P[s]
                self.F[self.z] = j
                self.P[s] = j
            for i in range(self.F[self.z], self.P[self.z]):
                self.S[self.E[i]] = self.z
            M[s] = 0
            M[self.z] = 0
            self.z += 1


B = Partition()
C = Partition()

nn = mm = ff = q0 = 0
T = []    # tails
Lbl = []  # labels
H = []    # heads

A = []    # adjacency list
Fa = []   # adjacency offsets (F in C++)

rr = 0    # reached state count


def make_adjacent(K):
    global A, Fa
    Fa = [0] * (nn + 1)
    for t in range(mm):
        Fa[K[t]] += 1
    for q in range(nn):
        Fa[q + 1] += Fa[q]
    A = [0] * mm
    for t in range(mm - 1, -1, -1):
        Fa[K[t]] -= 1
        A[Fa[K[t]]] = t


def reach(q):
    global rr
    i = B.L[q]
    if i >= rr:
        B.E[i] = B.E[rr]
        B.L[B.E[i]] = i
        B.E[rr] = q
        B.L[q] = rr
        rr += 1


def rem_unreachable(Tv, Hv):
    global mm, rr
    make_adjacent(Tv)
    i = 0
    while i < rr:
        q = B.E[i]
        for j in range(Fa[q], Fa[q + 1]):
            reach(Hv[A[j]])
        i += 1
    j = 0
    for t in range(mm):
        if B.L[Tv[t]] < rr:
            Hv[j] = Hv[t]
            Lbl[j] = Lbl[t]
            Tv[j] = Tv[t]
            j += 1
    mm = j
    B.P[0] = rr
    rr = 0


def main():
    global nn, mm, ff, q0, T, Lbl, H, M, W, w, rr

    data = sys.stdin.read().split()
    idx = 0
    nn = int(data[idx]);  mm = int(data[idx+1])
    q0 = int(data[idx+2]); ff = int(data[idx+3])
    idx += 4

    T   = [0] * mm
    Lbl = [0] * mm
    H   = [0] * mm
    B.init(nn)
    # Pre-allocate F and P large enough for up to nn blocks
    B.F = [0] * (nn + 1)
    B.P = [0] * (nn + 1)
    B.S = [0] * nn
    if nn:
        B.F[0] = 0; B.P[0] = nn

    for t in range(mm):
        T[t]   = int(data[idx])
        Lbl[t] = int(data[idx+1])
        H[t]   = int(data[idx+2])
        idx += 3

    # Allocate M and W globally (size mm+1 is safe upper bound for both B and C)
    M = [0] * (mm + 2)
    W = [0] * (mm + 2)
    w = 0

    # Remove forward-unreachable states
    reach(q0)
    rem_unreachable(T, H)

    # Mark final states and remove backward-unreachable states
    for _ in range(ff):
        q = int(data[idx]); idx += 1
        if B.L[q] < B.P[0]:
            reach(q)
    ff = rr
    rem_unreachable(H, T)

    # Initial partition: final vs non-final
    M[0] = ff
    if ff:
        W[w] = 0; w += 1
        B.split()

    # Transition partition: group by label
    C.init(mm)
    C.F = [0] * (mm + 1)
    C.P = [0] * (mm + 1)
    C.S = [0] * mm
    if mm:
        C.E.sort(key=lambda t: Lbl[t])
        C.z = 0
        M[0] = 0
        a = Lbl[C.E[0]]
        for i in range(mm):
            t = C.E[i]
            if Lbl[t] != a:
                a = Lbl[t]
                C.P[C.z] = i
                C.z += 1
                C.F[C.z] = i
                M[C.z] = 0
            C.S[t] = C.z
            C.L[t] = i
        C.P[C.z] = mm
        C.z += 1

    # Split blocks and cords
    make_adjacent(H)
    b = 1
    c = 0
    while c < C.z:
        for i in range(C.F[c], C.P[c]):
            B.mark(T[C.E[i]])
        B.split()
        c += 1
        while b < B.z:
            for i in range(B.F[b], B.P[b]):
                q = B.E[i]
                for j in range(Fa[q], Fa[q + 1]):
                    C.mark(A[j])
            C.split()
            b += 1

    # Count output transitions and final states
    mo = sum(1 for t in range(mm) if B.L[T[t]] == B.F[B.S[T[t]]])
    fo = sum(1 for blk in range(B.z) if B.F[blk] < ff)

    # Output
    lines = [f"{B.z} {mo} {B.S[q0]} {fo}"]
    for t in range(mm):
        if B.L[T[t]] == B.F[B.S[T[t]]]:
            lines.append(f"{B.S[T[t]]} {Lbl[t]} {B.S[H[t]]}")
    for blk in range(B.z):
        if B.F[blk] < ff:
            lines.append(str(blk))
    print("\n".join(lines))


if __name__ == "__main__":
    main()
