"""An artist's impression of a mox125 die.

This is not a layout: nothing here was placed or routed.  It is a
floorplan drawn to look like a die photograph, with block areas chosen
to roughly follow the synthesized design (the 8 KiB instruction cache
dominates, execute is the largest block of logic).

usage: mox125-die.py <output.png>
"""
import colorsys
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.patheffects as pe
import matplotlib.pyplot as plt
import numpy as np

N = 2400  # pixels per side
rng = np.random.default_rng(125)


def tint(gray, hue, sat=0.55, lo=0.10, hi=0.95):
    """Colour a 0..1 grayscale texture with one hue."""
    r, g, b = colorsys.hsv_to_rgb(hue, sat, 1.0)
    v = lo + (hi - lo) * gray
    return np.stack([v * r, v * g, v * b], axis=-1)


def logic(h, w, row=9):
    """Standard-cell rows: cells of random width between power rails."""
    img = np.zeros((h, w))
    for y in range(0, h, row):
        x = 0
        while x < w:
            cw = int(rng.integers(3, 16))
            img[y + 1:y + row - 1, x:x + cw - 1] = rng.uniform(0.25, 0.9)
            x += cw
        img[y:y + 1, :] = 1.0  # rail
    # Routing: a scattering of thin vertical wires on top.
    for _ in range(w // 3):
        x = int(rng.integers(0, w))
        y0 = int(rng.integers(0, h))
        img[y0:y0 + int(rng.integers(10, 90)), x:x + 1] += 0.25
    return np.clip(img, 0, 1)


def memory(h, w, cell=4, bank=96):
    """A memory array: a fine regular grid cut into banks."""
    y, x = np.mgrid[0:h, 0:w]
    img = 0.55 + 0.25 * np.cos(2 * np.pi * x / cell) * np.cos(2 * np.pi * y / cell)
    img += 0.08 * np.cos(2 * np.pi * y / (cell * 8))
    img[(y % bank) < 3] = 0.12
    img[:, :][(x % bank) < 3] = 0.12
    # Sense amplifiers and decoders along two edges.
    edge = max(10, min(h, w) // 14)
    img[-edge:, :] = 0.35 + 0.5 * logic(edge, w, row=5)
    img[:, :edge] = 0.35 + 0.5 * logic(h, edge, row=5)
    return np.clip(img, 0, 1)


def datapath(h, w, bits=32):
    """A regular bit-sliced structure such as a multiplier array."""
    y, x = np.mgrid[0:h, 0:w]
    slice_w = max(4, w // bits)
    img = 0.45 + 0.3 * np.cos(2 * np.pi * x / slice_w) + 0.15 * np.cos(2 * np.pi * (x + y) / 23)
    img[(x % slice_w) < 1] = 0.95
    img[(y % 37) < 2] = 0.2
    return np.clip(img, 0, 1)


# --- The die -------------------------------------------------------------
die = np.zeros((N, N, 3))
base = 0.05 + 0.02 * rng.random((N, N))
die[:] = tint(base, 0.62, sat=0.5, lo=0.0, hi=1.0)

# Faint filler logic everywhere inside the pad ring, so the gaps
# between blocks look like routing channels rather than bare silicon.
PAD = int(N * 0.075)
core = slice(PAD + 30, N - PAD - 30)
filler = logic(N - 2 * PAD - 60, N - 2 * PAD - 60, row=7)
die[core, core] = tint(filler, 0.60, sat=0.45, lo=0.03, hi=0.22)

# name, x0, y0, x1, y1 in percent of the die (y up), texture, hue, label
BLOCKS = [
    ("icache", 10, 39, 50, 90, "memory", 0.47, "Instruction cache\n8 KiB, 256 lines"),
    ("tags", 10, 31, 50, 37.5, "memory", 0.13, "cache tags"),
    ("fetch", 10, 10, 29.5, 29, "logic", 0.57, "Fetch\nPC, length decode"),
    ("decode", 31, 10, 50, 29, "logic", 0.76, "Decode\nmicrocode ROM"),
    ("execute", 52, 46, 90, 90, "logic", 0.90, "Execute\nALU, result mux,\nbus sequencer,\nspecial registers"),
    ("mul", 52, 31, 70, 44, "datapath", 0.08, "Multiplier"),
    ("div", 71.5, 31, 90, 44, "datapath", 0.33, "Divider"),
    ("regs", 52, 10, 71.5, 29, "memory", 0.70, "Register file\n16 x 32"),
    ("dtlb", 73, 20.5, 90, 29, "memory", 0.02, "Data TLB"),
    ("itlb", 73, 10, 90, 18.5, "memory", 0.18, "Instruction TLB"),
]

labels = []
for name, x0, y0, x1, y1, kind, hue, label in BLOCKS:
    c0, c1 = int(N * x0 / 100), int(N * x1 / 100)
    r0, r1 = int(N * (100 - y1) / 100), int(N * (100 - y0) / 100)
    h, w = r1 - r0, c1 - c0
    if kind == "memory":
        tex = memory(h, w, cell=4 if name == "icache" else 5,
                     bank=120 if name == "icache" else 64)
        sat = 0.6
    elif kind == "datapath":
        tex = datapath(h, w)
        sat = 0.55
    else:
        tex = logic(h, w)
        sat = 0.5
    # Thin-film interference: let the hue drift gently across the block.
    yy, xx = np.mgrid[0:h, 0:w]
    drift = 0.03 * np.sin(xx / w * 3.1 + yy / h * 2.3 + hue * 9)
    rgb = np.zeros((h, w, 3))
    hues = (hue + drift) % 1.0
    v = 0.12 + 0.8 * tex
    for i in range(3):
        rgb[..., i] = v
    # Vectorised HSV to RGB for a per-pixel hue.
    k = (hues * 6.0)
    f = k - np.floor(k)
    p, q, t = 1 - sat, 1 - sat * f, 1 - sat * (1 - f)
    sector = np.floor(k).astype(int) % 6
    table = [(1, t, p), (q, 1, p), (p, 1, t), (p, q, 1), (t, p, 1), (1, p, q)]
    for s, (rr, gg, bb) in enumerate(table):
        m = sector == s
        for i, comp in enumerate((rr, gg, bb)):
            comp = np.broadcast_to(comp, hues.shape)
            rgb[..., i][m] = v[m] * comp[m]
    die[r0:r1, c0:c1] = rgb
    # A dark guard ring around the block.
    for a, b, cc, dd in ((r0 - 4, r0, c0 - 4, c1 + 4), (r1, r1 + 4, c0 - 4, c1 + 4),
                         (r0, r1, c0 - 4, c0), (r0, r1, c1, c1 + 4)):
        die[a:b, cc:dd] *= 0.25
    labels.append(((x0 + x1) / 2, (y0 + y1) / 2, label, min(w, h)))

# Top-metal power grid, faint, over the whole core.
grid = np.zeros((N, N))
for k in range(PAD + 60, N - PAD - 60, 150):
    grid[:, k:k + 14] = 1
    grid[k:k + 14, :] = 1
grid[:PAD + 30, :] = grid[-PAD - 30:, :] = 0
grid[:, :PAD + 30] = grid[:, -PAD - 30:] = 0
die += 0.07 * grid[..., None] * np.array([1.0, 0.85, 0.55])

# Bond pads around the edge.
gold = np.array([0.93, 0.74, 0.32])
pads = 22
pitch = (N - 2 * PAD) / pads
size = int(pitch * 0.62)
inset = int(N * 0.018)
for i in range(pads):
    c = int(PAD + i * pitch + (pitch - size) / 2)
    for r0, c0 in ((inset, c), (N - inset - size, c), (c, inset), (c, N - inset - size)):
        shade = 0.75 + 0.25 * rng.random((size, size, 1))
        die[r0:r0 + size, c0:c0 + size] = gold * shade
        die[r0 + 5:r0 + size - 5, c0 + 5:c0 + size - 5] *= 1.08
        # The scar a probe or bond wire leaves.
        m = size // 2
        die[r0 + m - 6:r0 + m + 6, c0 + m - 10:c0 + m + 10] *= 0.7

# Seal ring and a soft vignette.
for k in (6, 12):
    die[k:k + 3, k:N - k] = die[N - k - 3:N - k, k:N - k] = 0.28
    die[k:N - k, k:k + 3] = die[k:N - k, N - k - 3:N - k] = 0.28
yy, xx = np.mgrid[0:N, 0:N]
radius = np.hypot(xx - N / 2, yy - N / 2) / (N / 2)
die *= (1.0 - 0.22 * np.clip(radius - 0.55, 0, 1))[..., None]
die = np.clip(die, 0, 1)

# --- Annotate ------------------------------------------------------------
fig = plt.figure(figsize=(12, 12.5), dpi=110, facecolor="#0b0d10")
ax = fig.add_axes([0.03, 0.065, 0.94, 0.905])
ax.imshow(die, extent=(0, 100, 0, 100), interpolation="lanczos")
ax.axis("off")
stroke = [pe.withStroke(linewidth=3.2, foreground="#000000")]
for x, y, label, small in labels:
    ax.text(x, y, label, color="white", ha="center", va="center", weight="bold",
            fontsize=13 if small > 300 else 9.5, linespacing=1.3, path_effects=stroke)

# The mask-layer signature every die seems to have.
ax.text(50, 94.2, "MOXIE  mox125", color="#d9b45a", ha="center", va="center",
        fontsize=11, family="monospace", weight="bold", alpha=0.9)
fig.text(0.5, 0.027, "mox125: an artist's impression of a die.  Not a real layout; "
         "block areas loosely follow the synthesized design.",
         color="#9aa5b1", ha="center", va="center", fontsize=11)
fig.savefig(sys.argv[1], facecolor=fig.get_facecolor())
