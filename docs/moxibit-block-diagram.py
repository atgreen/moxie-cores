"""Block diagram of the moxibit core, in the style of mox125-block-diagram.py."""
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch

INK = "#1f2933"
MUTED = "#52606d"
STAGE = "#eef2f7"
STAGE_EDGE = "#7b8794"
UNIT = "#ffffff"
ACCENT = "#1d6fa5"   # data path
CTRL = "#b4491c"     # control
BUS = "#2f7d4f"      # wishbone

fig, ax = plt.subplots(figsize=(16, 9.6), dpi=150)
ax.set_xlim(0, 160)
ax.set_ylim(0, 96)
ax.axis("off")


def box(x, y, w, h, fc, ec, lw=1.4, r=1.2):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle=f"round,pad=0,rounding_size={r}",
                                fc=fc, ec=ec, lw=lw, zorder=2))


def stage(x, y, w, h, title, sub):
    box(x, y, w, h, STAGE, STAGE_EDGE, lw=1.8, r=1.6)
    ax.text(x + 2, y + h - 3.2, title, fontsize=15, weight="bold", color=INK, va="center", zorder=3)
    ax.text(x + w - 2, y + h - 3.3, sub, fontsize=8.6, color=MUTED, va="center", ha="right", zorder=3)


def unit(x, y, w, h, title, detail=None):
    box(x, y, w, h, UNIT, STAGE_EDGE, lw=1.1, r=0.9)
    if detail and detail.count("\n") >= 2:
        # A tall box: the title at the top, the detail under it.
        ax.text(x + w / 2, y + h - 2.4, title, fontsize=10.5, weight="bold", color=INK,
                ha="center", va="center", zorder=3)
        ax.text(x + w / 2, y + (h - 4.2) / 2, detail, fontsize=8.6, color=MUTED,
                ha="center", va="center", zorder=3, linespacing=1.25)
    elif detail:
        ax.text(x + w / 2, y + h / 2 + 1.5, title, fontsize=10.5, weight="bold", color=INK,
                ha="center", va="center", zorder=3)
        ax.text(x + w / 2, y + h / 2 - 1.7, detail, fontsize=8.6, color=MUTED,
                ha="center", va="center", zorder=3, linespacing=1.25)
    else:
        ax.text(x + w / 2, y + h / 2, title, fontsize=10.5, weight="bold", color=INK,
                ha="center", va="center", zorder=3)


def label(x, y, text, color, ha="center"):
    ax.text(x, y, text, fontsize=8.8, color=color, ha=ha, va="center", zorder=5, linespacing=1.25,
            bbox=dict(fc="white", ec="none", pad=1.2, alpha=0.92))


def arrow(p, q, color, text=None, lpos=None, style="-|>", lw=2.0):
    ax.add_patch(FancyArrowPatch(p, q, arrowstyle=style, mutation_scale=16, lw=lw, color=color,
                                 zorder=4, shrinkA=0, shrinkB=0))
    if text:
        label(*lpos, text, color)


def path(points, color, lw=2.0):
    xs, ys = zip(*points[:-1])
    ax.plot(list(xs), list(ys), color=color, lw=lw, zorder=4, solid_capstyle="round",
            solid_joinstyle="round")
    arrow(points[-2], points[-1], color, lw=lw)


# Title
ax.text(4, 92.5, "moxibit", fontsize=24, weight="bold", color=INK, va="center")
ax.text(25.5, 92.2, "bit-serial Moxie core  ·  microcoded  ·  big-endian  ·  "
        "16-bit Wishbone data, 32-bit address", fontsize=12, color=MUTED, va="center")

# Panels
SY, SH = 46, 38
stage(4, SY, 50, SH, "Sequencer", "microcode · block RAM")
stage(62, SY, 94, SH, "Serial datapath", "1 bit per clock · a pass is 32 clocks, LSB first")

# Sequencer internals
unit(7, 69, 21, 8, "Instruction", "16 bits")
unit(30, 69, 21, 8, "Dispatch ROM", "512 x 8 · by opcode")
unit(7, 59.5, 44, 8, "Microcode ROM",
     "256 x 61-bit µops: a pass, a bus cycle or a wait")
unit(7, 50, 44, 8, "µPC",
     "next µop, or a two-way branch on zero, irq or done")

# Datapath internals
W = 28.5
for i, (t, d) in enumerate((("ALU adder", "add, sub, logic, mov\ncmp sets 5 condition codes"),
                            ("PC adder", "+2 by a carry into bit 1\nbranch offsets, constants"),
                            ("MDU adder", "multiply and divide steps\nleft out when MDU = 0"))):
    unit(65 + i * (W + 1.5), 68, W, 9, t, d)
for i, (t, d) in enumerate((("Shift tap", "DBUF[sh]: one pass\nfor any shift amount"),
                            ("Extend", "zero- or sign-extend\na byte or a short"),
                            ("Status", "interrupt enable, condition\ncodes · gsr, ssr, eret"))):
    unit(65 + i * (W + 1.5), 57.5, W, 9, t, d)
unit(65, 48, 88.5, 8, "Operand and result selection",
     "chosen by the µop: ports A and B, ADR, DBUF, immediate, constant, status")

# Lower row: what the datapath streams through
unit(62, 19, 36, 13, "Register file",
     "32 x 32 bits in one 512 x 2 RAM\nfp, sp, r0-r13, sr1-sr7, scratch\nport A in even clocks, B in odd")
unit(102, 19, 25, 13, "ADR", "32-bit shift register\nthe bus address, and\nthe PC in between")
unit(131, 19, 25, 13, "DBUF", "32-bit shift register\nbus data in at [15:0],\nout from [31:16]")

box(4, 3, 152, 8.5, "#e9f4ec", BUS, lw=1.8, r=1.4)
ax.text(80, 8.6, "Wishbone master", fontsize=12.5, weight="bold", color=INK, ha="center",
        va="center", zorder=3)
ax.text(80, 5.4, "one 16-bit bus for fetches and data  ·  a clock with cyc low between requests  ·  "
        "mox125's byte lanes, so it drops into the same SoCs", fontsize=9, color=MUTED,
        ha="center", va="center", zorder=3)

# Control between the sequencer and the datapath
arrow((54, 70), (62, 70), CTRL, "µop", (58, 73))
arrow((62, 55), (54, 55), CTRL, "zero\ndone", (58, 59.2))
path([(50, 46), (50, 25.5), (62, 25.5)], CTRL)
label(56, 29, "register\nselects", CTRL)

# Data between the datapath and what it streams through
arrow((72, 32), (72, 46), ACCENT, "ra, rb", (72, 39))
arrow((88, 46), (88, 32), ACCENT, "write", (88, 39))
arrow((114.5, 32), (114.5, 46), ACCENT, "aq", (114.5, 39), style="<|-|>")
arrow((143.5, 32), (143.5, 46), ACCENT, "dq", (143.5, 39), style="<|-|>")

# Bus connections
arrow((14, 11.5), (14, 46), BUS, "opcode", (14, 30))
arrow((114.5, 19), (114.5, 11.5), BUS, "address", (114.5, 15.3))
arrow((143.5, 19), (143.5, 11.5), BUS, "data", (143.5, 15.3), style="<|-|>")

# Legend
lx, ly = 20, 38
for i, (c, t) in enumerate(((ACCENT, "data, one bit per clock"), (CTRL, "control"), (BUS, "bus"))):
    ax.plot([lx, lx + 5], [ly - i * 3.2, ly - i * 3.2], color=c, lw=2.4, zorder=4)
    ax.text(lx + 7, ly - i * 3.2, t, fontsize=9, color=MUTED, va="center")

ax.text(156, 0.9, "iCE40, Yosys 0.67: 498 LUT4, 129 flops, 6 block RAMs  ·  "
        "382 LUT4, 121 flops with MDU = 0 and COMPACT = 0",
        fontsize=8.5, color=MUTED, ha="right", va="center")

fig.savefig(sys.argv[1], bbox_inches="tight", pad_inches=0.25, facecolor="white")
