"""Block diagram of the mox125 core."""
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
CTRL = "#b4491c"     # control / feedback
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
    if detail:
        ax.text(x + w / 2, y + h / 2 + 1.5, title, fontsize=10.5, weight="bold", color=INK,
                ha="center", va="center", zorder=3)
        ax.text(x + w / 2, y + h / 2 - 1.7, detail, fontsize=8.6, color=MUTED,
                ha="center", va="center", zorder=3, linespacing=1.25)
    else:
        ax.text(x + w / 2, y + h / 2, title, fontsize=10.5, weight="bold", color=INK,
                ha="center", va="center", zorder=3)


def arrow(p, q, color, label=None, lpos=None, style="-|>", rad=0.0, lw=2.0, ha="center", va="center"):
    ax.add_patch(FancyArrowPatch(p, q, arrowstyle=style, mutation_scale=16, lw=lw, color=color,
                                 connectionstyle=f"arc3,rad={rad}", zorder=4,
                                 shrinkA=0, shrinkB=0))
    if label:
        x, y = lpos
        ax.text(x, y, label, fontsize=8.8, color=color, ha=ha, va=va, zorder=5, linespacing=1.25,
                bbox=dict(fc="white", ec="none", pad=1.2, alpha=0.92))


def path(points, color, lw=2.0):
    xs, ys = zip(*points[:-1])
    ax.plot(list(xs), list(ys), color=color, lw=lw, zorder=4, solid_capstyle="round",
            solid_joinstyle="round")
    arrow(points[-2], points[-1], color, lw=lw)


# Title
ax.text(4, 92.5, "mox125", fontsize=24, weight="bold", color=INK, va="center")
ax.text(25, 92.2, "pipelined Moxie core  ·  big-endian  ·  16-bit Wishbone data, 32-bit address",
        fontsize=12, color=MUTED, va="center")

# Stages
SY, SH = 40, 38
stage(4, SY, 40, SH, "Fetch", "cpu_fetch · 73 LUT")
stage(52, SY, 34, SH, "Decode", "cpu_decode · 133 LUT")
stage(94, SY, 62, SH, "Execute", "cpu_execute · 1,903 LUT")

# Fetch internals
unit(7, 62, 34, 8.5, "PC and next-PC", "+2 / +4 / +6 by instruction length")
unit(7, 52, 34, 8.5, "Length decode", "16-, 32- or 48-bit instruction")
unit(7, 42, 34, 8.5, "Instruction cache", "8 KiB direct mapped, 256 lines of 32 bytes\nreturns a 48-bit window")

# Decode internals
unit(55, 62, 28, 8.5, "Opcode decode", "opcode byte to 7-bit op")
unit(55, 52, 28, 8.5, "Microcode ROM", "64 x 6 control bits")
unit(55, 42, 28, 8.5, "Operand select", "32-bit / signed 16-bit / 8-bit")

# Execute internals
unit(97, 62, 27, 8.5, "ALU", "add, logic, shifts, extend\nmultiply in 3 DSP blocks")
unit(126, 62, 27, 8.5, "Divider", "shared, iterative\n1 bit per clock")
unit(97, 52, 27, 8.5, "Compare and branch", "condition codes, target")
unit(126, 52, 27, 8.5, "Result latch", "two register write ports")
unit(97, 42, 56, 8.5, "Data bus sequencer",
     "store, load, push, pop, jsr and ret as 16-bit bus cycles\nraises busy until the last ack")

# Lower row
unit(52, 19, 34, 12, "Register file", "16 x 32 bits · 973 LUT\n2 read, 2 write, fp and sp taps")
unit(94, 19, 30, 12, "Forwarding", "from execute outputs\nand from write-back")
box(4, 3, 152, 8.5, "#e9f4ec", BUS, lw=1.8, r=1.4)
ax.text(80, 8.6, "Wishbone master", fontsize=12.5, weight="bold", color=INK, ha="center", va="center", zorder=3)
ax.text(80, 5.4, "data side has priority over instruction side  ·  an ack counts only if the same request "
        "was on the bus one clock earlier", fontsize=9, color=MUTED, ha="center", va="center", zorder=3)

# Forward data path
arrow((44, 66), (52, 66), ACCENT, "opcode\noperand\nPC, valid", (48, 59.5))
arrow((86, 66), (94, 66), ACCENT, "op, operand\ncontrol bits\nreg indices", (90, 59.5))

# Register read / forward / execute operands
arrow((69, 40), (69, 31), ACCENT, "read A, B", (75.5, 35.5))
arrow((86, 25), (94, 25), ACCENT)
path([(109, 31), (109, 36), (109, 40)], ACCENT)
ax.text(115.5, 35.5, "regA, regB", fontsize=8.8, color=ACCENT, ha="center", va="center", zorder=5,
        bbox=dict(fc="white", ec="none", pad=1.2, alpha=0.92))

# Write-back and forwarding taps from execute outputs
path([(145, 40), (145, 15), (60, 15), (60, 19)], ACCENT)
ax.text(134, 15, "results, write index and enable (ports A and B)", fontsize=8.8, color=ACCENT,
        ha="right", va="center", zorder=5, bbox=dict(fc="white", ec="none", pad=1.2, alpha=0.92))
arrow((145, 25), (124, 25), ACCENT)

# Feedback: branch and stall over the top
path([(150, 78), (150, 86), (10, 86), (10, 78)], CTRL)
ax.text(70, 86, "branch flag and target: redirect fetch, squash the next instruction", fontsize=9,
        color=CTRL, ha="center", va="center", zorder=5, bbox=dict(fc="white", ec="none", pad=1.5))
path([(125, 78), (125, 82.3), (69, 82.3), (69, 78)], CTRL)
path([(69, 82.3), (30, 82.3), (30, 78)], CTRL)
ax.text(98, 82.3, "busy: stall fetch, decode and register reads", fontsize=9, color=CTRL,
        ha="center", va="center", zorder=5, bbox=dict(fc="white", ec="none", pad=1.5))

# Bus connections
arrow((14, 42), (14, 11.5), BUS, "instruction\nline fills", (14, 27), style="<|-|>")
arrow((152, 42), (152, 11.5), BUS, "data\naccess", (152, 30), style="<|-|>")

# Legend
lx, ly = 26, 28
for i, (c, t) in enumerate(((ACCENT, "data path"), (CTRL, "control feedback"), (BUS, "bus"))):
    ax.plot([lx + 34 * 0, lx + 5], [ly - i * 3.2, ly - i * 3.2], color=c, lw=2.4, zorder=4)
    ax.text(lx + 7, ly - i * 3.2, t, fontsize=9, color=MUTED, va="center")

ax.text(156, 0.9, "LUT counts: yosys 0.63, Spartan-6 LUT6 mapping, caches excluded  ·  core total 3,403 LUT, 1,268 flops",
        fontsize=8.5, color=MUTED, ha="right", va="center")

fig.savefig(sys.argv[1], bbox_inches="tight", pad_inches=0.25, facecolor="white")
