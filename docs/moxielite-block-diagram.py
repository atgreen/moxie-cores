"""Block diagram of the MoxieLite core, in the style of mox125-block-diagram.py."""
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
ax.text(4, 92.5, "MoxieLite", fontsize=24, weight="bold", color=INK, va="center")
ax.text(30.5, 92.2, "multi-cycle Moxie core  ·  VHDL  ·  big- or little-endian  ·  "
        "16-bit data bus, 32-bit address", fontsize=12, color=MUTED, va="center")

# Panels
SY, SH = 46, 38
stage(4, SY, 50, SH, "Control", "one state machine")
stage(62, SY, 94, SH, "Datapath", "32 bits wide, combinational")

# Control internals
unit(7, 69, 21, 8, "Instruction", "16 bits")
unit(30, 69, 21, 8, "Decoder", "state, ALU op, mode")
unit(7, 59.5, 44, 8, "State machine",
     "fetch, decode, immediate, load, execute, store")
unit(7, 50, 21, 8, "Flags", "Z, C, O, S")
unit(30, 50, 21, 8, "Condition", "from the flags")

# Datapath internals
W = 28.5
for i, (t, d) in enumerate((("ALU", "add, adc, sub, sbb, logic\nshifts, rotates, extends"),
                            ("ALU2", "multiplier, 2 stages\ndivider, 32 steps"),
                            ("Adders", "PC + 2, PC + offset\nbase + offset, \\$sp + offset"))):
    unit(65 + i * (W + 1.5), 68, W, 9, t, d)
unit(65, 57.5, W, 9, "Operand A", "register, immediate\nor branch offset")
unit(65 + W + 1.5, 57.5, W, 9, "Operand B", "register, immediate, loaded\ndata, 8-bit value or 4")
unit(65 + 2 * (W + 1.5), 57.5, W, 9, "Exceptions", "irq, swi, brk, bus error\nto the handler in sr1")
unit(65, 48, 88.5, 8, "Addressing modes",
     "register, immediate, absolute, indirect and indirect with offset, chosen by the decoder")

# Lower row: what the datapath reads and writes
unit(62, 19, 36, 13, "Register file",
     "16 x 32 bits in flip-flops\n\\$fp, \\$sp, \\$r0-\\$r13\nread ports A and B")
unit(102, 19, 25, 13, "Special registers", "sr0-sr16\nstatus, handler,\ncause, resume")
unit(131, 19, 25, 13, "Bus sequencer", "address and data\nregisters; byte or\n16-bit cycles")

box(4, 3, 152, 8.5, "#e9f4ec", BUS, lw=1.8, r=1.4)
ax.text(80, 8.6, "Bus and debug ports", fontsize=12.5, weight="bold", color=INK, ha="center",
        va="center", zorder=3)
ax.text(80, 5.4, "16-bit data, strobes, byte mask, wait  ·  gdb_i halts the core and dumps "
        "its registers on debug_o, for cores/gdbtarget  ·  moxielite_wb.v makes it Wishbone",
        fontsize=9, color=MUTED, ha="center", va="center", zorder=3)

# Control between the state machine and the datapath
arrow((54, 70), (62, 70), CTRL, "controls", (58, 73))
arrow((62, 55), (54, 55), CTRL, "flags", (58, 59.2))
path([(50, 46), (50, 25.5), (62, 25.5)], CTRL)
label(56, 29, "register\nselects", CTRL)

# Data between the datapath and what it reads and writes
arrow((72, 32), (72, 46), ACCENT, "A, B", (72, 39))
arrow((88, 46), (88, 32), ACCENT, "write", (88, 39))
arrow((114.5, 32), (114.5, 46), ACCENT, "gsr, ssr", (114.5, 39), style="<|-|>")
arrow((143.5, 32), (143.5, 46), ACCENT, "address, data", (143.5, 39), style="<|-|>")

# Bus connections
arrow((14, 11.5), (14, 46), BUS, "instruction", (14, 30))
arrow((143.5, 19), (143.5, 11.5), BUS, "bus cycles", (143.5, 15.3), style="<|-|>")
arrow((36, 11.5), (36, 46), BUS, "halt, dump", (36, 30), style="<|-|>")

# Legend
lx, ly = 18.5, 40.5
for i, (c, t) in enumerate(((ACCENT, "data"), (CTRL, "control"), (BUS, "bus"))):
    ax.plot([lx, lx + 5], [ly - i * 3.2, ly - i * 3.2], color=c, lw=2.4, zorder=4)
    ax.text(lx + 7, ly - i * 3.2, t, fontsize=9, color=MUTED, va="center")

fig.savefig(sys.argv[1], bbox_inches="tight", pad_inches=0.25, facecolor="white")
