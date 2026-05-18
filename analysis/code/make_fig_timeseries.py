#!/usr/bin/env python3
"""
make_fig_timeseries.py
Long-run time-series figures, two incumbent cohorts overlaid.

Input:   analysis/input/cycle_longrun.parquet
Outputs: analysis/output/fig_share_LR.{pdf,png}
         analysis/output/fig_polarization.{pdf,png}
"""
from __future__ import annotations
import os
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib-codex")
os.environ.setdefault("XDG_CACHE_HOME", "/tmp/xdg-cache-codex")
Path(os.environ["MPLCONFIGDIR"]).mkdir(parents=True, exist_ok=True)
Path(os.environ["XDG_CACHE_HOME"]).mkdir(parents=True, exist_ok=True)

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

BASE = Path(__file__).resolve().parents[2]
IN   = BASE / "analysis/input/cycle_longrun.parquet"
OUT  = BASE / "analysis/output"
OUT.mkdir(parents=True, exist_ok=True)

C_LEFT, C_RIGHT = "#2c5d8a", "#a8322d"
C_GREY,  C_BAND = "#222222", "#bcbcbc"

plt.rcParams.update({
    "font.family":      "serif",
    "font.serif":       ["Times New Roman", "Nimbus Roman", "DejaVu Serif"],
    "mathtext.fontset": "stix",
    "axes.titlesize":   11, "axes.labelsize": 10,
    "xtick.labelsize":  9,  "ytick.labelsize": 9,
    "legend.fontsize":  9,
    "axes.linewidth":   0.7,
    "xtick.major.width": 0.6, "ytick.major.width": 0.6,
    "xtick.major.size":  3.0, "ytick.major.size":  3.0,
    "savefig.bbox":     "tight", "savefig.pad_inches": 0.02,
})


def style(ax):
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    for s in ("left", "bottom"):
        ax.spines[s].set_color("#555555")
    ax.tick_params(axis="both", color="#555555", labelcolor="#222222")


def pivot(df, measure, cohort):
    s = df[(df["measure"] == measure) & (df["cohort"] == cohort)] \
            .sort_values("cycle")
    return s["cycle"].to_numpy(), s["mean"].to_numpy(), \
           s["ci_lo"].to_numpy(), s["ci_hi"].to_numpy()


def plot_share_LR(df, cohorts):
    fig, ax = plt.subplots(figsize=(6.8, 4.0))
    for measure, color in [("L", C_LEFT), ("R", C_RIGHT)]:
        for (cohort, label, ls, lw, marker) in cohorts:
            x, y, lo, hi = pivot(df, measure, cohort)
            ax.fill_between(x, lo, hi, color=color, alpha=0.10, linewidth=0)
            ax.plot(x, y, color=color, linewidth=lw, linestyle=ls,
                    label=f"${measure}_{{ct}}(\\kappa)$ -- {label}")
            if marker:
                ax.scatter(x, y, s=18, facecolor="white",
                           edgecolor=color, linewidth=1.0, zorder=3)
    ax.set_xlabel("Election cycle")
    ax.set_ylabel("Share of county--cycle physicians")
    cycles = sorted(df["cycle"].unique())
    ax.set_xticks(np.arange(cycles[0], cycles[-1] + 1, 4))
    ax.yaxis.set_major_formatter(
        plt.FuncFormatter(lambda v, _p: f"{int(round(v*100))}%"))
    ax.set_ylim(bottom=0)
    style(ax)
    ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.22), ncol=2,
              frameon=False, handlelength=2.4, columnspacing=2.2)
    fig.tight_layout()
    stub = OUT / "fig_share_LR"
    fig.savefig(stub.with_suffix(".pdf"))
    fig.savefig(stub.with_suffix(".png"), dpi=400)
    plt.close(fig)
    print(f"Saved: {stub}.{{pdf,png}}")


def plot_polarization(df, cohorts):
    fig, axes = plt.subplots(2, 1, figsize=(6.8, 6.2), sharex=True)
    panels = [
        (axes[0], "Pi",
         r"$\Pi_{ct}(\kappa) = 4\, L_{ct}(\kappa)\, R_{ct}(\kappa)$",
         "Two-pole polarization index"),
        (axes[1], "sd_p",
         r"$\mathrm{SD}(\tilde{p}_{ict})$",
         r"Within-county dispersion of $\tilde{p}_{ict}$"),
    ]
    for ax, measure, ylab, title in panels:
        for (cohort, label, ls, lw, marker) in cohorts:
            x, y, lo, hi = pivot(df, measure, cohort)
            ax.fill_between(x, lo, hi, color=C_BAND, alpha=0.45, linewidth=0)
            ax.plot(x, y, color=C_GREY, linewidth=lw, linestyle=ls, label=label)
            if marker:
                ax.scatter(x, y, s=20, facecolor="white", edgecolor=C_GREY,
                           linewidth=1.0, zorder=3)
        ax.set_title(title, loc="left", pad=8, fontsize=10.5)
        ax.set_ylabel(ylab)
        ymin, ymax = ax.get_ylim()
        ax.set_ylim(ymin, ymax + 0.22 * (ymax - ymin))
        style(ax)
    axes[0].legend(loc="upper left", frameon=False,
                   handlelength=2.4, labelspacing=0.3)
    axes[-1].set_xlabel("Election cycle")
    cycles = sorted(df["cycle"].unique())
    axes[-1].set_xticks(np.arange(cycles[0], cycles[-1] + 1, 4))
    fig.align_ylabels(axes)
    fig.tight_layout(h_pad=1.4)
    stub = OUT / "fig_polarization"
    fig.savefig(stub.with_suffix(".pdf"))
    fig.savefig(stub.with_suffix(".png"), dpi=400)
    plt.close(fig)
    print(f"Saved: {stub}.{{pdf,png}}")


def main():
    df = pd.read_parquet(IN)
    cohorts = [
        ("incumbents_<=_2008", r"incumbents $\leq$ 2008", "-",  1.6, True),
        ("incumbents_<=_1996", r"incumbents $\leq$ 1996", "--", 1.3, False),
    ]
    plot_share_LR(df, cohorts)
    plot_polarization(df, cohorts)


if __name__ == "__main__":
    main()
