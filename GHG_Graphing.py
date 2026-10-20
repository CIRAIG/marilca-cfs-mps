"""Figures for the climate change (GHG) characterization factors of microplastic degradation.

Reads the output of 05_Run_SB_CI_GHGs.R and 06_Default_GHG.R (results/results_GHG_CI.xlsx)
and writes PNG figures (300 dpi) to figures/GHG/.

Run from the repository root:  python GHG_Graphing.py
"""

import re
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.colors import LinearSegmentedColormap, LogNorm
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

# =============================================================================
# Settings
# =============================================================================

RESULTS_FILE = Path("results/results_GHG_CI.xlsx")
CACHE_DIR = Path("results/.ghg_cache")  # parquet copies of the Excel sheets (reading the Excel takes minutes)
FIG_DIR = Path("figures/GHG")
PIAO_FILE = Path("input/Piao2024_GWP100.csv")  # GWP100 from Piao et al. (2024), SI Tables S3, S4 and S17

REGION_DETAIL = "North America"  # region for the figure that has no World average (mass degraded)

# Default size of each shape (as in 06_Default_GHG.R)
DEFAULT_SIZE = {"Sphere": 1000, "Fiber": 10, "Film": 100}

# Journal style: 180 mm double-column width, 7 pt text
FULL_WIDTH = 7.09  # in
mpl.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Arial", "Helvetica", "DejaVu Sans"],
    "font.size": 7,
    "axes.titlesize": 7.5,
    "axes.labelsize": 7,
    "xtick.labelsize": 6.5,
    "ytick.labelsize": 6.5,
    "legend.fontsize": 6.5,
    "legend.title_fontsize": 7,
    "axes.linewidth": 0.6,
    "xtick.major.width": 0.6,
    "ytick.major.width": 0.6,
    "xtick.minor.width": 0.4,
    "ytick.minor.width": 0.4,
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.edgecolor": "#333333",
    "text.color": "#111111",
    "axes.labelcolor": "#111111",
    "xtick.color": "#333333",
    "ytick.color": "#333333",
    "legend.frameon": False,
    "savefig.dpi": 300,
    "savefig.bbox": "tight",
    "savefig.pad_inches": 0.03,
})

INK_MUTED = "#6b6b6b"
GRID = "#e6e6e6"
BAND = "#f3f3f3"

# Polymers: fossil-based, bio-based, then the default polymers (TRWP has no GHG results)
FOSSIL = ["EPS", "HDPE", "LDPE", "PP", "PS", "PVC", "PET", "PA_Nylon", "PU", "PAN", "PCL", "PBAT", "PBSA"]
# Cellulose fibres are shown as one "Cellulose" row: the average of the six fibres
CELLULOSE = ["Cotton", "Linen", "Viscose", "Rayon", "Lyocell", "Modal"]
BIOBASED = ["PLA", "PHA", "starch_blend", "Cellulose"]
DEFAULTS = ["default_low_density", "default_high_density", "default_all_density"]
# Polymers degraded only by UV-driven photo-oxidation (polyolefins, EPS, PS, PVC): k_deg = 0 in the UV-absent compartments
UV_ONLY_POLYMERS = {"EPS", "HDPE", "LDPE", "PP", "PS", "PVC"}
UV_ABSENT = {"seawater column", "freshwater sediments", "lake sediments", "marine sediments",
             "natural soil", "agricultural soil"}
FOOTNOTE_UV_ONLY = "* Degraded by UV only"
FOOTNOTE_UV = "** UV-absent compartment"
FOOTNOTE_BOTH = f"{FOOTNOTE_UV_ONLY}.   {FOOTNOTE_UV}."

POLYMER_LABELS = {
    "PA_Nylon": "PA (nylon)",
    "starch_blend": "Starch blend",
    "default_low_density": "Default (low density)",
    "default_high_density": "Default (high density)",
    "default_all_density": "Default (all densities)",
}

# Emission compartments: the six continental compartments of the PEBBLE figures, then the others
EMISSION_MAIN = [
    "continental air",
    "continental riverwater",
    "continental lakewater",
    "continental seawater surface",
    "continental natural soil",
    "continental agricultural soil",
]
EMISSION_ALL = EMISSION_MAIN[:4] + [
    "continental seawater column",
    "continental freshwater sediments",
    "continental lake sediments",
    "continental marine sediments",
] + EMISSION_MAIN[4:] + [
    "global seawater surface",
    "global seawater column",
    "global marine sediments",
]

# Emission compartment colours of the PEBBLE figures (all pairs pass the CVD check), plus a marker each
EMISSION_STYLE = {
    "continental air":               ("#808080", "o"),
    "continental riverwater":        ("#3283FE", "s"),
    "continental lakewater":         ("#88CCEE", "D"),
    "continental seawater surface":  ("#325A9B", "^"),
    "continental natural soil":      ("#B3DE69", "v"),
    "continental agricultural soil": ("#006400", "P"),
}

# Receiving compartment types (continental and global merged). Colours follow the PEBBLE FF figures,
# re-stepped where neighbours were too close (adjacent pairs pass the CVD check)
RECEIVING_COLORS = {
    "air":                  "#8C8C8C",
    "riverwater":           "#88CCEE",
    "lakewater":            "#3283FE",
    "seawater surface":     "#325A9B",
    "seawater column":      "#132B4A",
    "freshwater sediments": "#F1E2CC",
    "lake sediments":       "#D9A85E",
    "marine sediments":     "#8A5A10",
    "natural soil":         "#B3DE69",
    "agricultural soil":    "#006400",
}
NOT_DEGRADED = "#F2F2F2"

# Categorical slots for shapes, ordinal blue ramp for time horizons, CO2/CH4 pair
SHAPE_STYLE = {"Sphere": ("#2a78d6", "o"), "Fiber": ("#eb6834", "s"), "Film": ("#1baf7a", "^")}
HORIZON_COLORS = {"GWP20": "#86b6ef", "GWP100": "#2a78d6", "GWP500": "#104281"}
GAS_COLORS = {"CO2": "#2a78d6", "CH4": "#eb6834"}

CMAP_CF = LinearSegmentedColormap.from_list("blues", ["#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"])


# =============================================================================
# Data
# =============================================================================

def load_sheet(sheet):
    """Read one sheet of the results, using a parquet cache refreshed when the Excel file changes."""
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    cache = CACHE_DIR / f"{sheet}.parquet"
    if not cache.exists() or cache.stat().st_mtime < RESULTS_FILE.stat().st_mtime:
        print(f"Reading sheet {sheet} from {RESULTS_FILE} ...")
        pd.read_excel(RESULTS_FILE, sheet_name=sheet).to_parquet(cache)
    return pd.read_parquet(cache)


def gwp_values():
    """GWP values used in the R script, read from the README sheet."""
    readme = load_sheet("README").set_index("Item")["Value"]
    parse = lambda text: {k: float(v) for k, v in re.findall(r"(GWP\d+) = ([\d.]+)", text)}
    return {"CO2": parse(readme["GWP CO2"]), "CH4": parse(readme["GWP CH4 fossil"])}


def merge_cellulose(df):
    """Replace the cellulose fibres by one polymer "Cellulose": average of their numeric columns."""
    is_cell = df["polymer"].isin(CELLULOSE)
    if not is_cell.any():
        return df
    value_cols = [c for c in df.columns if pd.api.types.is_float_dtype(df[c])]
    keys = [c for c in df.columns if c not in value_cols + ["polymer", "elementary_flowname"]]
    agg = {c: "mean" for c in value_cols}
    d = df[is_cell].copy()
    if "elementary_flowname" in d.columns:
        # Keep the "(default diameter/thickness)" copies apart from the rows they duplicate
        d["..default_size"] = d["elementary_flowname"].str.contains(r"\(default (?:diameter|thickness)\)", regex=True)
        keys.append("..default_size")
        agg["elementary_flowname"] = "first"
    cell = d.groupby(keys, dropna=False, sort=False).agg(agg).reset_index().assign(polymer="Cellulose")
    return pd.concat([df[~is_cell], cell[df.columns]], ignore_index=True)


def without_default_size_rows(df):
    """Drop the copies named '(default diameter)'/'(default thickness)', which duplicate one size."""
    return df[~df["elementary_flowname"].str.contains(r"\(default (?:diameter|thickness)\)", regex=True)]


def label(polymer):
    """Polymer name for the figures; polymers degraded by UV only are marked with *."""
    return POLYMER_LABELS.get(polymer, polymer) + ("*" if polymer in UV_ONLY_POLYMERS else "")


def comp_label(compartment, mode="bare"):
    """Compartment name for the figures; UV-absent compartments are marked with **.
    mode: 'bare' (Natural soil**), 'short' (cont. natural soil**) or 'emission' (Emission to natural soil**)."""
    scale, _, kind = compartment.partition(" ")
    if scale not in ("continental", "global"):
        scale, kind = "", compartment
    kind_label = kind + ("**" if kind in UV_ABSENT else "")
    if mode == "short":
        return f"{'cont.' if scale == 'continental' else 'glob.'} {kind_label}"
    if mode == "emission":
        return f"Emission to {kind_label}"
    return kind_label[0].upper() + kind_label[1:]


def savefig(fig, name):
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    fig.savefig(FIG_DIR / f"{name}.png")
    plt.close(fig)
    print(f"Saved {FIG_DIR / name}.png")


def polymer_order(available):
    order = [p for p in FOSSIL + BIOBASED + DEFAULTS if p in set(available)]
    return order


def shade_rows(ax, n, horizontal=True):
    """Alternating background bands, as in the PEBBLE figures."""
    for i in range(0, n, 2):
        if horizontal:
            ax.axhspan(i - 0.5, i + 0.5, color=BAND, zorder=0, lw=0)


def group_separators(ax, order):
    """Thin lines between fossil, bio-based and default polymers."""
    groups = [FOSSIL, BIOBASED, DEFAULTS]
    pos = 0
    for g in groups[:-1]:
        pos += sum(p in g for p in order)
        if 0 < pos < len(order):
            ax.axhline(pos - 0.5, color="#9a9a9a", lw=0.6, zorder=1)


def fmt_sig(x):
    """Two significant figures, without scientific notation (0.004, 0.039, 1.5, 38, 9,000)."""
    if not np.isfinite(x):
        return ""
    if x == 0:
        return "0"
    v = float(f"{x:.2g}")
    return f"{v:,.0f}" if v >= 100 else f"{v:.2g}"


def add_bottom(fig, xlabel=None, handles=None, legend_title=None, ncol=4, gap=0.012, footnote=None, **legend_kw):
    """Put the shared x-axis label, the legend and a footnote right below the lowest axes (after tight_layout)."""
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    axes = [ax for ax in fig.axes if ax.get_label() != "<colorbar>"]
    y = min(ax.get_tightbbox(renderer).y0 for ax in fig.axes) / fig.bbox.height
    x = np.mean([min(ax.get_position().x0 for ax in axes), max(ax.get_position().x1 for ax in axes)])
    bottom = lambda artist: (fig.canvas.draw(), artist.get_window_extent(renderer).y0 / fig.bbox.height)[1]
    if xlabel:
        y = bottom(fig.text(x, y - gap, xlabel, ha="center", va="top", fontsize=7))
    if handles:
        y = bottom(fig.legend(handles=handles, title=legend_title, loc="upper center", bbox_to_anchor=(x, y - gap),
                              ncol=ncol, **legend_kw))
    if footnote:
        fig.text(x, y - gap, footnote, ha="center", va="top", fontsize=6, color=INK_MUTED)


# =============================================================================
# Figure 1 - CF overview (GWP100, World): polymers x emission compartments, per shape and size
# =============================================================================

def figure_cf_overview(cf, ghg_factors, indicator="GWP100", sizes=(1, 10, 100, 1000), x_min=1e-4, x_max=50):
    d = without_default_size_rows(cf)
    d = d[(d["region"] == "World") & (d["indicator"] == indicator) & d["emission_compartment"].isin(EMISSION_MAIN)]
    order = [p for p in polymer_order(d["polymer"].unique()) if p not in DEFAULTS and d.loc[d["polymer"] == p, "CF_fossil_geom_mean"].notna().any()]
    ypos = {p: i for i, p in enumerate(order)}

    # Full mineralisation to CO2: upper bound of the CF from CO2 alone
    co2_max = ghg_factors.groupby("polymer")["kg_CO2_to_atm_per_kg_degraded"].max()

    shapes = list(DEFAULT_SIZE)
    fig, axes = plt.subplots(len(shapes), len(sizes), figsize=(FULL_WIDTH, 0.6 + 0.12 * len(shapes) * len(order)), sharex=True, sharey=True)
    offsets = np.linspace(-0.3, 0.3, len(EMISSION_MAIN))

    for r, shape in enumerate(shapes):
        for c, size in enumerate(sizes):
            ax = axes[r, c]
            shade_rows(ax, len(order))
            group_separators(ax, order)
            sub = d[(d["shape"] == shape) & (d["size"] == size)]
            if sub.empty:
                ax.text(0.5, 0.5, "outside the\nmicroplastic range", ha="center", va="center",
                        transform=ax.transAxes, color=INK_MUTED, fontsize=6.5)
            for k, comp in enumerate(EMISSION_MAIN):
                color, marker = EMISSION_STYLE[comp]
                s = sub[(sub["emission_compartment"] == comp) & sub["polymer"].isin(order)]
                x = s["CF_fossil_geom_mean"].to_numpy()
                y = s["polymer"].map(ypos).to_numpy() + offsets[k]
                below = x < x_min
                ax.scatter(np.clip(x[~below], x_min, None), y[~below], s=9, marker=marker, color=color,
                           edgecolor="white", linewidth=0.3, zorder=3)
                ax.scatter(np.full(below.sum(), x_min), y[below], s=9, marker=marker, facecolor="white",
                           edgecolor=color, linewidth=0.6, zorder=3)
            if not sub.empty:
                ax.scatter(co2_max.reindex(order).to_numpy(), np.arange(len(order)), marker="|", s=40,
                           color="#222222", linewidth=0.8, zorder=2)
            ax.set_xscale("log")
            ax.set_xlim(x_min / 1.6, x_max)
            ax.grid(axis="x", color=GRID, lw=0.5, zorder=0)
            ax.set_axisbelow(True)
            if r == 0:
                ax.set_title(f"{size} µm", pad=3)
            if c == len(sizes) - 1:
                ax.text(1.04, 0.5, shape, transform=ax.transAxes, rotation=270, va="center", ha="left", fontsize=7.5)

    ax0 = axes[0, 0]
    ax0.set_ylim(len(order) - 0.5, -0.5)
    ax0.set_yticks(range(len(order)))
    ax0.set_yticklabels([label(p) for p in order])
    ax0.tick_params(axis="y", length=0)
    e_min = int(np.log10(x_min))
    ticks = [10.0 ** e for e in range(e_min, 2, 2)]
    for ax in axes[-1]:
        ax.set_xticks(ticks)
        ax.set_xticklabels([f"≤10$^{{{e_min}}}$"] + [f"10$^{{{int(np.log10(t))}}}$" for t in ticks[1:]])
    fig.text(0.5, 0.995, "Diameter / thickness", ha="center", va="top", fontsize=7.5)
    fig.tight_layout(rect=(0, 0, 0.97, 0.985), h_pad=0.6, w_pad=0.6)

    handles = [Line2D([], [], ls="", marker=EMISSION_STYLE[c][1], color=EMISSION_STYLE[c][0], markersize=4.5,
                      label=comp_label(c)) for c in EMISSION_MAIN]
    handles += [Line2D([], [], ls="", marker="|", color="#222222", markersize=6, label="Full mineralisation to CO$_2$"),
                Line2D([], [], ls="", marker="o", markerfacecolor="white", markeredgecolor=INK_MUTED, markersize=4.5,
                       label=f"Below 10$^{{{e_min}}}$ (incl. 0)")]
    add_bottom(fig, f"Characterization factor, World, {indicator} [kg CO$_2$-eq/kg emitted]", handles,
               "Emission compartment", ncol=4, columnspacing=1.2, handletextpad=0.3,
               footnote=FOOTNOTE_BOTH)
    savefig(fig, f"Figure_GHG_1_CF_overview_{indicator}")


# =============================================================================
# Figure 2 - CF heatmap (World, default size): polymers x all emission compartments
# =============================================================================

def figure_cf_heatmap(cf, shape="Sphere", indicator="GWP100", accounting="fossil"):
    size = DEFAULT_SIZE[shape]
    d = without_default_size_rows(cf)
    d = d[(d["region"] == "World") & (d["indicator"] == indicator) & (d["shape"] == shape) & (d["size"] == size)]
    col = f"CF_{accounting}_geom_mean"
    mat = d.pivot_table(index="polymer", columns="emission_compartment", values=col, aggfunc="first")
    order = [p for p in polymer_order(mat.index) if mat.loc[p].notna().any()]
    comps = [c for c in EMISSION_ALL if c in mat.columns]
    mat = mat.loc[order, comps]

    vmin = 1e-3
    vmax = np.nanmax(mat.to_numpy())
    norm = LogNorm(vmin=vmin, vmax=vmax)
    fig, ax = plt.subplots(figsize=(FULL_WIDTH, 1.4 + 0.17 * len(order)))
    values = mat.to_numpy()
    im = ax.imshow(np.clip(values, vmin, None), cmap=CMAP_CF, norm=norm, aspect="auto", interpolation="none")

    # Cell separators (surface-coloured gaps)
    ax.set_xticks(np.arange(-0.5, len(comps)), minor=True)
    ax.set_yticks(np.arange(-0.5, len(order)), minor=True)
    ax.grid(which="minor", color="white", lw=1.0)
    ax.tick_params(which="minor", length=0)
    for spine in ax.spines.values():
        spine.set_visible(False)

    for i in range(len(order)):
        for j in range(len(comps)):
            v = values[i, j]
            if not np.isfinite(v):
                continue
            rgba = CMAP_CF(norm(max(v, vmin)))
            lum = 0.2126 * rgba[0] + 0.7152 * rgba[1] + 0.0722 * rgba[2]
            ax.text(j, i, "<0.001" if v < vmin else fmt_sig(v), ha="center", va="center", fontsize=5,
                    color="white" if lum < 0.45 else "#111111")

    ax.set_xticks(range(len(comps)))
    ax.set_xticklabels([comp_label(c, "short") for c in comps],
                       rotation=40, ha="right", rotation_mode="anchor")
    ax.set_yticks(range(len(order)))
    ax.set_yticklabels([label(p) for p in order])
    ax.tick_params(length=0)
    group_separators(ax, order)
    ax.set_xlabel("Emission compartment")
    dim = "thickness" if shape == "Film" else "diameter"
    ax.set_title(f"{shape}, {size} µm {dim} (default size), World, {indicator}", loc="left")

    cbar = fig.colorbar(im, ax=ax, fraction=0.035, pad=0.015)
    cbar.set_label("CF [kg CO$_2$-eq/kg emitted]")
    cbar.outline.set_visible(False)
    fig.tight_layout()
    add_bottom(fig, footnote=FOOTNOTE_BOTH)
    savefig(fig, f"Figure_GHG_2_CF_heatmap_{shape}_{indicator}_{accounting}")


# =============================================================================
# Figure 3 - Time horizon and carbon accounting (World, default size Sphere)
# =============================================================================

def figure_horizons(cf, shape="Sphere", compartments=("continental air", "continental seawater surface", "continental natural soil")):
    size = DEFAULT_SIZE[shape]
    d = without_default_size_rows(cf)
    d = d[(d["region"] == "World") & (d["shape"] == shape) & (d["size"] == size)]
    order = [p for p in polymer_order(d["polymer"].unique()) if d.loc[d["polymer"] == p, "CF_fossil_geom_mean"].notna().any()]
    ypos = {p: i for i, p in enumerate(order)}

    fig, axes = plt.subplots(1, len(compartments), figsize=(FULL_WIDTH, 0.6 + 0.17 * len(order)), sharey=True)
    for ax, comp in zip(axes, compartments):
        shade_rows(ax, len(order))
        group_separators(ax, order)
        s = d[(d["emission_compartment"] == comp) & d["polymer"].isin(order)]
        wide = s.pivot_table(index="polymer", columns="indicator", values=["CF_fossil_geom_mean", "CF_biogenic_geom_mean"])
        for p in order:
            if p not in wide.index:
                continue
            vals = wide.loc[p].dropna()
            ax.plot([vals.min(), vals.max()], [ypos[p]] * 2, color="#b5b5b5", lw=0.8, zorder=1)
        for ind, color in HORIZON_COLORS.items():
            f = wide[("CF_fossil_geom_mean", ind)]
            ax.scatter(f.to_numpy(), f.index.map(ypos), s=14, color=color, edgecolor="white", linewidth=0.3, zorder=3)
            b = wide[("CF_biogenic_geom_mean", ind)]
            bio = b.index.isin(BIOBASED)
            ax.scatter(b[bio].to_numpy(), b[bio].index.map(ypos), s=14, facecolor="white", edgecolor=color,
                       linewidth=0.8, zorder=3)
        ax.set_title(comp_label(comp, "emission"), loc="left")
        ax.grid(axis="x", color=GRID, lw=0.5)
        ax.set_axisbelow(True)
        ax.set_xlim(left=0)
    axes[0].set_ylim(len(order) - 0.5, -0.5)
    axes[0].set_yticks(range(len(order)))
    axes[0].set_yticklabels([label(p) for p in order])
    axes[0].tick_params(axis="y", length=0)
    handles = [Line2D([], [], ls="", marker="o", color=c, markersize=4.5, label=k) for k, c in HORIZON_COLORS.items()]
    handles += [Line2D([], [], ls="", marker="o", color="#555555", markersize=4.5, label="Fossil carbon accounting"),
                Line2D([], [], ls="", marker="o", markerfacecolor="white", markeredgecolor="#555555", markersize=4.5,
                       label="Biogenic accounting (bio-based polymers)")]
    fig.suptitle(f"World, {shape} {size} µm (default size)", x=0.01, ha="left", fontsize=7.5)
    fig.tight_layout()
    add_bottom(fig, "Characterization factor [kg CO$_2$-eq/kg emitted]", handles, ncol=5, handletextpad=0.3,
               footnote=FOOTNOTE_BOTH)
    savefig(fig, "Figure_GHG_3_time_horizon_accounting")


# =============================================================================
# Figure 4 - Contribution of CO2 and CH4 (World), per emission compartment and time horizon
# =============================================================================

def figure_gas_contribution(ghg, gwp, polymers=("default_all_density", "PET", "PLA"), shape="Sphere", size=1000):
    d = ghg[(ghg["region"] == "World") & (ghg["shape"] == shape) & (ghg["size"] == size)]
    horizons = list(gwp["CH4"])
    comps = [c for c in EMISSION_ALL if c in d["emission_compartment"].unique()]
    fig, axes = plt.subplots(len(polymers), len(horizons), figsize=(FULL_WIDTH, 2.0 * len(polymers) + 0.6),
                             sharey=True, squeeze=False)
    for r, pol in enumerate(polymers):
        for c, h in enumerate(horizons):
            ax = axes[r, c]
            s = d[(d["polymer"] == pol) & (d["time_horizon"] == h)].set_index("emission_compartment").reindex(comps)
            co2 = s["CO2_geom_mean"].to_numpy() * gwp["CO2"][h]
            ch4 = s["CH4_geom_mean"].to_numpy() * gwp["CH4"][h]
            y = np.arange(len(comps))
            ax.barh(y, co2, color=GAS_COLORS["CO2"], height=0.72, edgecolor="white", linewidth=0.5)
            ax.barh(y, ch4, left=co2, color=GAS_COLORS["CH4"], height=0.72, edgecolor="white", linewidth=0.5)
            total = co2 + ch4
            xmax = np.nanmax(total) if np.isfinite(total).any() else 1
            for yi, (t, m) in enumerate(zip(total, ch4)):
                if np.isfinite(t) and t > 0.02 * xmax and m / t >= 0.05:
                    ax.text(t + 0.02 * xmax, yi, f"{100 * m / t:.0f}% CH$_4$", va="center", fontsize=5.5, color=INK_MUTED)
            ax.set_xlim(0, xmax * 1.28)
            ax.grid(axis="x", color=GRID, lw=0.5)
            ax.set_axisbelow(True)
            if r == 0:
                ax.set_title(h, pad=3)
            if c == 0:
                ax.text(-0.02, 1.04, label(pol), transform=ax.transAxes, ha="right", fontsize=7.5, fontweight="bold")
    axes[0, 0].set_ylim(len(comps) - 0.5, -0.5)
    axes[0, 0].set_yticks(range(len(comps)))
    axes[0, 0].set_yticklabels([comp_label(c, "short") for c in comps])
    for ax in axes[:, 0]:
        ax.tick_params(axis="y", length=0)
    handles = [Patch(color=GAS_COLORS["CO2"], label="CO$_2$"), Patch(color=GAS_COLORS["CH4"], label="CH$_4$")]
    fig.suptitle(f"World, {shape} {size} µm", x=0.01, ha="left", fontsize=7.5)
    fig.tight_layout(h_pad=1.2)
    add_bottom(fig, "Characterization factor, fossil carbon accounting [kg CO$_2$-eq/kg emitted]", handles, ncol=2,
               footnote=FOOTNOTE_UV + ".")
    savefig(fig, "Figure_GHG_4_CO2_CH4_contribution")


# =============================================================================
# Figure 5 - Where the mass degrades within 100 years (one region)
# =============================================================================

def figure_mass_degraded(mass, polymers=("LDPE", "PET", "PLA", "Cellulose"), shape="Sphere", sizes=(1, 100, 1000),
                         region=REGION_DETAIL, horizon_label="100 years"):
    d = mass[(mass["region"] == region) & (mass["shape"] == shape) & mass["size"].isin(sizes)
             & mass["emission_compartment"].isin(EMISSION_MAIN) & (mass["receiving_compartment"] != "total")].copy()
    d["receiving_type"] = d["receiving_compartment"].str.replace(r"^(continental|global) ", "", regex=True)
    agg = d.groupby(["polymer", "size", "emission_compartment", "receiving_type"])["deg_geom_mean"].sum()

    fig, axes = plt.subplots(len(polymers), len(sizes), figsize=(FULL_WIDTH, 1.55 * len(polymers) + 0.9),
                             sharex=True, sharey=True, squeeze=False)
    y = np.arange(len(EMISSION_MAIN))
    for r, pol in enumerate(polymers):
        for c, size in enumerate(sizes):
            ax = axes[r, c]
            left = np.zeros(len(EMISSION_MAIN))
            for rec, color in RECEIVING_COLORS.items():
                vals = np.array([agg.get((pol, size, e, rec), 0.0) for e in EMISSION_MAIN])
                ax.barh(y, vals, left=left, color=color, height=0.75, edgecolor="white", linewidth=0.4)
                left += vals
            ax.barh(y, np.clip(1 - left, 0, None), left=left, color=NOT_DEGRADED, height=0.75,
                    edgecolor="white", linewidth=0.4, hatch="//////")
            ax.set_xlim(0, 1)
            if r == 0:
                ax.set_title(f"{size} µm", pad=3)
            if c == 0:
                ax.text(-0.02, 1.06, label(pol), transform=ax.transAxes, ha="right", fontsize=7.5, fontweight="bold")
    axes[0, 0].set_ylim(len(EMISSION_MAIN) - 0.5, -0.5)
    axes[0, 0].set_yticks(y)
    axes[0, 0].set_yticklabels([comp_label(e) for e in EMISSION_MAIN])
    for ax in axes[:, 0]:
        ax.tick_params(axis="y", length=0)
    for ax in axes[-1]:
        ax.xaxis.set_major_formatter(mpl.ticker.PercentFormatter(1.0, decimals=0))
    handles = [Patch(color=c, label=comp_label(k)) for k, c in RECEIVING_COLORS.items()]
    handles.append(Patch(facecolor=NOT_DEGRADED, edgecolor="#9a9a9a", hatch="//////", lw=0.4,
                         label=f"Not degraded within {horizon_label}"))
    fig.suptitle(f"{region}, {shape}. Rows: polymer; y-axis: emission compartment", x=0.01, ha="left", fontsize=7.5)
    fig.tight_layout(h_pad=1.0)
    add_bottom(fig, f"Share of the emitted mass degraded within {horizon_label}", handles,
               "Compartment where degradation occurs (continental + global)", ncol=6, columnspacing=1.0,
               handletextpad=0.4, footnote=FOOTNOTE_BOTH)
    savefig(fig, "Figure_GHG_5_mass_degraded_100yr")


# =============================================================================
# Figure 6 - Effect of size and shape (World, GWP100)
# =============================================================================

def figure_size_shape(cf, polymers=("LDPE", "PP", "PET", "PA_Nylon", "PLA"),
                      compartments=("continental natural soil", "continental seawater surface"), indicator="GWP100"):
    d = without_default_size_rows(cf)
    d = d[(d["region"] == "World") & (d["indicator"] == indicator)]
    fig, axes = plt.subplots(len(compartments), len(polymers), figsize=(FULL_WIDTH, 3.4), sharex=True, squeeze=False)
    for r, comp in enumerate(compartments):
        for c, pol in enumerate(polymers):
            ax = axes[r, c]
            s = d[(d["polymer"] == pol) & (d["emission_compartment"] == comp)]
            for shape, (color, marker) in SHAPE_STYLE.items():
                t = s[s["shape"] == shape].sort_values("size")
                ax.fill_between(t["size"], t["CF_fossil_p2_5"], t["CF_fossil_p97_5"], color=color, alpha=0.15, lw=0)
                ax.plot(t["size"], t["CF_fossil_geom_mean"], color=color, lw=1.2, marker=marker, markersize=3.2,
                        markeredgecolor="white", markeredgewidth=0.3)
            ax.set_xscale("log")
            ax.set_ylim(bottom=0)
            ax.grid(color=GRID, lw=0.5)
            ax.set_axisbelow(True)
            if r == 0:
                ax.set_title(label(pol), pad=3)
            if c == 0:
                ax.set_ylabel(comp_label(comp, "emission").replace("Emission to ", "Emission to\n"))
    for ax in axes[-1]:
        ax.set_xticks([1, 10, 100, 1000])
        ax.set_xticklabels(["1", "10", "10$^2$", "10$^3$"])
    fig.text(0.003, 0.55, f"CF, World, {indicator} [kg CO$_2$-eq/kg emitted]", rotation=90, va="center", fontsize=7)
    handles = [Line2D([], [], color=col, marker=m, markersize=3.5, lw=1.2, label=s) for s, (col, m) in SHAPE_STYLE.items()]
    handles.append(Patch(color="#999999", alpha=0.3, label="2.5–97.5th percentile"))
    fig.tight_layout(rect=(0.015, 0, 1, 1), h_pad=0.8, w_pad=0.4)
    add_bottom(fig, "Diameter / thickness [µm]", handles, ncol=4, footnote=FOOTNOTE_BOTH)
    savefig(fig, f"Figure_GHG_6_size_shape_{indicator}")


# =============================================================================
# Figure 7 - Regional variation (GWP100), with Monte Carlo uncertainty
# =============================================================================

def figure_regions(cf, polymer="default_all_density", shape="Sphere", indicator="GWP100"):
    size = DEFAULT_SIZE[shape]
    d = without_default_size_rows(cf)
    d = d[(d["polymer"] == polymer) & (d["indicator"] == indicator) & (d["shape"] == shape) & (d["size"] == size)]
    regions = sorted(r for r in d["region"].unique() if r != "World") + ["World"]
    ypos = {r: i for i, r in enumerate(regions)}

    fig, axes = plt.subplots(2, 3, figsize=(FULL_WIDTH, 3.6), sharey=True)
    for ax, comp in zip(axes.flat, EMISSION_MAIN):
        s = d[d["emission_compartment"] == comp]
        y = s["region"].map(ypos).to_numpy()
        world = (s["region"] == "World").to_numpy()
        x = s["CF_fossil_geom_mean"].to_numpy()
        lo = x - s["CF_fossil_p2_5"].to_numpy()
        hi = s["CF_fossil_p97_5"].to_numpy() - x
        ax.errorbar(x[~world], y[~world], xerr=[lo[~world], hi[~world]], fmt="o", color="#2a78d6", markersize=3.5,
                    elinewidth=0.8, capsize=0, markeredgecolor="white", markeredgewidth=0.3)
        ax.errorbar(x[world], y[world], xerr=[lo[world], hi[world]], fmt="D", color="#111111", markersize=3.5,
                    elinewidth=0.8, capsize=0)
        ax.axhline(len(regions) - 1.5, color="#9a9a9a", lw=0.6)
        ax.set_title(comp_label(comp, "emission"), loc="left")
        ax.set_xlim(left=0)
        ax.grid(axis="x", color=GRID, lw=0.5)
        ax.set_axisbelow(True)
    axes[0, 0].set_ylim(len(regions) - 0.5, -0.5)
    axes[0, 0].set_yticks(range(len(regions)))
    axes[0, 0].set_yticklabels(regions)
    for ax in axes[:, 0]:
        ax.tick_params(axis="y", length=0)
    fig.suptitle(f"{label(polymer)}, {shape} {size} µm (default size). World = emission-weighted average of the regions",
                 x=0.01, ha="left", fontsize=7.5)
    fig.tight_layout()
    add_bottom(fig, f"CF, {indicator} [kg CO$_2$-eq/kg emitted] (geometric mean, 2.5–97.5th percentile)",
               footnote=FOOTNOTE_UV + ".")
    savefig(fig, f"Figure_GHG_7_regions_{indicator}")


# =============================================================================
# Figure 8 - Comparison with Piao et al. (2024): GWP100 of microplastics emitted to freshwater
# =============================================================================

def figure_piao_comparison(cf, piao, compartments=("continental riverwater", "continental lakewater"),
                           polymers=("PHA", "PCL", "starch_blend", "PBSA", "PLA", "PET")):
    """Piao et al. (2024): static GWP100 of spherical microplastics emitted to a freshwater ecosystem,
    all CO2 and CH4 produced reaching the atmosphere. Polymers matched as PHB-PHA, TPS-starch blend, PBS-PBSA."""
    d = without_default_size_rows(cf)
    d = d[(d["region"] == "World") & (d["indicator"] == "GWP100") & (d["shape"] == "Sphere")]
    ours = {"continental riverwater": ("#2a78d6", "o"), "continental lakewater": ("#1baf7a", "s")}
    piao_style = {s: ("#eb6834", m, ls) for s, m, ls in zip(sorted(piao["scenario"].unique()), ("D", "D"), ("-", "--"))}

    # Sizes at equally spaced positions (ordinal axis), so 5000 µm is as far from 1000 µm as the other steps
    sizes = [0.1, 1, 10, 100, 1000, 5000]
    xpos = {s: i for i, s in enumerate(sizes)}

    fig, axes = plt.subplots(2, 3, figsize=(FULL_WIDTH, 3.9), sharex=True, sharey=True)
    for ax, pol in zip(axes.flat, polymers):
        for comp in compartments:
            color, marker = ours[comp]
            t = d[(d["polymer"] == pol) & (d["emission_compartment"] == comp)].sort_values("size")
            ax.fill_between(t["size"].map(xpos), t["CF_fossil_p2_5"], t["CF_fossil_p97_5"], color=color, alpha=0.15, lw=0)
            ax.plot(t["size"].map(xpos), t["CF_fossil_geom_mean"], color=color, marker=marker, lw=1.2, markersize=3.2,
                    markeredgecolor="white", markeredgewidth=0.3)
        for scen, (color, marker, ls) in piao_style.items():
            t = piao[(piao["polymer"] == pol) & (piao["scenario"] == scen)].sort_values("size_um")
            if t.empty:
                continue
            err = np.vstack([t["gwp100_mean"] - t["gwp100_low"], t["gwp100_high"] - t["gwp100_mean"]])
            ax.errorbar(t["size_um"].map(xpos), t["gwp100_mean"], yerr=np.nan_to_num(err), color=color, ls=ls, lw=1.0,
                        marker=marker, markersize=3.2, elinewidth=0.7, capsize=0,
                        markerfacecolor=color if ls == "-" else "white", markeredgewidth=0.8)
        piao_name = piao.loc[piao["polymer"] == pol, "polymer_piao"].iloc[0]
        ax.set_title(label(pol) if piao_name == pol else f"{label(pol)} (Piao: {piao_name})", pad=3)
        ax.set_yscale("log")
        ax.grid(color=GRID, lw=0.5)
        ax.set_axisbelow(True)
    axes[0, 0].set_ylim(5e-4, 40)
    axes[0, 0].set_xlim(-0.4, len(sizes) - 0.6)
    for ax in axes[-1]:
        ax.set_xticks(range(len(sizes)))
        ax.set_xticklabels([f"{s:g}" for s in sizes])
    fig.text(0.003, 0.55, "GWP100 [kg CO$_2$-eq/kg emitted]", rotation=90, va="center", fontsize=7)
    handles = [Line2D([], [], color=ours[c][0], marker=ours[c][1], markersize=3.5, lw=1.2,
                      label=f"This study, emission to {c.replace('continental ', '')} (World)") for c in compartments]
    handles.append(Patch(color="#999999", alpha=0.3, label="This study, 2.5–97.5th percentile"))
    handles += [Line2D([], [], color=c, ls=ls, marker=m, markersize=3.5, lw=1.0,
                       markerfacecolor=c if ls == "-" else "white", label=f"Piao et al. (2024), scenario {s}")
                for s, (c, m, ls) in piao_style.items()]
    fig.tight_layout(rect=(0.015, 0, 1, 1), h_pad=0.8)
    add_bottom(fig, "Diameter [µm] (spheres)", handles, ncol=2, columnspacing=1.5)
    savefig(fig, "Figure_GHG_8_comparison_Piao2024")


# =============================================================================
# Main
# =============================================================================

if __name__ == "__main__":
    cf = merge_cellulose(load_sheet("CF_GWP_defaults"))
    ghg = merge_cellulose(load_sheet("GHG_emitted_defaults"))
    ghg_factors = merge_cellulose(load_sheet("GHG_per_kg_degraded"))
    mass_100 = merge_cellulose(load_sheet("Mass_degraded_100yr"))
    gwp = gwp_values()

    figure_cf_overview(cf, ghg_factors, "GWP100")
    figure_cf_overview(cf, ghg_factors, "GWP20")
    figure_cf_heatmap(cf, "Sphere", "GWP100", "fossil")
    figure_cf_heatmap(cf, "Sphere", "GWP100", "biogenic")
    figure_horizons(cf)
    figure_gas_contribution(ghg, gwp)
    figure_mass_degraded(mass_100)
    figure_size_shape(cf)
    figure_regions(cf)
    figure_piao_comparison(cf, pd.read_csv(PIAO_FILE))
