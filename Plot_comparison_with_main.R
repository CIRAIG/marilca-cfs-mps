# Figure of the CF changes per material vs another git branch (default: main)
# Run from the repository root after Compare_with_main.R:
#   source("Plot_comparison_with_main.R")
# Output: figures/Comparison_with_previous_version_CF_ratios.png

## Load libraries ----------------------------------------------------------
library(tidyverse)
library(openxlsx)

## Settings ----------------------------------------------------------------
ref_branch <- "main"
ref_label <- "previous version" #name of the reference in the figure
in_file <- paste0("comparison_vs_", ref_branch, ".xlsx")
sheet <- "SI_C_CF_mid_PAF_d" #steady-state midpoint CFs; other SI_C/SI_D sheets show the same pattern
out_file <- "figures/Comparison_with_previous_version_CF_ratios.png"
threshold <- 0.10 #a CF counts as changed if it differs by more than 10%

## Data --------------------------------------------------------------------
cmp <- read.xlsx(in_file, sheet = sheet) |>
  distinct()

ratios <- cmp |>
  dplyr::select(polymer, ends_with("_geom_mean_ratio")) |>
  pivot_longer(-polymer, names_to = "ecosystem", values_to = "ratio") |>
  mutate(ecosystem = str_remove(ecosystem, "_geom_mean_ratio"),
         ecosystem = factor(str_to_title(ecosystem), levels = c("Marine", "Freshwater", "Terrestrial", "Total"))) |>
  filter(is.finite(ratio), ratio > 0)

# Share of CFs changed per material (all ecosystems), used to order and label the materials
changed <- ratios |>
  group_by(polymer) |>
  summarise(share_changed = mean(abs(ratio - 1) > threshold),
            max_dev = max(abs(log10(ratio))), .groups = "drop") |>
  mutate(label = sprintf("%s (%.0f%%)", polymer, 100 * share_changed),
         status = ifelse(round(100 * share_changed) >= 1, "Changed", "Unchanged")) |> #consistent with the rounded share in the label
  arrange(max_dev)

ratios <- ratios |>
  left_join(changed, by = "polymer") |>
  mutate(label = factor(label, levels = changed$label))

## Plot --------------------------------------------------------------------
colors <- c("Changed" = "#2a78d6", "Unchanged" = "#a8a7a1")

p <- ggplot(ratios, aes(x = ratio, y = label, color = status)) +
  annotate("rect", xmin = 1 - threshold, xmax = 1 + threshold, ymin = -Inf, ymax = Inf,
           fill = "#f0efec") +
  geom_vline(xintercept = 1, color = "#52514e", linewidth = 0.4) +
  geom_jitter(height = 0.2, width = 0, size = 0.6, alpha = 0.35, stroke = 0) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2), labels = c("0.1×", "0.2×", "0.5×", "1×", "2×")) +
  scale_color_manual(values = colors, name = NULL,
                     labels = c("Changed" = paste0("Changed (≥1% of CFs beyond ±", 100 * threshold, "%)"), "Unchanged" = "Unchanged")) +
  facet_wrap(~ecosystem, nrow = 1) +
  labs(
    title = paste0("Change in CFs per material: comparison with the ", ref_label),
    subtitle = paste0("Each point is one CF (geometric mean, ", sheet, "), as the ratio new / ", ref_label, " (", ref_branch, " branch)",
                      ". Shaded band: ±", 100 * threshold, "%.\nIn brackets: share of CFs changed by more than ±",
                      100 * threshold, "% (all ecosystems)."),
    x = paste0("Ratio new / ", ref_label, " (log scale)"), y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "#fcfcfb", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#e6e5e0", linewidth = 0.3),
    plot.title = element_text(color = "#0b0b0b", face = "bold"),
    plot.subtitle = element_text(color = "#52514e", size = 8.5),
    axis.text = element_text(color = "#52514e"),
    strip.text = element_text(color = "#0b0b0b", face = "bold"),
    panel.spacing.x = unit(1.5, "lines"),
    legend.position = "top",
    legend.justification = "left"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2.5, alpha = 1)))

ggsave(out_file, p, width = 11, height = 7, dpi = 200, bg = "#fcfcfb")
message("Figure saved in ", out_file)
