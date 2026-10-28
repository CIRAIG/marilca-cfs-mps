# ReCiPe CFs for the three cultural perspectives -------------------------------
# Builds results/ReCiPe/ReCiPe_CF.xlsx from results/SI_C.xlsx (steady state) and results/SI_D.xlsx (20 and 100 yrs):
#   - Endpoint CFs (species·yr/kg) per cultural perspective
#   - Midpoint CFs expressed in kg PET, 1µm, sphere, rw/sw/ns-eq/kg (reference: World region)
#   - Midpoint to endpoint conversion factors (species·yr/kg PET, 1µm, sphere, rw/sw/ns-eq)
# Run from the repository root after 03_Default_CF.R:
#   source("results/ReCiPe/ReCiPe_CF.R")

## Load libraries ----------------------------------------------------------
library(tidyverse)
library(openxlsx)

## Settings ----------------------------------------------------------------
out_file <- "results/ReCiPe/ReCiPe_CF.xlsx"

# Cultural perspectives: time horizon over which impacts are integrated, and the corresponding SI sheets
perspectives <- tribble(
  ~perspective,    ~time_horizon,           ~file,               ~midpoint_sheet,         ~endpoint_sheet,
  "Individualist", "20 years",              "results/SI_D.xlsx", "CF_mid_PAF_d_20_yrs",   "CF_species_yr_20_yrs",
  "Hierarchist",   "100 years",             "results/SI_D.xlsx", "CF_mid_PAF_d_100_yrs",  "CF_end_species_yr_100_yrs",
  "Egalitarian",   "infinite (steady state)", "results/SI_C.xlsx", "CF_mid_PAF_d",          "CF_end_species_yr"
)

# Reference flow of the midpoint: 1 kg PET, 1µm, sphere, World region
# emitted to the compartment of each ecosystem (rw: river water, sw: seawater surface, ns: natural soil)
reference <- list(region = "World", polymer = "PET", size = 1, shape = "Sphere")
ecosystems <- tribble(
  ~ecosystem,    ~impact_category,               ~reference_emission,            ~reference_abbr,
  "freshwater",  "Freshwater physical effects",  "continental riverwater",       "rw",
  "marine",      "Marine physical effects",      "continental seawater surface", "sw",
  "terrestrial", "Terrestrial physical effects", "continental natural soil",     "ns"
) |>
  mutate(reference_unit = paste0("kg PET, 1µm, sphere, ", reference_abbr, "-eq"))

stats <- c("geom_mean", "gsd", "ll_95", "ul_95")
id_cols <- c("elementary_flowname", "region", "polymer", "size", "shape", "emission_compartment")

## Read CFs ----------------------------------------------------------------
read_sheet <- function(file, sheet) read.xlsx(file, sheet = sheet)

midpoint_si <- setNames(pmap(perspectives, function(file, midpoint_sheet, ...) read_sheet(file, midpoint_sheet)),
                        perspectives$perspective)
endpoint_si <- setNames(pmap(perspectives, function(file, endpoint_sheet, ...) read_sheet(file, endpoint_sheet)),
                        perspectives$perspective)

# Reference CF (geometric mean) of one SI sheet, for one ecosystem
reference_cf <- function(df, ecosystem, reference_emission) {
  ref_row <- df |>
    filter(region == reference$region, polymer == reference$polymer, size == reference$size,
           shape == reference$shape, emission_compartment == reference_emission) |>
    distinct()
  if (nrow(ref_row) != 1) stop("Reference CF not found (or not unique) for ", ecosystem)
  ref_row[[paste0(ecosystem, "_geom_mean")]]
}

# One row per flow and impact category (instead of the ecosystems side by side), for one SI sheet
# unit: one unit for all ecosystems, or one unit per ecosystem (named vector)
# scale: divides geom_mean, ll_95 and ul_95 (one value per ecosystem); the gsd is unchanged
to_long <- function(df, unit, scale = setNames(rep(1, nrow(ecosystems)), ecosystems$ecosystem)) {
  pmap_dfr(ecosystems, function(ecosystem, impact_category, ...) {
    df |>
      transmute(
        impact_category = impact_category,
        unit = if (length(unit) > 1) unit[[ecosystem]] else unit,
        across(all_of(id_cols)),
        geom_mean = .data[[paste0(ecosystem, "_geom_mean")]] / scale[[ecosystem]],
        gsd       = .data[[paste0(ecosystem, "_gsd")]],
        ll_95     = .data[[paste0(ecosystem, "_ll_95")]] / scale[[ecosystem]],
        ul_95     = .data[[paste0(ecosystem, "_ul_95")]] / scale[[ecosystem]],
        n_samples = n_samples
      )
  })
}

## Midpoint: kg PET, 1µm, sphere, rw/sw/ns-eq/kg ---------------------------
# For each perspective and ecosystem: CF_mid / CF_mid of the reference flow (same perspective, same ecosystem)
# The reference CF is used as a constant: the gsd is unchanged, ll_95 and ul_95 are divided by the reference CF
midpoint_references <- map_dfr(perspectives$perspective, function(p) {
  pmap_dfr(ecosystems, function(ecosystem, impact_category, reference_emission, ...) {
    tibble(impact_category = impact_category, perspective = p, reference_emission = reference_emission,
           reference_CF_PAF_d_per_kg = reference_cf(midpoint_si[[p]], ecosystem, reference_emission))
  })
})

midpoint <- map(setNames(perspectives$perspective, perspectives$perspective), function(p) {
  ref <- midpoint_references |>
    filter(perspective == p) |>
    left_join(ecosystems, by = c("impact_category", "reference_emission"))
  to_long(midpoint_si[[p]], unit = setNames(paste0(ecosystems$reference_unit, "/kg"), ecosystems$ecosystem),
          scale = setNames(ref$reference_CF_PAF_d_per_kg, ref$ecosystem))
})

## Endpoint: species·yr/kg -------------------------------------------------
# Same values as the species·yr sheets of SI_C and SI_D, one row per flow and impact category (total not included)
endpoint <- map(endpoint_si, ~ to_long(.x, unit = "species·yr/kg"))

## Midpoint to endpoint conversion factors ---------------------------------
# Endpoint CF of the reference flow (species·yr per kg PET, 1µm, sphere, rw/sw/ns-eq), per perspective
conversion_all <- pmap_dfr(ecosystems, function(ecosystem, impact_category, reference_emission, ...) {
  map_dfr(perspectives$perspective, function(p) {
    tibble(impact_category = impact_category, perspective = p,
           value = reference_cf(endpoint_si[[p]], ecosystem, reference_emission))
  })
})

# The reference flow (PET, 1µm) has the same CF for all time horizons, so one factor per ecosystem is used
conversion_check <- conversion_all |>
  group_by(impact_category) |>
  summarise(max_rel_diff = max(value) / min(value) - 1, .groups = "drop")
if (any(conversion_check$max_rel_diff > 1e-3)) {
  warning("Midpoint to endpoint factors differ between perspectives by up to ",
          signif(100 * max(conversion_check$max_rel_diff), 2), "%. The Hierarchist value is reported.")
}
conversion <- conversion_all |>
  filter(perspective == "Hierarchist") |>
  transmute(`Midpoint to endpoint conversion factor` = impact_category,
            Unit = paste0("species·yr/", ecosystems$reference_unit[match(impact_category, ecosystems$impact_category)]),
            Value = value)

## Example table (geometric means only) ------------------------------------
example_flows <- tribble(
  ~polymer, ~size, ~shape,   ~label,
  "PET",    1,     "Sphere", "Polyethylene terephthalate 1µm sphere",
  "EPS",    10,    "Sphere", "Expanded polystyrene 10µm sphere"
)
example_emissions <- c("continental air" = "Air",
                       "continental riverwater" = "River water",
                       "continental seawater surface" = "Seawater surface",
                       "continental natural soil" = "Natural soil")

example_blocks <- map(ecosystems$impact_category, function(cat) {
  map(perspectives$perspective, function(p) {
    midpoint[[p]] |>
      filter(impact_category == cat, region == reference$region,
             emission_compartment %in% names(example_emissions)) |>
      inner_join(example_flows, by = c("polymer", "size", "shape")) |>
      distinct() |>
      transmute(label, emission_compartment, perspective = p, geom_mean)
  }) |>
    bind_rows() |>
    pivot_wider(names_from = perspective, values_from = geom_mean) |>
    mutate(label = factor(label, levels = example_flows$label),
           emission = factor(example_emissions[emission_compartment], levels = example_emissions)) |>
    arrange(label, emission) |>
    transmute(!!cat := as.character(label), `Emission compartment` = as.character(emission),
              Individualist, Hierarchist, Egalitarian)
})

## README sheet ------------------------------------------------------------
readme <- c(
  "ReCiPe characterization factors for physical effects of microplastics",
  "",
  "Cultural perspectives (time horizon over which impacts are integrated):",
  paste0("  ", perspectives$perspective, ": ", perspectives$time_horizon,
         " (", basename(perspectives$file), ", sheets ", perspectives$midpoint_sheet, " and ", perspectives$endpoint_sheet, ")"),
  "",
  "Sheets midpoint_Individualist, midpoint_Hierarchist, midpoint_Egalitarian: midpoint CFs in kg PET, 1µm, sphere, rw/sw/ns-eq/kg emitted",
  "  CF_midpoint = CF_mid (PAF·d/kg) / CF_mid of the reference flow (PAF·d/kg), for the same cultural perspective and ecosystem.",
  "  Reference flow: 1 kg of PET, 1µm, sphere, World region, emitted to:",
  paste0("    ", ecosystems$impact_category, ": ", ecosystems$reference_emission, " (unit: ", ecosystems$reference_unit, "/kg)"),
  "  rw, sw and ns stand for the emission compartment of the reference flow: river water, seawater surface and natural soil.",
  "  The reference flow therefore has a midpoint CF of 1 in each perspective.",
  "",
  "Sheets endpoint_Individualist, endpoint_Hierarchist, endpoint_Egalitarian: endpoint CFs in species·yr/kg emitted",
  "  Same values as the species·yr sheets of SI_C and SI_D. The sum over the impact categories (total) is not included.",
  "",
  "Midpoint and endpoint sheets have one row per flow and impact category (column impact_category):",
  paste0("  ", paste(ecosystems$impact_category, collapse = ", ")),
  "",
  "Sheet midpoint_to_endpoint: conversion factors from midpoint to endpoint, in species·yr/kg PET, 1µm, sphere, rw/sw/ns-eq",
  "  Value = endpoint CF of the reference flow. CF_endpoint = CF_midpoint x conversion factor.",
  "  The reference flow (PET, 1µm) has the same CF for the three time horizons, so one factor per ecosystem applies to all perspectives.",
  "",
  "Sheet reference_CFs: midpoint CFs (PAF·d/kg) of the reference flow, per ecosystem and perspective.",
  "",
  "Sheet example: midpoint CFs (geometric means only) for PET 1µm and EPS 10µm spheres, World region.",
  "",
  "Variables:",
  "  impact_category: Freshwater, Marine or Terrestrial physical effects",
  "  unit: unit of the CF",
  "  elementary_flowname, region, polymer, size (µm, diameter or thickness), shape, emission_compartment: identification of the flow",
  "  geom_mean: CF value (geometric mean of the Monte Carlo runs)",
  "  gsd: geometric standard deviation",
  "  ll_95: lower limit of the 95% confidence interval (geom_mean / gsd^1.96)",
  "  ul_95: upper limit of the 95% confidence interval (geom_mean x gsd^1.96)",
  "  n_samples: number of Monte Carlo runs",
  "  For the midpoint, the reference CF is treated as a constant: the gsd is the gsd of CF_mid, and ll_95 and ul_95 are divided by the reference CF.",
  "  When all Monte Carlo runs are 0 (no transport pathway to the ecosystem), geom_mean is 0 and gsd, ll_95 and ul_95 are not defined (empty).",
  "",
  "Generated by results/ReCiPe/ReCiPe_CF.R"
)

## Save --------------------------------------------------------------------
wb <- createWorkbook()
header_style <- createStyle(textDecoration = "bold")

addWorksheet(wb, "README")
writeData(wb, "README", readme, colNames = FALSE)
addStyle(wb, "README", header_style, rows = 1, cols = 1)

for (p in perspectives$perspective) {
  sheet_name <- paste0("midpoint_", p)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, midpoint[[p]], withFilter = TRUE)
}

for (p in perspectives$perspective) {
  sheet_name <- paste0("endpoint_", p)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, endpoint[[p]], withFilter = TRUE)
}

addWorksheet(wb, "midpoint_to_endpoint")
writeData(wb, "midpoint_to_endpoint", conversion, headerStyle = header_style)

addWorksheet(wb, "reference_CFs")
writeData(wb, "reference_CFs", midpoint_references, headerStyle = header_style)

addWorksheet(wb, "example")
row <- 1
for (block in example_blocks) {
  writeData(wb, "example", block, startRow = row, headerStyle = header_style)
  row <- row + nrow(block) + 1
}
addStyle(wb, "example", createStyle(numFmt = "0.00E+00"), rows = 1:row, cols = 3:5, gridExpand = TRUE)
setColWidths(wb, "example", cols = 1:5, widths = c(40, 22, 14, 14, 14))

saveWorkbook(wb, out_file, overwrite = TRUE)
message("ReCiPe CFs saved in ", out_file)
