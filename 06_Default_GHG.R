rm(list = ls())  # Removes all objects from environment

library(openxlsx)
library(readxl)
library(dplyr)

# Paths are relative to SimpleBox/SBooScripts, as in 03_Default_CF.R
if (dir.exists("SimpleBox/SBooScripts")) setwd("SimpleBox/SBooScripts")

file <- "../../results/results_GHG_CI.xlsx" #output of 05_Run_SB_CI_GHGs.R

# Sheets that get World averages and default polymers/sizes, written to a new sheet "<sheet>_defaults" next to it
# The original sheets are kept unchanged
target_sheets <- c("CF_GWP", "GHG_emitted")
defaults_suffix <- "_defaults"

# Region weights - based on regional emissions (Louvet et al., 2026)
weights <- c(
  "North America"        = 0.00,
  "Latin America"        = 0.09,
  "Europe"               = 0.05,
  "Africa & Middle East" = 0.34,
  "Southeast Asia"       = 0.49,
  "Central Asia"         = 0.02,
  "Northern regions"     = 0.00,
  "Oceania"              = 0.00
)

# Global emission compartments to keep only world averages
global_compartments <- c(
  "global seawater surface",
  "global seawater column",
  "global marine sediments"
)

# Default size for each shape
size_mappings <- c(
  "Sphere" = 1000,
  "Fiber" = 10,
  "Film" = 100
)

# Polymer weights for the default polymers - based on polymer emissions
weightings_data <- read_excel("../../input/SI_B.xlsx", sheet = "4.3.polymer_emissions")
weightings_data <- weightings_data[!is.na(weightings_data$polymer), ]

density_weights <- list(
  "low density"  = setNames(coalesce(weightings_data$weighting_low_density, 0), weightings_data$polymer),
  "high density" = setNames(coalesce(weightings_data$weighting_high_density, 0), weightings_data$polymer),
  "all density"  = setNames(coalesce(weightings_data$weighting_all_density, 0), weightings_data$polymer)
)

id_cols <- c("elementary_flowname", "region", "polymer", "size", "shape", "emission_compartment")

# Functions -----------------------------------------------------------------

# Weighted mean over the rows with a value and a weight > 0 (NA if none, e.g. TRWP)
weighted_mean_na <- function(x, w) {
  ok <- !is.na(x) & w > 0
  if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
}

# Elementary flow name, same nomenclature as 05_Run_SB_CI_GHGs.R
# size_text: e.g. "10 µm", or "default" for the default size
make_flowname <- function(shape, pol, size_text, reg) {
  pol <- ifelse(startsWith(pol, "default_"), gsub("_", " ", pol), pol) #default_low_density -> default low density
  case_when(
    shape == "Sphere" ~ paste0("Microsphere/fragment - ", pol, " (", size_text, " diameter), ", reg),
    shape == "Fiber"  ~ paste0("Microfiber/cylinder - ", pol, " (", size_text, " diameter), ", reg),
    shape == "Film"   ~ paste0("Microfilm - ", pol, " (", size_text, " thickness), ", reg),
    TRUE              ~ paste0("Micro", tolower(shape), " - ", pol, ", ", reg)
  )
}

# Weighted average of all statistics columns over the rows of each group, using the column ..w as weights
# Statistics are averaged like in 03_Default_CF.R; n_zero is not meaningful for an average and is set to NA
average_stats <- function(df, group_cols, stat_cols) {
  zero_cols <- grep("_n_zero$", stat_cols, value = TRUE)
  df |>
    group_by(across(all_of(group_cols))) |>
    filter(any(..w > 0)) |>
    summarise(across(all_of(stat_cols), ~ weighted_mean_na(.x, ..w)), .groups = "drop") |>
    mutate(across(all_of(zero_cols), ~ NA_real_))
}

add_world_and_defaults <- function(df_orig, sheet_name) {
  message("Sheet: ", sheet_name)
  has_flowname <- "elementary_flowname" %in% names(df_orig)

  # Remove World and default rows from a previous run
  df <- df_orig |>
    filter(region != "World", !grepl("^default", polymer))

  stat_cols <- setdiff(names(df)[sapply(df, is.numeric)], "size")
  extra_cols <- setdiff(names(df), c(id_cols, stat_cols)) #e.g. indicator or time_horizon

  # 1. World average, weighted by regional emissions
  df_world <- df |>
    filter(region %in% names(weights)) |>
    mutate(..w = weights[region]) |>
    average_stats(c("polymer", "size", "shape", "emission_compartment", extra_cols), stat_cols) |>
    mutate(region = "World")

  # Global emission compartments: keep only the World average
  n_global <- sum(df$emission_compartment %in% global_compartments)
  message("  Removing ", n_global, " regional rows for global compartments")
  df <- df |>
    filter(!emission_compartment %in% global_compartments) |>
    bind_rows(df_world)

  # 2. Default polymers, weighted by polymer emissions (in every region, World included)
  df_defaults <- bind_rows(lapply(names(density_weights), function(label) {
    w <- density_weights[[label]]
    df |>
      mutate(..w = coalesce(unname(w[polymer]), 0)) |> #polymers without weight (e.g. bio-based) get 0
      average_stats(c("region", "size", "shape", "emission_compartment", extra_cols), stat_cols) |>
      mutate(polymer = paste0("default_", gsub(" ", "_", label)))
  }))
  message("  Added ", nrow(df_world), " World rows and ", nrow(df_defaults), " default polymer rows")

  df_out <- bind_rows(df, df_defaults)

  if (has_flowname) {
    df_out <- df_out |>
      mutate(elementary_flowname = ifelse(is.na(elementary_flowname),
                                          make_flowname(shape, polymer, paste(size, "µm"), region),
                                          elementary_flowname))

    # 3. Default size for each shape: copy of the rows at that size, named "default diameter/thickness"
    df_default_size <- df_out |>
      filter(size == size_mappings[shape]) |>
      mutate(elementary_flowname = make_flowname(shape, polymer, "default", region))
    message("  Added ", nrow(df_default_size), " default size rows")

    df_out <- bind_rows(df_out, df_default_size)
  }

  message("  Final row count: ", nrow(df_out))
  df_out[names(df_orig)]
}

# Process -----------------------------------------------------------------

wb <- loadWorkbook(file)

# Remove the defaults sheets of a previous run
for (sheet_name in intersect(paste0(target_sheets, defaults_suffix), names(wb))) {
  removeWorksheet(wb, sheet_name)
}
sheet_order <- names(wb)

for (sheet_name in intersect(target_sheets, names(wb))) {
  df_out <- add_world_and_defaults(readWorkbook(wb, sheet = sheet_name), sheet_name)
  new_sheet <- paste0(sheet_name, defaults_suffix)
  addWorksheet(wb, new_sheet)
  writeData(wb, new_sheet, df_out, withFilter = TRUE)
  freezePane(wb, new_sheet, firstRow = TRUE)

  # Place the defaults sheet right after its original sheet
  pos <- match(sheet_name, sheet_order)
  sheet_order <- append(sheet_order, new_sheet, after = pos)
}

# Document the World and default rows in the README
readme <- readWorkbook(wb, sheet = "README") |>
  filter(!Item %in% c("World", "Default polymers", "Default size")) |>
  bind_rows(data.frame(
    Item = c("World", "Default polymers", "Default size"),
    Value = c(paste0("Average of the regions weighted by regional emissions (",
                     paste0(names(weights), " = ", weights, collapse = "; "),
                     "). Global emission compartments are only given for World. Sheets: ",
                     paste0(target_sheets, defaults_suffix, collapse = ", ")),
              "default_low_density, default_high_density, default_all_density: average of the polymers weighted by polymer emissions (SI_B, 4.3.polymer_emissions)",
              paste0("Copy of the rows at ", paste0(names(size_mappings), " = ", size_mappings, " µm", collapse = "; "),
                     ", named (default diameter) or (default thickness). Sheet: CF_GWP", defaults_suffix))
  ))
removeWorksheet(wb, "README")
addWorksheet(wb, "README")
writeData(wb, "README", readme)
worksheetOrder(wb) <- match(sheet_order, names(wb)) #new sheets are added at the end: put each defaults sheet after its original

saveWorkbook(wb, file, overwrite = TRUE)

message("\n=== Processing complete ===")
