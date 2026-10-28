# Compare the results of this branch (SI_C and SI_D) with the results of another git branch (default: main)
# Run from the repository root after 02_Run_SB_CI.R and 03_Default_CF.R:
#   source("Compare_with_main.R")
# Output: a summary in the console and comparison_vs_<ref>.xlsx (summary + ratios new/ref per CF)

## Load libraries ----------------------------------------------------------
library(tidyverse)
library(openxlsx)

## Settings ----------------------------------------------------------------
ref_branch <- "main"
files_to_compare <- c("results/SI_C.xlsx", "results/SI_D.xlsx")
out_file <- paste0("comparison_vs_", ref_branch, ".xlsx")
write_details_for_FF <- FALSE #FF sheets are large (>600k rows): only summarised unless TRUE

stat_pattern <- "(geom_mean|gsd|ll_95|ul_95|n_samples)$"

## Functions ---------------------------------------------------------------
# Extract a file from a git branch into a temporary file
git_file <- function(ref, path) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  status <- system2("git", c("show", shQuote(paste0(ref, ":", path))), stdout = tmp)
  if (!identical(as.integer(status), 0L)) stop("Could not read ", path, " from ", ref)
  tmp
}

# Key and value columns of a sheet
sheet_columns <- function(df) {
  stat_cols <- grep(stat_pattern, names(df), value = TRUE)
  if (length(stat_cols) > 0) {
    keys <- setdiff(names(df), stat_cols)
    values <- grep("(geom_mean|gsd)$", stat_cols, value = TRUE)
  } else {
    # e.g. CF_TRWP: one column per region, values may be stored as text
    is_num <- sapply(df, function(x) mean(!is.na(suppressWarnings(as.numeric(x)))) > 0.5)
    keys <- names(df)[!is_num]
    values <- names(df)[is_num]
  }
  list(keys = keys, values = values)
}

compare_sheet <- function(df_new, df_ref) {
  cols <- sheet_columns(df_ref)
  keys <- intersect(cols$keys, names(df_new))
  values <- intersect(cols$values, names(df_new))

  prep <- function(df) {
    df |>
      mutate(across(all_of(keys), as.character),
             across(all_of(values), ~ suppressWarnings(as.numeric(.x)))) |>
      group_by(across(all_of(keys))) |>
      mutate(.dup = row_number()) |> #keeps rows with identical keys apart
      ungroup() |>
      dplyr::select(all_of(c(keys, ".dup", values)))
  }

  joined <- full_join(prep(df_new), prep(df_ref), by = c(keys, ".dup"), suffix = c("_new", "_ref"))
  for (v in values) {
    new <- joined[[paste0(v, "_new")]]
    ref <- joined[[paste0(v, "_ref")]]
    joined[[paste0(v, "_ratio")]] <- ifelse(new == 0 & ref == 0, 1, new / ref)
  }
  list(joined = joined, keys = keys, values = values)
}

summarise_comparison <- function(cmp, file, sheet) {
  j <- cmp$joined
  group <- if ("polymer" %in% names(j)) ifelse(j$polymer == "TRWP", "TRWP", "other polymers") else "all"
  map_dfr(cmp$values, function(v) {
    tibble(group = group,
           new = j[[paste0(v, "_new")]],
           ref = j[[paste0(v, "_ref")]],
           ratio = j[[paste0(v, "_ratio")]]) |>
      group_by(group) |>
      summarise(
        n_rows = n(),
        only_in_new = sum(is.na(ref) & !is.na(new)),
        only_in_ref = sum(is.na(new) & !is.na(ref)),
        n_compared = sum(is.finite(ratio)),
        median_ratio = median(ratio[is.finite(ratio)]),
        p05_ratio = quantile(ratio[is.finite(ratio)], 0.05, names = FALSE),
        p95_ratio = quantile(ratio[is.finite(ratio)], 0.95, names = FALSE),
        max_abs_log10_ratio = max(abs(log10(ratio[is.finite(ratio) & ratio > 0])), -Inf),
        share_within_1pct = mean(abs(ratio[is.finite(ratio)] - 1) <= 0.01),
        share_within_10pct = mean(abs(ratio[is.finite(ratio)] - 1) <= 0.10),
        .groups = "drop"
      ) |>
      mutate(file = basename(file), sheet = sheet, value = v, .before = 1)
  })
}

## Compare -----------------------------------------------------------------
summary_all <- list()
details <- list()

for (f in files_to_compare) {
  ref_path <- git_file(ref_branch, f)
  sheets <- setdiff(intersect(getSheetNames(f), getSheetNames(ref_path)), "Cover page")
  missing <- setdiff(union(getSheetNames(f), getSheetNames(ref_path)), c(sheets, "Cover page"))
  if (length(missing) > 0) message(f, ": sheets not in both versions: ", paste(missing, collapse = ", "))

  for (sh in sheets) {
    message("Comparing ", f, " - ", sh)
    cmp <- compare_sheet(read.xlsx(f, sheet = sh), read.xlsx(ref_path, sheet = sh))
    summary_all[[paste(f, sh)]] <- summarise_comparison(cmp, f, sh)
    if (write_details_for_FF || !grepl("^FF", sh)) {
      details[[paste0(sub("\\.xlsx$", "", basename(f)), "_", sh)]] <- cmp$joined |>
        dplyr::select(all_of(cmp$keys), ends_with("_ratio"), everything(), -.dup)
    }
  }
}

summary_df <- bind_rows(summary_all)
print(summary_df |>
        filter(grepl("total_geom_mean|ff_geom_mean", value) | !grepl("geom_mean|gsd", value)) |>
        dplyr::select(file, sheet, value, group, n_compared, only_in_new, only_in_ref,
                      median_ratio, p05_ratio, p95_ratio, share_within_10pct),
      n = Inf, width = Inf)

## Save --------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_df, withFilter = TRUE)
for (nm in names(details)) {
  sheet_name <- substr(nm, 1, 31) #Excel sheet names are max 31 characters
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, details[[nm]], withFilter = TRUE)
}
saveWorkbook(wb, out_file, overwrite = TRUE)
message("Comparison saved in ", out_file)
