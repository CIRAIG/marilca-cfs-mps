## Load libraries ----------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(MASS)
library(Rmpfr)

## Load data ---------------------------------------------------------------
filepath_paper <- "results_FF_CF_CI_5.xlsx"
filepath_testrun <- "Output/results_FF_CF_CI_4_test.xlsx"

## Define functions for comparison
compare_excel_sheets <- function(filepath_paper, filepath_testrun, sheet_num = 1, compare_cols, diff_cols) {
  # Lees de data in
  df_paper <- openxlsx::read.xlsx(filepath_paper, sheet = sheet_num)
  df_test  <- openxlsx::read.xlsx(filepath_testrun, sheet = sheet_num)
  
  # Unieke waarden per compare_col
  for (col in compare_cols) {
    cat("\n=== ", col, " ===\n")
    vals_paper <- sort(unique(df_paper[[col]]))
    vals_test  <- sort(unique(df_test[[col]]))
    cat("In paper, aantal uniek:", length(vals_paper), "\n")
    cat("In test, aantal uniek: ", length(vals_test), "\n")
    
    only_in_paper <- setdiff(vals_paper, vals_test)
    only_in_test  <- setdiff(vals_test, vals_paper)
    
    if (length(only_in_paper) > 0) {
      cat("Only in paper:", paste(only_in_paper, collapse=", "), "\n")
    }
    if (length(only_in_test) > 0) {
      cat("Only in test: ", paste(only_in_test, collapse=", "), "\n")
    }
    if (length(only_in_paper) == 0 & length(only_in_test) == 0) {
      cat("Parameters are the same!\n")
    }
  }
  
  # Joinen op compare_cols en verschil berekenen voor diff_cols
  df_comp <- left_join(
    df_paper,
    df_test,
    by = compare_cols,
    suffix = c("_Paper", "_Test")
  )
  
  # Voeg kolommen toe met verschil en relatief verschil
  for (col in diff_cols) {
    paper_col <- paste0(col, "_Paper")
    test_col  <- paste0(col, "_Test")
    diff_col  <- paste0(col, "_diff")
    rel_col   <- paste0(col, "_rel_diff")
    df_comp[[diff_col]] <- df_comp[[paper_col]] - df_comp[[test_col]]
    df_comp[[rel_col]]  <- df_comp[[diff_col]] / df_comp[[paper_col]]
  }
  
  return(df_comp)
}

## Compare mean FFs ------------------------------------------------------------
comparison_result_ff <- compare_excel_sheets(
  filepath_paper = filepath_paper,
  filepath_testrun = filepath_testrun,
  sheet_num = 1,
  compare_cols = c("region", "polymer", "size", "shape", "emission_compartment", "receiving_compartment"),
  diff_cols = c("ff_geom_mean") # vul hier de kolomnamen in waarvoor je verschil wilt berekenen
)

# Histogram of relative differences
ggplot(comparison_result_ff, aes(x = ff_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean FFs in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_ff, aes(x = ff_geom_mean_Paper, y = ff_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean FFs in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_ff |>
  filter(is.infinite(rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

## Compare mean CFs mid PAF day ------------------------------------------------------------
comparison_result_cf_paf_day <- compare_excel_sheets(
  filepath_paper = filepath_paper,
  filepath_testrun = filepath_testrun,
  sheet_num = 2,
  compare_cols = c("region", "polymer", "size", "shape", "emission_compartment"),
  diff_cols = c("terrestrial_geom_mean", "marine_geom_mean", "freshwater_geom_mean") # vul hier de kolomnamen in waarvoor je verschil wilt berekenen
)

### Terrestrial ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_paf_day, aes(x = terrestrial_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_paf_day, aes(x = terrestrial_geom_mean_Paper, y = terrestrial_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_paf_day |>
  filter(is.infinite(terrestrial_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Freshwater ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_paf_day, aes(x = freshwater_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean freshwater CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_paf_day, aes(x = freshwater_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean freshwater CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_paf_day |>
  filter(is.infinite(freshwater_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Marine ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_paf_day, aes(x = marine_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean marine CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_paf_day, aes(x = marine_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean marine CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_paf_day |>
  filter(is.infinite(marine_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

## Compare mean CF mid PDF ---------------------------------------------------------

## Compare CF end PDF
comparison_result_cf_mid_pdf <- compare_excel_sheets(
  filepath_paper = filepath_paper,
  filepath_testrun = filepath_testrun,
  sheet_num = 3,
  compare_cols = c("region", "polymer", "size", "shape", "emission_compartment"),
  diff_cols = c("terrestrial_geom_mean", "marine_geom_mean", "freshwater_geom_mean") # vul hier de kolomnamen in waarvoor je verschil wilt berekenen
)

### Terrestrial ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_mid_pdf, aes(x = terrestrial_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_mid_pdf, aes(x = terrestrial_geom_mean_Paper, y = terrestrial_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_mid_pdf |>
  filter(is.infinite(terrestrial_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Freshwater ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_mid_pdf, aes(x = freshwater_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean freshwater CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_mid_pdf, aes(x = freshwater_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean freshwater CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_mid_pdf |>
  filter(is.infinite(freshwater_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Marine ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_mid_pdf, aes(x = marine_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean marine CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_mid_pdf, aes(x = marine_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean marine CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_mid_pdf |>
  filter(is.infinite(marine_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

## Compare mean CF end species year ---------------------------------------------------------

## Compare CF end PDF
comparison_result_cf_end_species_year <- compare_excel_sheets(
  filepath_paper = filepath_paper,
  filepath_testrun = filepath_testrun,
  sheet_num = 4,
  compare_cols = c("region", "polymer", "size", "shape", "emission_compartment"),
  diff_cols = c("terrestrial_geom_mean", "marine_geom_mean", "freshwater_geom_mean") # vul hier de kolomnamen in waarvoor je verschil wilt berekenen
)

### Terrestrial ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_end_species_year, aes(x = terrestrial_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_end_species_year, aes(x = terrestrial_geom_mean_Paper, y = terrestrial_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_end_species_year |>
  filter(is.infinite(terrestrial_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Freshwater ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_end_species_year, aes(x = freshwater_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean freshwater CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_end_species_year, aes(x = freshwater_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean freshwater CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_end_species_year |>
  filter(is.infinite(freshwater_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Marine ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_end_species_year, aes(x = marine_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean marine CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_end_species_year, aes(x = marine_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean marine CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_end_species_year |>
  filter(is.infinite(marine_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

## Compare mean CF end m2 year ---------------------------------------------------------

## Compare CF end PDF
comparison_result_cf_end_species_year <- compare_excel_sheets(
  filepath_paper = filepath_paper,
  filepath_testrun = filepath_testrun,
  sheet_num = 4,
  compare_cols = c("region", "polymer", "size", "shape", "emission_compartment"),
  diff_cols = c("terrestrial_geom_mean", "marine_geom_mean", "freshwater_geom_mean") # vul hier de kolomnamen in waarvoor je verschil wilt berekenen
)

### Terrestrial ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_end_species_year, aes(x = terrestrial_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_end_species_year, aes(x = terrestrial_geom_mean_Paper, y = terrestrial_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean terrestrial CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_end_species_year |>
  filter(is.infinite(terrestrial_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Freshwater ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_end_species_year, aes(x = freshwater_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean freshwater CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_end_species_year, aes(x = freshwater_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean freshwater CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_end_species_year |>
  filter(is.infinite(freshwater_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)

### Marine ----------------------------------------------------------------------------

# Histogram of relative differences
ggplot(comparison_result_cf_end_species_year, aes(x = marine_geom_mean_rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Comparison reported mean marine CFs PAF day in Paper vs Test", x = "Relative difference", y = "Number of values")

# Scatter plot log-log
ggplot(comparison_result_cf_end_species_year, aes(x = marine_geom_mean_Paper, y = freshwater_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Comparison reported mean marine CFs PAF day in in Paper vs Test", x = "Paper", y = "Test")

# For 3 mean ffs the mean is zero in the results you provided. The values are very small however (<1*10^-15), so could be due to solver problems and should not make a large difference. 
reldif_inf <-comparison_result_cf_end_species_year |>
  filter(is.infinite(marine_geom_mean_rel_diff))

print(nrow(reldif_inf))

unique(reldif_inf$emission_compartment)

unique(reldif_inf$receiving_compartment)



