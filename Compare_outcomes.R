## Load libraries ----------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(MASS)
library(Rmpfr)

## Load data ---------------------------------------------------------------
filepath_paper <- "results_FF_CF_CI_4.xlsx"
filepath_testrun <- "Output/results_FF_CF_CI_4_test.xlsx"

## Compare data ------------------------------------------------------------

### Compare per sheet
results_FF_paper <- read.xlsx(filepath_paper, sheet = 1)
results_FF_testrun <- read.xlsx(filepath_testrun, sheet = 1)

common_cols <- c("region", "polymer", "size", "shape", "emission_compartment", "receiving_compartment")

# Many more rows in the test I ran than in the results provided. Further checks: 

# Unique rows in testrun
only_in_test <- anti_join(
  results_FF_testrun, 
  results_FF_paper, 
  by = common_cols
)

# Unique rows in results paper
only_in_paper <- anti_join(
  results_FF_paper, 
  results_FF_testrun, 
  by = common_cols
)

# Check the unique values for each variable
for (col in common_cols) {
  cat("\n=== ", col, " ===\n")
  vals_paper <- sort(unique(results_FF_paper[[col]]))
  vals_test  <- sort(unique(results_FF_testrun[[col]]))
  cat("In paper, aantal uniek:", length(vals_paper), "\n")
  cat("In test, aantal uniek: ", length(vals_test), "\n")
  
  only_in_paper <- setdiff(vals_paper, vals_test)
  only_in_test  <- setdiff(vals_test, vals_paper)
  
  if (length(only_in_paper) > 0) {
    cat("Alleen in paper:", paste(only_in_paper, collapse=", "), "\n")
  }
  if (length(only_in_test) > 0) {
    cat("Alleen in test: ", paste(only_in_test, collapse=", "), "\n")
  }
  if (length(only_in_paper) == 0 & length(only_in_test) == 0) {
    cat("Waarden komen overeen!\n")
  }
}

# There are materials and regions that occur in the test run but not in the results paper. 
# For now let's filter for the same unique values in both dfs and continue analysis. 

# Join the data frames and filter for common unique values
results_FF_comparison <- left_join(
  results_FF_paper,
  results_FF_testrun,
  by = common_cols,
  suffix = c("_Paper", "_Test")
) |>
  mutate(ff_geom_mean_diff = ff_geom_mean_Paper - ff_geom_mean_Test,
         rel_diff = ff_geom_mean_diff/ff_geom_mean_Paper) |>
  filter(region %in% unique(results_FF_paper$region)) |>
  filter(polymer %in% unique(results_FF_paper$polymer))

# Histogram van relatieve verschillen
ggplot(results_FF_comparison, aes(x = rel_diff)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(title = "Relatief verschil Paper vs Test", x = "Relatief verschil", y = "Aantal")

# Scatter plot log-log
ggplot(results_FF_comparison, aes(x = ff_geom_mean_Paper, y = ff_geom_mean_Test)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Vergelijking Paper vs Test", x = "Paper", y = "Test")

# For 2577 mean ffs the mean is zero in the results you provided, but I get values different from zero. why? 
reldif_inf <- results_FF_comparison |>
  filter(is.infinite(rel_diff))
