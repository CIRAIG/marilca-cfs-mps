# Set up SimpleBox----------------------------------------------------------

## Load libraries ----------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(MASS)
library(Rmpfr)
library(expm)
library(parallel)

## Install SBoo ------------------------------------------------------------
# Only install SBoo if it is not installed yet
if (!dir.exists("SimpleBox")){
  source("InstallSBoo.R")
}

setwd("SimpleBox/SBooScripts")

# Read csvs

plastic_values <- read.xlsx("../../input/SI_B.xlsx", sheet = "3.2.polymer_list") 
regions <- read.xlsx("../../input/SI_B.xlsx", sheet = "3.1.regional_data") 
trackmpd <- read.xlsx("../../input/trackmpd_data/Velocities_all_weighted.xlsx", sheet = "LongFormat") 
colnames(regions) <- regions[3,]
regions_rows = nrow(regions)
degradation_CI_all <- readxl::read_xlsx("../../input/SI_B.xlsx", 
                                        sheet = "3.3.polymer_data_CI")[, 1:9]

#Import the data with regionalization. Some variables are left as the default input of SBoo
#If no variable is provided in the regio sheet, the default value is kept
regions <- regions |>
  slice(4:regions_rows)|>
  filter(!is.na(Variable)) |>
  dplyr::select(Variable, Scale, SubCompart, `North America`, `Latin America`, `Europe`, 
                `Africa & Middle East`, `Central Asia`, `Southeast Asia`, `Northern regions`, `Oceania`) |>
  #dplyr::select(Variable, Scale, SubCompart, `Southeast Asia`, `Northern regions`,`Central Asia`) |> #test with SEA and NR only
  rename(varName = Variable)


## Initialize World -----------------------------------------------------
use_scenario_data <- TRUE # Use the scenario-specific data in marilca-cfs-mps/data

source("baseScripts/initWorld.R")

#If Test FALSE: new version of SB, if True, then the old version (excel)
#World$SetConst(Test = "TRUE")
World$SetConst(Test_surface_water = "TRUE")
World$SetConst(Regional_and_Continental_deepocean = "TRUE")
World$SetConst(Remove_global = "TRUE") #if true, remove flows to moderate, arctic and tropic
World$UpdateKaas(mergeExisting = FALSE)

setup <- NULL

# Check advection k's 
kaas <- World$kaas |>
  filter(process == "k_Advection")

## Rescale landscape -----------------------------------------------------

# Change global landscape properties - make scales really small to remove them.
# In this case, the continental scale of SBoo IS USED AS THE GLOBAL -  and the regional is used as the continental
# It is because SBoo has 3 different global regions, and a regional, which we don't need in LCIA
# So regional(SBoo)=continental(LCIA) and continental(SBoo)=global(LCIA)

# The continental and regional are nested in the moderate scale. TotalArea needs to be the some of comp+nested comp.
global_df <- data.frame(varName = c("TotalArea", "TotalArea", "TotalArea","FRACsea"), 
                        Scale = c("Arctic", "Moderate", "Tropic", "Moderate"),
                        Waarde = c(10^-20,  502268688752959+1 , 10^-20, 0.7244500823950340000000000000)) 

World$mutateVars(global_df)
World$UpdateDirty(unique(global_df$varName))

# Remove flows to moderate, arctic, tropic
global_df <- data.frame(varName = c("x_FromModerate2ArctWater", "x_OceanMixing2Deep", "x_OceanMixing2Sea","FRACsea"), 
                        Waarde = c(10^-20,  10^-20, 10^-20, 0.7244500823950340000000000000)) 

World$mutateVars(global_df)
World$UpdateDirty(unique(global_df$varName))

# Remove flows to moderate, arctic, tropic
airflow_df <- data.frame(varName = c("AirFlow", "AirFlow", "AirFlow"), 
                         Scale = c("Arctic", "Moderate", "Tropic"),
                         Waarde = c(10^-20,  10^-20, 10^-20)) 
World$mutateVars(airflow_df)
World$UpdateDirty(unique(airflow_df$varName))

# Remove flows to moderate, artic, tropic
current_df <- data.frame(varName = c("OceanCurrent"), 
                         Waarde = c(10^-20)) 

# Remove flows to moderate, artic, tropic
current_df <- data.frame(varName = c("x_ToModerateWater"), 
                         fromScale = c("Continental"),
                         Waarde = c(10^-20)) 

World$mutateVars(current_df)
World$UpdateDirty(unique(current_df$varName))

## Adjust SBoo variables  ------------------------------------------------

# Set the interception in soil
intercept_df <- data.frame(varName = c("VegInterceptFrac","SizeRunoff"), 
                           Waarde = c(0.9715,50)) #SizeRunoff should be based on radius = 50 (ab)

World$mutateVars(intercept_df)
World$UpdateDirty(unique(intercept_df$varName))

# Set the netsedrate in deepocean
sedrate_df <- data.frame(varName = c("NETsedrate"), 
                         Scale = c("Continental"),
                         SubCompart = c("sea"),
                           Waarde = c(6.34e-11)) #same as regional "sea"

World$mutateVars(sedrate_df)
World$UpdateDirty(unique(sedrate_df$varName))



# Change the drag method to Dioguardi 2018
DragMethod_df <- data.frame(varName = "DragMethod",
                            Waarde = "Bagheri")
World$mutateVars(DragMethod_df)
World$UpdateDirty(unique(DragMethod_df$varName))

# Set the depth of all sediments and soil to 10cm, as per USEtox
global_df <- data.frame(varName = c("VertDistance", "VertDistance", "VertDistance", "VertDistance", "VertDistance", "VertDistance"), 
                        SubCompart = c("freshwatersediment","lakesediment","marinesediment","naturalsoil","agriculturalsoil","othersoil"),
                        Waarde = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)) 

World$mutateVars(global_df)
World$UpdateDirty(unique(global_df$varName))

# Remove fragmentation
kfrag_df <- data.frame(varName = c("kfrag"), 
                       Waarde = c(0))
World$mutateVars(kfrag_df)
World$UpdateDirty(unique(kfrag_df$varName))


## Set up GHG variables -------------------------------------------------

region_names <- colnames(regions)[4:11]
region_names <- region_names[!is.na(region_names)]
polymer_names <- colnames(plastic_values)[3:ncol(plastic_values)]

sizes <- c(1,10,100,1000,5000) #D will be divided by 2
shapes <- c("Sphere","Fiber","Film")
n_samples <- 100 #Monte Carlo runs per region/polymer/size/shape
seed <- 123 #same LHS samples for every combination, so differences between combinations are not due to sampling
n_cores <- if (.Platform$OS.type == "windows") 1 else max(1, parallel::detectCores() - 1) #polymers are run in parallel (forking is not available on Windows)

seconds_per_year <- 365.25 * 24 * 3600
time_horizons <- c(GWP20 = 20, GWP100 = 100, GWP500 = 500) #[yrs] mass degraded is integrated from 0 to the time horizon of each GWP

# GWP values from IPCC AR6 WG1, Chapter 7, Table 7.15 [kg CO2-eq/kg gas]
GWP_CO2 <- c(GWP20 = 1, GWP100 = 1, GWP500 = 1)
GWP_CH4_fossil <- c(GWP20 = 82.5, GWP100 = 29.8, GWP500 = 10.0)
GWP_CH4_nonfossil <- c(GWP20 = 79.7, GWP100 = 27.0, GWP500 = 7.2)

# Bio-based polymers: in the biogenic-adjusted CFs, CO2 counts as 0 and CH4 uses the non-fossil GWP
# All other polymers are fossil-based, so both CFs are equal
biogenic_polymers <- c("PHA","PLA","starch_blend","Cotton","Linen","Viscose","Rayon","Lyocell","Modal")

#list of possible emission compartments to loop over
#only SOLID
emission_compartments <- c("aR","w1R","w0R","w2R", "w3R","sd1R","sd0R","sd2R","s1R","s2R",
                           "w2C", "w3C","sd2C") #At global scale, keep only marine environments
compartment_names <- c( #fully written names
  "aR"  = "continental air",
  "cwR"  = "continental cloudwater",
  "w1R" = "continental riverwater",
  "w0R" = "continental lakewater",
  "w2R" = "continental seawater surface",
  "w3R" = "continental seawater column",
  "sd0R"= "continental lake sediments",
  "sd1R"= "continental freshwater sediments",
  "sd2R"= "continental marine sediments",
  "s1R" = "continental natural soil",
  "s2R" = "continental agricultural soil",
  "aC"  = "global air",
  "cwC" = "global cloudwater",
  "w1C" = "global riverwater",
  "w0C" = "global lakewater",
  "w2C" = "global seawater surface",
  "w3C" = "global seawater column",
  "sd0C"= "global lake sediments",
  "sd1C"= "global freshwater sediments",
  "sd2C"= "global marine sediments",
  "s1C" = "global natural soil",
  "s2C" = "global agricultural soil"
)

X_list <- c("aR", "cwR","w1R","w0R","w2R", "w3R","sd1R","sd0R","sd2R","s1R","s2R",    #careful, this list contains cw (to for the X matrix)
            "aC", "cwC","w1C","w0C","w2C", "w3C","sd1C","sd0C","sd2C","s1C","s2C")

# Receiving compartments reported for mass degraded: cloudwater is merged into air
receiving_compartments <- setdiff(X_list, c("cwR","cwC"))

## Degradation products ---------------------------------------------------

deg_file <- "../../input/Degradation products/Degradation product calculation.xlsx"

# Theoretical mass of CO2 and CH4 produced per kg of polymer degraded
M_deg_sheet <- readxl::read_xlsx(deg_file, sheet = "M_deg", skip = 1) #first row holds the units
molar_masses <- M_deg_sheet |>
  filter(!is.na(Variables)) |>
  dplyr::select(Variables, Value) |>
  deframe()
MM_CO2 <- as.numeric(molar_masses["MM_CO2"])
MM_CH4 <- as.numeric(molar_masses["MM_CH4"])

M_deg <- M_deg_sheet |>
  filter(Variable %in% polymer_names) |>
  transmute(polymer = Variable,
            M_CO2_aerobic = suppressWarnings(as.numeric(M_CO2_th_aerobic)),     #kg CO2/kg polymer
            M_CO2_anaerobic = suppressWarnings(as.numeric(M_CO2_th_anaerobic)), #kg CO2/kg polymer
            M_CH4_anaerobic = suppressWarnings(as.numeric(M_CH4_th_anaerobic))) #kg CH4/kg polymer

# Fraction of the CO2 and CH4 produced in each compartment that reaches the atmosphere
f_atm <- readxl::read_xlsx(deg_file, sheet = "f_atm")
colnames(f_atm)[1:6] <- c("compartment", "redox", "stoichiometry", "f_atm_CO2", "f_atm_CH4", "f_ox_CH4")
f_atm <- f_atm |>
  dplyr::select(compartment, redox, f_atm_CO2, f_atm_CH4, f_ox_CH4) |>
  mutate(across(c(f_atm_CO2, f_atm_CH4, f_ox_CH4), ~ replace_na(suppressWarnings(as.numeric(.x)), 0))) #n/a: no CH4 produced

# Map each SBoo compartment to the start of its row label in the f_atm sheet
f_atm_compartment <- c(
  "aR"  = "Air",
  "cwR" = "Air",
  "w1R" = "River water",
  "w0R" = "Lake water",
  "w2R" = "Marine surface",
  "w3R" = "Continental seawater column",
  "sd1R"= "River sediments",
  "sd0R"= "Lake sediments",
  "sd2R"= "Continental marine sediments",
  "s1R" = "Soil",
  "s2R" = "Soil",
  "aC"  = "Air",
  "cwC" = "Air",
  "w1C" = "River water",
  "w0C" = "Lake water",
  "w2C" = "Marine surface",
  "w3C" = "Global seawater column",
  "sd1C"= "River sediments",
  "sd0C"= "Lake sediments",
  "sd2C"= "Global marine sediments",
  "s1C" = "Soil",
  "s2C" = "Soil"
)[X_list]

f_atm_rows <- sapply(f_atm_compartment, function(label) {
  row <- which(startsWith(f_atm$compartment, label))
  if (length(row) != 1) stop("f_atm sheet: expected one row starting with '", label, "', found ", length(row))
  row
})
f_atm_mapped <- f_atm[f_atm_rows, ] |>
  mutate(Abbr = X_list, compartment_name = compartment_names[X_list], .before = 1)

# kg CO2 and kg CH4 reaching the atmosphere per kg of polymer degraded in each compartment
# Oxic: full oxidation to CO2. Anoxic: CO2 + CH4 (Boyle), the oxidised CH4 becomes CO2 and follows f_atm,CO2
ghg_per_kg_degraded <- function(pol) {
  m <- M_deg[M_deg$polymer == pol, ]
  ghg <- matrix(NA_real_, nrow = length(X_list), ncol = 2, dimnames = list(X_list, c("CO2", "CH4")))
  if (nrow(m) != 1 || anyNA(c(m$M_CO2_aerobic, m$M_CO2_anaerobic, m$M_CH4_anaerobic))) {
    return(ghg) #no stoichiometry (e.g. TRWP): GHG results are NA
  }
  anoxic <- f_atm_mapped$redox == "Anoxic"
  ghg[, "CO2"] <- ifelse(anoxic,
                         (m$M_CO2_anaerobic + m$M_CH4_anaerobic * f_atm_mapped$f_ox_CH4 * MM_CO2 / MM_CH4) * f_atm_mapped$f_atm_CO2,
                         m$M_CO2_aerobic * f_atm_mapped$f_atm_CO2)
  ghg[, "CH4"] <- ifelse(anoxic, m$M_CH4_anaerobic * f_atm_mapped$f_atm_CH4, 0)
  ghg
}

## Functions for matrix aggregations ------------------------------------
Xfree <- function(k_free_agg, k_free_att, k_agg_tot, k_att_tot) {
  1 / (1 + (k_free_agg / k_agg_tot) + (k_free_att / k_att_tot))
}
Xagg <- function(k_free_agg, k_free_att, k_agg_tot, k_att_tot) {
  (k_free_agg / k_agg_tot) / (1 + (k_free_agg / k_agg_tot) + (k_free_att / k_att_tot))
}
Xatt <- function(k_free_agg, k_free_att, k_agg_tot, k_att_tot) {
  (k_free_att / k_att_tot) / (1 + (k_free_agg / k_agg_tot) + (k_free_att / k_att_tot))
}

# Pre-compute only the STRUCTURE (which cells to fill)
setup_fill_patterns <- function(first_mat, X_list) {
  comp_names <- rownames(first_mat)

  # Vectorized filtering (keep_idx - same for all)
  keep_idx <- !grepl("[UMT]", comp_names)
  name_len <- nchar(comp_names)
  before_last <- ifelse(name_len >= 2,
                        substr(comp_names, name_len - 1, name_len - 1),
                        "")
  keep_idx <- keep_idx & (before_last != "A") & !grepl("s3", comp_names, fixed = TRUE)

  filtered_names <- comp_names[keep_idx]

  # Create A matrix pattern (same for all - always 0 or 1)
  A_mat <- matrix(0, nrow = length(X_list), ncol = length(filtered_names))
  rownames(A_mat) <- X_list
  colnames(A_mat) <- filtered_names

  # Fill A pattern (always 1 for S/A/P compartments)
  for(prefix in X_list) {
    for(suffix in c("S", "A", "P")) {
      full_name <- paste0(prefix, suffix)
      if(full_name %in% filtered_names) {
        A_mat[prefix, full_name] <- 1
      }
    }
  }

  return(list(
    keep_idx = keep_idx,
    filtered_names = filtered_names,
    A_mat = A_mat,
    X_list = X_list
  ))
}

# Function to fill X matrix for a specific k_matrix
fill_X_matrix <- function(k_mat_filtered, setup) {
  # Create empty X matrix
  X <- matrix(0, nrow = nrow(k_mat_filtered), ncol = length(setup$X_list))
  colnames(X) <- setup$X_list
  rownames(X) <- rownames(k_mat_filtered)

  # Fill X matrix using actual k_matrix values
  for(prefix in setup$X_list) {
    free <- paste0(prefix, "S")
    agg <- paste0(prefix, "A")
    att <- paste0(prefix, "P")

    # Check if all compartments exist in filtered matrix
    if(all(c(free, agg, att) %in% rownames(k_mat_filtered))) {
      k_free_agg <- abs(k_mat_filtered[agg, free])
      k_free_att <- abs(k_mat_filtered[att, free])
      k_agg_tot <- abs(k_mat_filtered[agg, agg])
      k_att_tot <- abs(k_mat_filtered[att, att])

      X[free, prefix] <- Xfree(k_free_agg, k_free_att, k_agg_tot, k_att_tot)
      X[agg, prefix] <- Xagg(k_free_agg, k_free_att, k_agg_tot, k_att_tot)
      X[att, prefix] <- Xatt(k_free_agg, k_free_att, k_agg_tot, k_att_tot)
    }
  }

  return(X)
}

# Aggregate free (S), aggregated (A) and attached (P) particles of one k_matrix into one compartment each
aggregate_k_matrix <- function(k_mat, setup) {
  k_filtered <- k_mat[setup$keep_idx, setup$keep_idx, drop = FALSE]
  X_mat <- fill_X_matrix(k_filtered, setup)
  K_final <- setup$A_mat %*% k_filtered %*% X_mat

  return(list(K_final = K_final, X = X_mat))
}

# Mass integrated from 0 to T after a 1 kg pulse: FF_T = K^-1 * (exp(K*T) - I) [kg·s per kg emitted]
# Rows = receiving compartment, columns = emission compartment
ff_time_horizons <- function(K_final, horizons_seconds) {
  K_inv <- solve(K_final)
  I <- diag(nrow(K_final))
  lapply(horizons_seconds, function(time_horizon_seconds) {
    FF_T <- K_inv %*% (expm::expm(K_final * time_horizon_seconds) - I)
    dimnames(FF_T) <- dimnames(K_final)
    FF_T
  })
}

# Steady state from a direct linear solve instead of SBoo's iterative ODE (rootSolve::runsteady, ~65% of the solve time)
# Only the k matrices of each run are used in this script; the masses are kept for checks
LinearSteadyStateSolver <- function(k, e, parms) {
  m <- rep(NA_real_, nrow(k))
  idx <- diag(k) != 0 #states without any removal process make k singular
  m[idx] <- tryCatch(as.vector(solve(k[idx, idx], -e[idx])), error = function(err) NA_real_)
  list(m, signal = e)
}

# Degradation rate constants of every filtered state, for every run [states x runs, s-1]
get_kdeg_states <- function(k_processes, states, filtered_names, n_runs) {
  deg <- k_processes |>
    as.data.frame() |>
    filter(process == "k_Degradation") |>
    inner_join(states |> dplyr::select(Abbr, Scale, SubCompart, Species),
               by = c("fromScale" = "Scale", "fromSubCompart" = "SubCompart", "fromSpecies" = "Species")) |>
    filter(Abbr %in% filtered_names) |>
    group_by(Abbr, RUNs) |>
    summarise(k = sum(k, na.rm = TRUE), .groups = "drop")

  kdeg <- matrix(0, nrow = length(filtered_names), ncol = n_runs, dimnames = list(filtered_names, NULL))
  kdeg[cbind(match(deg$Abbr, filtered_names), as.integer(deg$RUNs))] <- deg$k
  kdeg
}

### Helper function for elementary flow name --------------------------
get_elementary_flowname <- function(shape, pol, size, reg) {
  switch(shape,
         "Sphere" = paste0("Microsphere/fragment", " - ", pol, " (", size, " µm diameter), ", reg),
         "Fiber"  = paste0("Microfiber/cylinder", " - ", pol, " (", size, " µm diameter), ", reg),
         "Film"   = paste0("Microfilm", " - ", pol, " (", size, " µm thickness), ", reg),
         paste0("Micro", tolower(shape), " - ", pol, ", ", reg)
  )
}

# 1 Statistics functions --------------------------------------------------

# Log-normal statistics on the positive runs; zeros (e.g. no degradation without UV) are counted apart
# If all runs are 0, geom_mean = 0 and gsd = NA. Empirical percentiles use all runs.
stat_names <- c("geom_mean", "gsd", "ll_95", "ul_95", "median", "p2_5", "p97_5", "n_zero")
summarise_runs <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(setNames(rep(NA_real_, length(stat_names)), stat_names))
  pos <- x[x > 0]
  geom_mean <- if (length(pos) > 0) exp(mean(log(pos))) else 0
  gsd <- if (length(pos) > 1) exp(sd(log(pos))) else NA_real_
  c(geom_mean = geom_mean,
    gsd = gsd,
    ll_95 = geom_mean / gsd^1.96,
    ul_95 = geom_mean * gsd^1.96,
    median = median(x),
    p2_5 = unname(quantile(x, 0.025)),
    p97_5 = unname(quantile(x, 0.975)),
    n_zero = sum(x == 0))
}

# Summarise an array whose first dimension is the Monte Carlo run into a long data frame
summarise_array <- function(arr, prefix) {
  other_dims <- dimnames(arr)[-1]
  s <- apply(arr, seq_along(dim(arr))[-1], summarise_runs)
  s <- t(matrix(s, nrow = length(stat_names)))
  colnames(s) <- paste0(prefix, "_", stat_names)
  cbind(expand.grid(other_dims, stringsAsFactors = FALSE), s)
}

# 2 Process one region/polymer/size/shape combination ----------------------

process_combination <- function(k_matrix, kdeg_states, setup, ghg_factors, biogenic,
                                reg, pol, size, shape) {
  n_runs <- length(k_matrix)
  horizon_names <- names(time_horizons)

  frac_degraded <- array(NA_real_, dim = c(n_runs, length(X_list), length(emission_compartments), length(time_horizons)),
                         dimnames = list(run = as.character(seq_len(n_runs)), receiving = X_list, emission = emission_compartments, horizon = horizon_names))
  kdeg_eff <- matrix(NA_real_, nrow = n_runs, ncol = length(X_list), dimnames = list(run = as.character(seq_len(n_runs)), compartment = X_list))

  for (i in seq_len(n_runs)) {
    agg <- aggregate_k_matrix(k_matrix[[i]], setup)

    # Effective k_deg of each compartment: k_deg of free, aggregated and attached particles weighted by their mass fraction X
    kdeg_i <- as.vector(crossprod(agg$X, kdeg_states[rownames(agg$X), i]))
    kdeg_eff[i, ] <- kdeg_i

    # Fraction of the emitted mass degraded in each receiving compartment within T: FF_T * k_deg
    ff <- ff_time_horizons(agg$K_final, time_horizons * seconds_per_year)
    for (h in seq_along(time_horizons)) {
      frac_degraded[i, , , h] <- pmax(kdeg_i * ff[[h]][, emission_compartments], 0) #remove tiny negative values from numerical noise
    }
  }

  # Mass degraded per receiving compartment (cloudwater merged into air) and in total
  frac_reported <- frac_degraded[, receiving_compartments, , , drop = FALSE]
  frac_reported[, "aR", , ] <- frac_reported[, "aR", , ] + frac_degraded[, "cwR", , ]
  frac_reported[, "aC", , ] <- frac_reported[, "aC", , ] + frac_degraded[, "cwC", , ]
  frac_total <- apply(frac_degraded, c(1, 3, 4), sum)

  # kg CO2 and kg CH4 reaching the atmosphere per kg emitted [run x emission x horizon]
  CO2 <- apply(frac_degraded, c(1, 3, 4), function(f) sum(f * ghg_factors[, "CO2"]))
  CH4 <- apply(frac_degraded, c(1, 3, 4), function(f) sum(f * ghg_factors[, "CH4"]))

  # CFs [kg CO2-eq/kg emitted]
  CF_fossil <- sweep(CO2, 3, GWP_CO2[horizon_names], "*") + sweep(CH4, 3, GWP_CH4_fossil[horizon_names], "*")
  CF_biogenic <- if (biogenic) sweep(CH4, 3, GWP_CH4_nonfossil[horizon_names], "*") else CF_fossil

  meta <- data.frame(region = reg, polymer = pol, size = size, shape = shape)
  name_emission <- function(df) mutate(df, emission_compartment = unname(compartment_names[emission]), .before = 1) |> dplyr::select(-emission)

  # Mass degraded
  mass_degraded <- bind_rows(
    summarise_array(frac_reported, "deg") |>
      mutate(receiving = unname(compartment_names[receiving])),
    summarise_array(frac_total, "deg") |>
      mutate(receiving = "total")
  ) |>
    name_emission() |>
    rename(receiving_compartment = receiving)

  # GHG emitted
  ghg_emitted <- summarise_array(CO2, "CO2") |>
    left_join(summarise_array(CH4, "CH4"), by = c("emission", "horizon")) |>
    name_emission()

  # CFs
  cf <- summarise_array(CF_fossil, "CF_fossil") |>
    left_join(summarise_array(CF_biogenic, "CF_biogenic"), by = c("emission", "horizon")) |>
    name_emission() |>
    rename(indicator = horizon)

  # k_deg statistics
  kdeg_stats <- summarise_array(kdeg_eff, "kdeg") |>
    mutate(compartment = unname(compartment_names[compartment]))

  list(
    mass_degraded = cbind(meta, mass_degraded),
    ghg_emitted = cbind(meta, ghg_emitted),
    cf = cbind(elementary_flowname = get_elementary_flowname(shape, pol, size, reg), meta, cf),
    kdeg = cbind(meta, kdeg_stats)
  )
}

# 3 Run all sizes and shapes of one polymer -----------------------------------
# Runs in a forked process when n_cores > 1: each polymer works on its own copy of World

progress_log <- "../../results/GHG_progress.log"

run_polymer <- function(pol, reg) {
  setup <- NULL
  results <- list()

  #Import the properties of that polymer from the df
  polymer_df <- plastic_values[,c("varName","SubCompart",pol)]
  names(polymer_df)[ncol(polymer_df)] <- "Waarde" #rename the last column "Waarde" instead of name of pol
  polymer_df_selected <- polymer_df |>
    mutate(Waarde=as.numeric(Waarde))

  #Set the new values for RhoS, Kssdr, CorFacSSA and MinSettVel for the specific polymer
  World$mutateVars(polymer_df_selected)
  vars_to_update <- unique(polymer_df_selected$varName)

  World$SetConst(Degrading_enzyme = polymer_df$Waarde[polymer_df$varName == "Degrading_enzyme"])
  World$SetConst(DegApproach = polymer_df$Waarde[polymer_df$varName == "DegApproach"])

  #Import distribution on degradation data
  degradation_CI <- degradation_CI_all  %>%
    filter(pol == !!pol) #Keep only the rows where distribution is defined for pol

  varFuns <- World$makeInvFuns(degradation_CI)

  ghg_factors <- ghg_per_kg_degraded(pol)
  biogenic <- pol %in% biogenic_polymers

  #loop over size
  for (size in sizes) {
    size_df <- data.frame(varName = "RadS",
                          Waarde = size/2*1000) #convert D to R and convert um to nm (SBoo takes RadS as nm)

    World$mutateVars(size_df)
    vars_to_update <- c(vars_to_update,unique(size_df$varName))

    #Loop over shape
    for (shape in shapes) {
      t_start <- Sys.time()

      #Define the dimensions of the microplastics emissions (not just diameter or thickness) - SB input is um for sides
      if (shape == "Sphere" | shape == "Default") {
        Longest_side <- size #Here size = diameter
        Intermediate_side <- size
        Shortest_side <- size
      } else if (shape == "Ellipsoid") {
        Longest_side <- size
        Intermediate_side <- size
        Shortest_side <- size
      } else if (shape == "Cube" | shape == "Box" | shape == "Film") {
        Shortest_side <- size #Thickness
        Intermediate_side <- Shortest_side * 30 #Kooi & Koelmans (2019): SI
        if (Shortest_side < 100 ) {
          Longest_side <- Shortest_side*100
        } else {
          Longest_side <- Shortest_side*30
        }
      } else if (shape == "Cylindric - circular" | shape == "Fiber") {
        Shortest_side <- size
        Intermediate_side <- Shortest_side #equal for fibers
        if (Shortest_side == 1 ) {
          Longest_side <- Shortest_side*50
        } else if (Shortest_side == 1000 || Shortest_side == 5000) { #microfiber range is defined with respect to the diameter <5000, not the length
          Longest_side <- Shortest_side*5
        } else{
          Longest_side <- Shortest_side*10
        }
      } else if (shape == "Cylindric - elliptic") {
        Longest_side <- size
        Intermediate_side <- size
        Shortest_side <- size
      } else {
        stop("Invalid shape! Please choose from Sphere, Ellipsoid, Cube, Box, Cylindric - circular, or Cylindric - elliptic.")
      }

      if (Shortest_side > 5000 || Intermediate_side > 5000 ) {
        next  # Skip if one dimension exceeds 5000, except for microfibers, which can have length >5000um
        #(Scientific Coalition for an Effective Plastic Treaty). Therefore, Longest_side is not part of the check.
      } #Microfiber

      shape_df <- data.frame(varName = "Shape", Waarde = as.character(shape))
      dimensions_df <- data.frame(
        varName = c("Shortest_side", "Intermediate_side", "Longest_side"),
        Waarde = c(Shortest_side, Intermediate_side, Longest_side)
      )
      World$mutateVars(shape_df)
      World$mutateVars(dimensions_df)
      vars_to_update <- c(vars_to_update,unique(shape_df$varName),unique(dimensions_df$varName))

      filtered_trackmpd_data <- trackmpd %>%
        filter(region == reg,
               polymer == pol,
               size == !!size,
               shape == !!shape)
      World$mutateVars(filtered_trackmpd_data)
      vars_to_update <- c(vars_to_update,unique(filtered_trackmpd_data$varName))

      filtered_trackmpd_data_glo <- trackmpd %>%
        filter(region == 'Global',
               polymer == pol,
               size == !!size,
               shape == !!shape)
      World$mutateVars(filtered_trackmpd_data_glo)
      vars_to_update <- c(vars_to_update,unique(filtered_trackmpd_data_glo$varName))

      #UpdateDirty with the names of all variables that were changed, in all the loops
      World$UpdateDirty(unique(vars_to_update))

      #solve: the solver is only used to build the k matrices and process k's of every run
      emissions <- data.frame(Abbr = c("aRS"), Emis = 1/3600/24, RUN = (1:n_samples)) #emission of 1kg/d input in kg/s
      set.seed(seed)
      World$NewSolver("LinearSteadyStateSolver")
      World$Solve(emissions = emissions, var_box_df = degradation_CI, var_invFun = varFuns, nRUNs = n_samples)

      k_matrix <- World$K_matrix() #list of k matrices, one per run
      k_processes <- World$K_process_df() #process k's of every run, used to retrieve k_deg

      # Setup on first iteration (only structure)
      if(is.null(setup)) {
        setup <- setup_fill_patterns(k_matrix[[1]], X_list)
      }

      kdeg_states <- get_kdeg_states(k_processes, states, setup$filtered_names, length(k_matrix))

      key <- paste(reg, pol, size, shape, sep = "|")
      results[[key]] <- process_combination(k_matrix, kdeg_states, setup, ghg_factors, biogenic,
                                            reg, pol, size, shape)

      cat(sprintf("%s  %s  %.1f s\n", format(Sys.time(), "%H:%M:%S"), key,
                  as.numeric(difftime(Sys.time(), t_start, units = "secs"))),
          file = progress_log, append = TRUE)

      vars_to_update = c() #reset to avoid recomputing vars that have not changed and making vars_to_update excessively long
    }
  }
  results
}

# 4 Run all regions ------------------------------------------------------------

# Get the names and abbreviations of all compartments
states <- World$states$asDataFrame

cat("", file = progress_log)
all_results <- list()
t_total <- Sys.time()

for(reg in region_names){
  print(reg)
  region_df <- regions[, c("varName", "Scale", "SubCompart", reg)]
  names(region_df)[ncol(region_df)] <- "Waarde"

  region_df <- region_df |>
    mutate(Waarde = as.numeric(Waarde))

  World$mutateVars(region_df)
  World$UpdateDirty(unique(region_df$varName))

  #remove flows to moderate, arctic, tropic
  airflow_df <- data.frame(varName = c("AirFlow", "AirFlow", "AirFlow"),
                           Scale = c("Arctic", "Moderate", "Tropic"),
                           Waarde = c(10^-20,  10^-20, 10^-20))
  World$mutateVars(airflow_df)
  World$UpdateDirty(unique(airflow_df$varName))

  # Polymer loop (in parallel)
  region_results <- parallel::mclapply(polymer_names, run_polymer, reg = reg,
                                       mc.cores = n_cores, mc.preschedule = FALSE)

  failed <- vapply(region_results, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop("Failed for ", reg, ": ", paste(polymer_names[failed], collapse = ", "), "\n",
         paste(unlist(region_results[failed]), collapse = "\n"))
  }
  all_results <- c(all_results, unlist(region_results, recursive = FALSE))
  print(Sys.time() - t_total)
}

# Combine the results of all combinations
combine_results <- function(name) bind_rows(lapply(all_results, `[[`, name))
results_mass_degraded <- combine_results("mass_degraded")
results_ghg_emitted <- combine_results("ghg_emitted")
results_CF <- combine_results("cf")
results_kdeg <- combine_results("kdeg")

# 5 Export ---------------------------------------------------------------------

stat_descriptions <- c(
  "geom_mean" = "Geometric mean of the runs > 0 (0 if all runs are 0)",
  "gsd" = "Geometric standard deviation of the runs > 0 (NA if fewer than 2 runs > 0)",
  "ll_95" = "Lower limit of the 95% interval of the log-normal distribution: geom_mean / gsd^1.96",
  "ul_95" = "Upper limit of the 95% interval of the log-normal distribution: geom_mean * gsd^1.96",
  "median" = "Median of all runs",
  "p2_5" = "2.5th percentile of all runs",
  "p97_5" = "97.5th percentile of all runs",
  "n_zero" = "Number of runs equal to 0"
)

readme <- data.frame(
  Item = c("Description",
           "Monte Carlo runs",
           "Mass degraded",
           "k_deg",
           "CO2 and CH4 emitted",
           "CF_fossil",
           "CF_biogenic",
           "Bio-based polymers",
           "Time horizons",
           "GWP CO2",
           "GWP CH4 fossil",
           "GWP CH4 non-fossil",
           "Cloudwater",
           "TRWP",
           paste0("Statistic: ", names(stat_descriptions))),
  Value = c("Mass of microplastics degraded in each compartment, and CO2/CH4 released to the atmosphere, for 1 kg emitted (pulse)",
            paste0(n_samples, " runs per region/polymer/size/shape (LHS on the degradation and Kssdr parameters, seed ", seed, ")"),
            "Fraction of the emitted mass degraded within T in each receiving compartment = FF_T * k_deg, with FF_T = K^-1 (exp(K*T) - I) [kg degraded/kg emitted]",
            "Effective degradation rate constant of each compartment: k_deg of free, aggregated and attached particles weighted by their mass fraction [s-1]",
            "Sum over receiving compartments of mass degraded * kg gas produced per kg degraded * f_atm [kg gas/kg emitted]. Oxic compartments: full oxidation to CO2. Anoxic compartments: CO2 + CH4 (Boyle); the oxidised CH4 becomes CO2 and follows f_atm,CO2",
            "CO2 * GWP_CO2 + CH4 * GWP_CH4_fossil [kg CO2-eq/kg emitted]",
            "Bio-based polymers: CH4 * GWP_CH4_non-fossil (biogenic CO2 counted as 0). Fossil polymers: equal to CF_fossil [kg CO2-eq/kg emitted]",
            paste(biogenic_polymers, collapse = ", "),
            paste0(names(time_horizons), ": mass degraded within ", time_horizons, " years", collapse = "; "),
            paste0(names(GWP_CO2), " = ", GWP_CO2, collapse = "; "),
            paste0(names(GWP_CH4_fossil), " = ", GWP_CH4_fossil, collapse = "; "),
            paste0(names(GWP_CH4_nonfossil), " = ", GWP_CH4_nonfossil, collapse = "; "),
            "Mass degraded in cloudwater is added to air",
            "No stoichiometry in M_deg: GHG results and CFs are NA",
            unname(stat_descriptions))
)

ghg_factors_table <- bind_rows(lapply(polymer_names, function(pol) {
  ghg <- ghg_per_kg_degraded(pol)
  data.frame(polymer = pol, compartment = compartment_names[X_list],
             kg_CO2_to_atm_per_kg_degraded = ghg[, "CO2"], kg_CH4_to_atm_per_kg_degraded = ghg[, "CH4"])
}))

out_file <- "../../results/results_GHG_CI.xlsx"
wb <- createWorkbook()
add_sheet <- function(wb, sheet_name, data) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data, withFilter = TRUE)
  freezePane(wb, sheet_name, firstRow = TRUE)
}
add_sheet(wb, "README", readme)
add_sheet(wb, "f_atm_mapping", f_atm_mapped)
add_sheet(wb, "GHG_per_kg_degraded", ghg_factors_table)
for (h in names(time_horizons)) {
  add_sheet(wb, paste0("Mass_degraded_", time_horizons[h], "yr"),
            results_mass_degraded |> filter(horizon == h) |> dplyr::select(-horizon))
}
add_sheet(wb, "GHG_emitted", results_ghg_emitted |> rename(time_horizon = horizon))
add_sheet(wb, "CF_GWP", results_CF)
add_sheet(wb, "k_deg", results_kdeg)
saveWorkbook(wb, out_file, overwrite = TRUE)
