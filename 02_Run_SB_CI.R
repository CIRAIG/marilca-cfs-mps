# Set up SimpleBox----------------------------------------------------------

## Load libraries ----------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(MASS)
library(Rmpfr)
library(expm)

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

vars_to_update = c()

## Set up FF variables --------------------------------------------------

region_names <- colnames(regions)[4:11]
region_names <- region_names[!is.na(region_names)]
polymer_names <- colnames(plastic_values)[3:ncol(plastic_values)]

#### emission compartments, volumes, SDF, etc needed for CF calculation
sizes <- c(1,10,100,1000,5000) #D will be divided by 2
shapes <- c("Sphere","Fiber","Film")
time_horizon_1=20 #[yrs] time horizon for dynamic solver (Time over which impacts are integrated for )
time_horizon_2=100 #[yrs] time horizon for dynamic solver (Time over which impacts are integrated for )
seconds_per_year <- 365.25 * 24 * 3600
time_horizon_seconds_1=time_horizon_1*seconds_per_year
time_horizon_seconds_2=time_horizon_2*seconds_per_year
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

receiving_compartments <- c("aRS","w1RS","w0RS","w2RS", "w3RS","sd1RS","sd0RS","sd2RS","s1RS","s2RS",
                            "aCS","w1CS","w0CS","w2CS", "w3CS","sd1CS","sd0CS","sd2CS","s1CS","s2CS") 

### fraction of species in each compartment
FracSpe_w_marine = 0.4458 #fraction of marine species feeding in water - Saadi et al., 2025
FracSpe_sed_marine = 1-FracSpe_w_marine #fraction of marine species feeding in the sediment - Saadi et al., 2025
FracSpe_ws_marine = FracSpe_w_marine*0.662 #fraction of marine species feeding in the water surface - Hajjar et al., 2025
FracSpe_wc_marine = FracSpe_w_marine*(1-0.662) #fraction of marine species feeding in the water column - Hajjar et al., 2025
FracSpe_river = 0.7385 #fraction of aquatic species living in rivers - IUCN Red List, 2024
FracSpe_lake = 1-FracSpe_river #fraction of aquatic species living in lakes - IUCN Red List, 2024
FracSpe_wc_aqua = 0.2989 #fraction of aquatic species living in water column - Schmidt-Kloiber et al., 2024
FracSpe_sed_aqua = 0.7011 #fraction of aquatic species living in sediments - Schmidt-Kloiber et al., 2024
FracSpe_nat_soil = 0.7031 #BDM, 2015
FracSpe_agr_soil = 0.2969 #BDM, 2015

#ReCiPe number of species:
tot_num_species_marine = 250000
tot_num_species_fresh = 150000
tot_num_species_terr = 1600000
list_tot_species = c(tot_num_species_marine, tot_num_species_fresh, tot_num_species_terr)

## Run settings ----------------------------------------------------------
n_samples <- 100 #Monte Carlo runs per combination
# n_cores > 1: polymers of a region are run in parallel (forked processes, macOS/Linux only).
#              Each polymer gets its own seed, so results are reproducible but Monte Carlo samples differ from the serial run
# n_cores = 1: serial, same random number stream as previous versions of this script (reproduces the published results)
n_cores <- if (.Platform$OS.type == "windows") 1 else max(1, parallel::detectCores() - 1) #polymers are run in parallel (forking is not available on Windows)
parallel_seed <- 123
# For quick tests, subset the loops, e.g.:
# region_names <- "Europe"; polymer_names <- c("EPS", "TRWP"); sizes <- c(1, 100); shapes <- "Sphere"; n_samples <- 5
results_suffix <- "" #e.g. "_test" to avoid overwriting the results files during tests

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
X_list <- c("aR", "cwR","w1R","w0R","w2R", "w3R","sd1R","sd0R","sd2R","s1R","s2R",    #careful, this list contains cw (to for the X matrix)
            "aC", "cwC","w1C","w0C","w2C", "w3C","sd1C","sd0C","sd2C","s1C","s2C") 

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
  
  # Indices of the X cells to fill (only prefixes for which free, agg and att states all exist)
  has_all <- sapply(X_list, function(prefix) all(paste0(prefix, c("S", "A", "P")) %in% filtered_names))
  X_idx <- data.frame(
    col  = which(has_all),
    free = match(paste0(X_list[has_all], "S"), filtered_names),
    agg  = match(paste0(X_list[has_all], "A"), filtered_names),
    att  = match(paste0(X_list[has_all], "P"), filtered_names)
  )

  # Return what we CAN pre-compute
  return(list(
    keep_idx = keep_idx,
    filtered_names = filtered_names,
    A_mat = A_mat,           # Always the same pattern
    X_list = X_list,
    X_idx = X_idx
  ))
}

# Aggregated K matrix (free + aggregated + attached species lumped per compartment) for one k_matrix
compute_K_final <- function(k_mat, setup) {
  # Filter matrix
  k_filtered <- k_mat[setup$keep_idx, setup$keep_idx, drop = FALSE]
  stopifnot(identical(rownames(k_filtered), setup$filtered_names))

  # Create X matrix (dynamic - uses actual k values), filled with matrix indexing instead of a loop over names
  ix <- setup$X_idx
  k_free_agg <- abs(k_filtered[cbind(ix$agg, ix$free)])
  k_free_att <- abs(k_filtered[cbind(ix$att, ix$free)])
  k_agg_tot  <- abs(k_filtered[cbind(ix$agg, ix$agg)])
  k_att_tot  <- abs(k_filtered[cbind(ix$att, ix$att)])

  X_mat <- matrix(0, nrow = nrow(k_filtered), ncol = length(setup$X_list),
                  dimnames = list(rownames(k_filtered), setup$X_list))
  X_mat[cbind(ix$free, ix$col)] <- Xfree(k_free_agg, k_free_att, k_agg_tot, k_att_tot)
  X_mat[cbind(ix$agg, ix$col)]  <- Xagg(k_free_agg, k_free_att, k_agg_tot, k_att_tot)
  X_mat[cbind(ix$att, ix$col)]  <- Xatt(k_free_agg, k_free_att, k_agg_tot, k_att_tot)

  # Compute K1X and K_final
  K1X <- k_filtered %*% X_mat
  setup$A_mat %*% K1X
}

# "Solver" for World$Solve that only builds the k matrices of the Monte Carlo runs (World$K_matrix())
# The SBoo steady-state masses are not used here (FFs are computed from the k matrices below),
# so skipping rootSolve::runsteady makes World$Solve ~3x faster. The k matrices are identical.
KMatrixOnlySolver <- function(k, e, parms) {
  list(rep(NA_real_, nrow(k)), e)
}

# Steady-state FFs and time-horizon FFs for one k_matrix [days]
# K_final and its inverse are computed once and shared by all horizons
compute_ff_matrices <- function(k_mat, setup, time_horizons_seconds) {
  K_final <- compute_K_final(k_mat, setup)
  K_inv <- solve(K_final)

  # Steady state: FF = -K^-1
  ff_ss <- (-1 / 86400) * K_inv #seconds to day
  rownames(ff_ss) <- paste0(rownames(ff_ss), "S")

  # Time horizon: FF = K^-1 * (exp(K*T) - I)
  I_mat <- diag(nrow(K_final))
  ff_T <- lapply(time_horizons_seconds, function(time_horizon_seconds) {
    exp_KT <- expm(K_final * time_horizon_seconds) #k matrix is in seconds
    FF_T_days <- K_inv %*% (exp_KT - I_mat) / 86400 #seconds to day (FFs are stored as days)
    rownames(FF_T_days) <- paste0(rownames(K_inv), "S")
    FF_T_days
  })

  c(list(ss = ff_ss), ff_T)
}

# 1 Code for CFs uncertainty -----------------------------------------------

## Define functions ------------------------------------------------------

SF <- 1 #Oginah et al. (2025)

### 1.1 Function to generate EEF Monte Carlo samples ---------------------
eef_params <- list(
  EEF_w = list(meanlog = 6.973081061, sdlog = 0.557267175),
  EEF_sed = list(meanlog = 2.783343185, sdlog = 1.146282228),
  EEF_s = list(meanlog = -1.709258248, sdlog = 0.197720485)
)

# TRWP-specific EEFs (physical effects) in water and soil [PAF m3/kg], 95% CI (z0.975=1.96)
# Ospital et al. (2026), https://doi.org/10.2139/ssrn.7565081
# Sediment EEF is kept as for the other polymers
#   EEF_water_trwp_physical = 18.521 [1.688 - 203.244]
#   EEF_soil_trwp_physical  = 0.197 [0.038 - 2.141] (CI not symmetric in log space: sdlog derived from the CI width)
eef_params_trwp <- eef_params
eef_params_trwp$EEF_w <- list(meanlog = log(18.521), sdlog = log(203.244 / 1.688) / (2 * 1.96))
eef_params_trwp$EEF_s <- list(meanlog = log(0.197),  sdlog = log(2.141 / 0.038) / (2 * 1.96))

# Same standard normal draws mapped to other lognormal parameters (no extra random numbers are drawn,
# so the random number stream used by SBoo afterwards is unchanged)
rescale_eef_samples <- function(eef_samples, from_params, to_params) {
  for (eef in names(to_params)) {
    z <- (log(eef_samples[[eef]]) - from_params[[eef]]$meanlog) / from_params[[eef]]$sdlog
    eef_samples[[eef]] <- exp(to_params[[eef]]$meanlog + to_params[[eef]]$sdlog * z)
  }
  eef_samples
}

generate_eef_samples <- function(n_samples = 1000) {

  set.seed(123) #set a seed so that we get the same random numbers - if we run the code again, the differences won't be due to a new distribution of EEF
  eef_samples <- data.frame(
    EEF_w = rlnorm(n_samples, eef_params$EEF_w$meanlog, eef_params$EEF_w$sdlog),
    EEF_sed = rlnorm(n_samples, eef_params$EEF_sed$meanlog, eef_params$EEF_sed$sdlog),
    EEF_s = rlnorm(n_samples, eef_params$EEF_s$meanlog, eef_params$EEF_s$sdlog),
    EEF_air = rep(0, n_samples)  # Fixed at 0
  )
  
  return(eef_samples)
}

#call the function with n_samples
eef_monte_carlo <- generate_eef_samples(n_samples)
eef_monte_carlo_trwp <- rescale_eef_samples(eef_monte_carlo, eef_params, eef_params_trwp)

### 1.2 Helper function for elementary flow name --------------------------
get_elementary_flowname <- function(shape, pol, size, reg) {
  switch(shape,
         "Sphere" = paste0("Microsphere/fragment", " - ", pol, " (", size, " µm diameter), ", reg),
         "Fiber"  = paste0("Microfiber/cylinder", " - ", pol, " (", size, " µm diameter), ", reg),
         "Film"   = paste0("Microfilm", " - ", pol, " (", size, " µm thickness), ", reg),
         paste0("Micro", tolower(shape), " - ", pol, ", ", reg)
  )
}

### 1.3 Mapping of SBoo states to the receiving compartments --------------
# All the steps from FFs to CFs are linear, so all Monte Carlo samples and emission compartments
# are processed at once with matrix products (instead of one dplyr pipeline per sample and emission compartment)
receiving_subcomparts <- c("air","river","lake","sea","deepocean",
                           "freshwatersediment","lakesediment",
                           "marinesediment","naturalsoil","agriculturalsoil")

build_receiving_map <- function(ff_rownames, states) {
  st <- states[match(ff_rownames, states$Abbr), ]
  st_scale <- as.character(st$Scale)
  st_sub <- as.character(st$SubCompart)
  # air and cloudwater are merged; global SBoo scales and othersoil are dropped
  st_sub <- ifelse(st_scale %in% c("Regional", "Continental") & st_sub %in% c("air", "cloudwater"), "air", st_sub)

  # Receiving compartments in the order Regional (=continental LCIA), Continental (=global LCIA) x receiving_subcomparts
  # This is the order of receiving_compartments, volume_compartments, areas_compartments and of the SDF columns
  rec_scale <- rep(c("Regional", "Continental"), each = length(receiving_subcomparts))
  rec_sub <- rep(receiving_subcomparts, times = 2)

  j <- which(st_scale %in% c("Regional", "Continental") & st_sub != "othersoil")
  i <- match(paste(st_scale[j], st_sub[j]), paste(rec_scale, rec_sub))
  stopifnot(!anyNA(i))

  # G sums the FFs of the SBoo states into the receiving compartments: FF_rec = G %*% FF
  G <- matrix(0, nrow = length(rec_sub), ncol = length(ff_rownames))
  G[cbind(i, j)] <- 1
  stopifnot(all(rowSums(G) > 0))

  list(
    G = G,
    Scale = factor(ifelse(rec_scale == "Regional", "Continental", "Global"), levels = c("Continental", "Global")),
    SubCompart = factor(rec_sub, levels = receiving_subcomparts),
    # EEF applied to each receiving compartment (in agricultural soil eef=0)
    eef_type = case_when(
      rec_sub == "air" ~ "EEF_air",
      rec_sub %in% c("river", "lake", "sea", "deepocean") ~ "EEF_w",
      rec_sub %in% c("freshwatersediment", "lakesediment", "marinesediment") ~ "EEF_sed",
      rec_sub == "naturalsoil" ~ "EEF_s",
      TRUE ~ "none"
    ),
    # Fraction of species in each receiving compartment, used for PDF·m²·year
    frac_species_m2 = case_when(
      rec_sub %in% c("river", "lake") ~ FracSpe_wc_aqua,
      rec_sub %in% c("freshwatersediment", "lakesediment") ~ FracSpe_sed_aqua,
      rec_sub == "sea" ~ FracSpe_ws_marine,
      rec_sub == "deepocean" ~ FracSpe_wc_marine,
      rec_sub == "marinesediment" ~ FracSpe_sed_marine,
      TRUE ~ 1
    ),
    # Ecosystem of each receiving compartment, used for PDF·m²·year (air is not included)
    ecosystem_m2 = rbind(
      Marine_Ecosystem = as.numeric(rec_sub %in% c("sea", "deepocean", "marinesediment")),
      Freshwater_Ecosystem = as.numeric(rec_sub %in% c("river", "freshwatersediment", "lake", "lakesediment")),
      Terrestrial_Ecosystem = as.numeric(rec_sub %in% c("naturalsoil", "agriculturalsoil"))
    )
  )
}

# 2  Process Monte Carlo -------------------------------------------------------

process_monte_carlo_combination <- function(ff_matrices_list, eef_samples, reg, pol, size, shape,
                                            emission_compartments, rec_map, compartment_names,
                                            volume_compartments, areas_compartments,
                                            SDF, SDF_GEP, SF, list_tot_species) {

  # Determine sample size
  n_samples <- min(length(ff_matrices_list), nrow(eef_samples))
  n_emis <- length(emission_compartments)
  n_rec <- nrow(rec_map$G)

  # A. FFs in the receiving compartments: n_rec x (n_emis * n_samples), columns ordered by sample, then emission compartment
  FF <- do.call(cbind, lapply(ff_matrices_list[1:n_samples], function(ff_mat) {
    rec_map$G %*% ff_mat[, emission_compartments, drop = FALSE]
  }))
  emis_idx <- rep(seq_len(n_emis), times = n_samples)
  mc_iteration <- rep(seq_len(n_samples), each = n_emis)
  emission_compartment_names <- unname(compartment_names[emission_compartments])

  # B. Combine with the EEF of each sample
  eef_by_type <- rbind(
    EEF_air = eef_samples$EEF_air[1:n_samples],
    EEF_w = eef_samples$EEF_w[1:n_samples],
    EEF_sed = eef_samples$EEF_sed[1:n_samples],
    EEF_s = eef_samples$EEF_s[1:n_samples],
    none = 0
  )
  eef <- eef_by_type[rec_map$eef_type, mc_iteration, drop = FALSE]
  CF_comp_PAF_m3_day <- FF * eef
  CF_comp_PAF_day <- CF_comp_PAF_m3_day / volume_compartments

  # C. CFs per ecosystem (rows: marine, freshwater, terrestrial)
  # 1. Midpoint CFs (PAF_loc·day)
  cf_mid <- as.matrix(SDF) %*% CF_comp_PAF_day
  # 2. Endpoint CFs (PDF·year): Compute PAF using the SDF_GEP instead of SDF for GLAM
  cf_end_pdf <- as.matrix(SDF_GEP) %*% CF_comp_PAF_day * SF / 365
  # 3. Endpoint CFs (species·year)
  cf_species <- (as.matrix(SDF) * list_tot_species) %*% CF_comp_PAF_day * SF / 365
  # 4. PDF·m²·year
  CF_comp_PDF_m2_year <- CF_comp_PAF_day * areas_compartments * SF / 365 * rec_map$frac_species_m2
  cf_pdf_m2 <- rec_map$ecosystem_m2 %*% CF_comp_PDF_m2_year

  # D. Long data frames for the statistics
  meta <- data.frame(
    elementary_flowname = get_elementary_flowname(shape, pol, size, reg),
    region = reg,
    polymer = pol,
    size = size,
    shape = shape,
    emission_compartment = emission_compartment_names[emis_idx],
    mc_iteration = mc_iteration,
    stringsAsFactors = FALSE
  )
  cf_long <- function(cf_mat, cf_prefix) {
    df <- data.frame(cf_mat[1, ], cf_mat[2, ], cf_mat[3, ], colSums(cf_mat))
    colnames(df) <- paste0(cf_prefix, c("_marine", "_freshwater", "_terrestrial", "_total"))
    cbind(meta, df)
  }

  all_ff_combined <- data.frame(
    region = reg,
    polymer = pol,
    size = size,
    shape = shape,
    emission_compartment = rep(emission_compartment_names[emis_idx], each = n_rec),
    Scale = rep(rec_map$Scale, times = ncol(FF)),
    SubCompart = rep(rec_map$SubCompart, times = ncol(FF)),
    FF = as.vector(FF),
    stringsAsFactors = FALSE
  )

  # Return all statistics
  return(list(
    ff_stats = calculate_ff_statistics(all_ff_combined),
    cf_mid_stats = calculate_cf_statistics(cf_long(cf_mid, "CF_mid_PAF_day"), "CF_mid_PAF_day"),
    cf_end_pdf_stats = calculate_cf_statistics(cf_long(cf_end_pdf, "CF_end_PDF_year"), "CF_end_PDF_year"),
    cf_end_species_stats = calculate_cf_statistics(cf_long(cf_species, "CF_end_species_year"), "CF_end_species_year"),
    cf_end_pdf_m2_stats = calculate_cf_statistics(cf_long(cf_pdf_m2, "CF_end_PDF_m2_year"), "CF_end_PDF_m2_year")
  ))
}

# 3 Statistics functions --------------------------------------------------

## 3.1 Function to calculate FF statistics ---------------------------------
calculate_ff_statistics <- function(ff_data) {
  ff_stats <- ff_data |>
    group_by(region, polymer, size, shape, emission_compartment, Scale, SubCompart) |>
    summarise(
      #ff_mean = mean(FF, na.rm = TRUE),
      ff_geom_mean = exp(mean(log(FF), na.rm = TRUE)),
      #ff_median = median(FF, na.rm = TRUE),
      #ff_sd = sd(FF, na.rm = TRUE),
      ff_gsd = exp(sd(log(FF), na.rm = TRUE)),
      #ff_cv = ff_sd / ff_mean,
      #ff_ll_95 = quantile(FF, 0.025, na.rm = TRUE),
      #ff_ul_95 = quantile(FF, 0.975, na.rm = TRUE),
      ff_ll_95 = ff_geom_mean / ff_gsd^1.96, #Computed ul and ll instead of retrieved from the list
      ff_ul_95 = ff_geom_mean * ff_gsd^1.96,
      #ff_ll_90 = quantile(FF, 0.05, na.rm = TRUE),
      #ff_ul_90 = quantile(FF, 0.95, na.rm = TRUE),
      n_samples = n(),
      .groups = "drop"
    ) |>
    mutate(SubCompart = recode(SubCompart, "lake" = "lakewater", "river" = "riverwater", "sea"="seawater surface", "deepocean"="seawater column", "marinesediment"="marine sediments", "lakesediment" = "lake sediments", "freshwatersediment" = "freshwater sediments", "agriculturalsoil" = "agricultural soil", "naturalsoil" = "natural soil"))|>
    mutate(Scale = recode(Scale, "Continental" = "continental", "Global" = "global"))|>
    mutate(receiving_compartment = paste(Scale, SubCompart))|>
    #dplyr::select(region, polymer, size, shape, emission_compartment, receiving_compartment,
    #              ff_mean, ff_geom_mean, ff_median, ff_sd, ff_gsd, ff_cv,
    #              ff_ll_95, ff_ul_95, ff_ll_90, ff_ul_90, n_samples)
    dplyr::select(region, polymer, size, shape, emission_compartment, receiving_compartment,
                  ff_geom_mean, ff_gsd, ff_ll_95, ff_ul_95, n_samples)
  
  return(ff_stats)
}

## 3.2 Function to calculate CF statistics --------------------------------
calculate_cf_statistics <- function(cf_data, cf_prefix) {
  # Extract ecosystem columns
  marine_col <- paste0(cf_prefix, "_marine")
  freshwater_col <- paste0(cf_prefix, "_freshwater")
  terrestrial_col <- paste0(cf_prefix, "_terrestrial")
  total_col <- paste0(cf_prefix, "_total")
  
  # Group by emission compartment and calculate statistics for EACH one
  stats <- cf_data %>%
    group_by(elementary_flowname, region, polymer, size, shape, emission_compartment) %>%
    summarise(
      # Marine statistics
      #marine_mean = mean(.data[[marine_col]], na.rm = TRUE),
      marine_geom_mean = exp(mean(log(.data[[marine_col]]), na.rm = TRUE)),
      #marine_median = median(.data[[marine_col]], na.rm = TRUE),
      #marine_sd = sd(.data[[marine_col]], na.rm = TRUE),
      marine_gsd = exp(sd(log(.data[[marine_col]]), na.rm = TRUE)),
      #marine_cv = marine_sd / marine_mean,
      #marine_ll_95 = quantile(.data[[marine_col]], 0.025, na.rm = TRUE),
      #marine_ul_95 = quantile(.data[[marine_col]], 0.975, na.rm = TRUE),
      marine_ll_95 = marine_geom_mean / marine_gsd^1.96,
      marine_ul_95 = marine_geom_mean * marine_gsd^1.96,
      
      # Shapiro test
      
      # marine_shapiro_p = {
      #   # Get the data
      #   x <- .data[[marine_col]]
      #   
      #   # Check for valid data
      #   if(length(x) < 3 || any(is.na(x)) || any(is.infinite(x)) || 
      #      sd(x, na.rm = TRUE) == 0 || all(x <= 0)) {
      #     NA_real_
      #   } else if(length(x) > 5000) {
      #     # Too large for Shapiro-Wilk, sample it
      #     sample_x <- sample(x, size = 1000, replace = FALSE)
      #     if(sd(sample_x, na.rm = TRUE) == 0 || all(sample_x <= 0)) {
      #       NA_real_
      #     } else {
      #       shapiro.test(log(sample_x))$p.value
      #     }
      #   } else {
      #     # Regular test
      #     shapiro.test(log(x))$p.value
      #   }
      # },
      # marine_is_lognormal = marine_shapiro_p > 0.05 & !is.na(marine_shapiro_p),
      # 
      # Freshwater statistics
      #freshwater_mean = mean(.data[[freshwater_col]], na.rm = TRUE),
      freshwater_geom_mean = exp(mean(log(.data[[freshwater_col]]), na.rm = TRUE)),
      #freshwater_median = median(.data[[freshwater_col]], na.rm = TRUE),
      #freshwater_sd = sd(.data[[freshwater_col]], na.rm = TRUE),
      freshwater_gsd = exp(sd(log(.data[[freshwater_col]]), na.rm = TRUE)),
      #freshwater_cv = freshwater_sd / freshwater_mean,
      #freshwater_ll_95 = quantile(.data[[freshwater_col]], 0.025, na.rm = TRUE),
      #freshwater_ul_95 = quantile(.data[[freshwater_col]], 0.975, na.rm = TRUE),
      freshwater_ll_95 = freshwater_geom_mean / freshwater_gsd^1.96,
      freshwater_ul_95 = freshwater_geom_mean * freshwater_gsd^1.96,
      
      # Terrestrial statistics
      #terrestrial_mean = mean(.data[[terrestrial_col]], na.rm = TRUE),
      terrestrial_geom_mean = exp(mean(log(.data[[terrestrial_col]]), na.rm = TRUE)),
      #terrestrial_median = median(.data[[terrestrial_col]], na.rm = TRUE),
      #terrestrial_sd = sd(.data[[terrestrial_col]], na.rm = TRUE),
      terrestrial_gsd = exp(sd(log(.data[[terrestrial_col]]), na.rm = TRUE)),
      #terrestrial_cv = terrestrial_sd / terrestrial_mean,
      #terrestrial_ll_95 = quantile(.data[[terrestrial_col]], 0.025, na.rm = TRUE),
      #terrestrial_ul_95 = quantile(.data[[terrestrial_col]], 0.975, na.rm = TRUE),
      terrestrial_ll_95 = terrestrial_geom_mean / terrestrial_gsd^1.96,
      terrestrial_ul_95 = terrestrial_geom_mean * terrestrial_gsd^1.96,
      
      # Total statistics
      #total_mean = mean(.data[[total_col]], na.rm = TRUE),
      total_geom_mean = exp(mean(log(.data[[total_col]]), na.rm = TRUE)),
      #total_median = median(.data[[total_col]], na.rm = TRUE),
      #total_sd = sd(.data[[total_col]], na.rm = TRUE),
      total_gsd = exp(sd(log(.data[[total_col]]), na.rm = TRUE)),
      #total_cv = total_sd / total_mean,
      #total_ll_95 = quantile(.data[[total_col]], 0.025, na.rm = TRUE),
      #total_ul_95 = quantile(.data[[total_col]], 0.975, na.rm = TRUE),
      total_ll_95 = total_geom_mean / total_gsd^1.96,
      total_ul_95 = total_geom_mean * total_gsd^1.96,
      
      n_samples = n(),
      
      .groups = "drop"
    )
  
  return(stats)
}

# Get the names and abbreviations of all compartments
states <- World$states$asDataFrame

# Results of each combination (combined into the results dataframes after the loops)
all_results <- list()
setup <- NULL
rec_map <- NULL
start_time <- Sys.time()

# Variables to test -------------------------------------------------------
reg = "North America"
pol = "EPS"
size = 1
shape = "Sphere"
emission_compartment = "s1RS"

# Calculation of fate factors ---------------------------------------------
for(reg in region_names){
  print(reg)
  region_df <- regions[, c("varName", "Scale", "SubCompart", reg)]
  names(region_df)[ncol(region_df)] <- "Waarde"
  
  region_df <- region_df |>
    mutate(Waarde = as.numeric(Waarde))
  
  World$mutateVars(region_df)
  World$UpdateDirty(unique(region_df$varName))
  #vars_to_update <- unique(region_df$varName) 
  
  #Retrieve the volume of each receiving compartment defined
  df_volumes <- World$fetchData("Volume")
  states_with_vol <- states %>% # join states with df_volumes to retrieve volume based on their code name (w1RS, etc)
    left_join(df_volumes, by = c("Scale", "SubCompart"))
  # now filter only receiving compartments of interest
  volume_compartments <- states_with_vol %>%
    filter(Abbr %in% receiving_compartments, Species == "Solid") %>%   # keep only Solid
    arrange(factor(Abbr, levels = receiving_compartments)) %>%         # keep original order
    pull(Volume)
  
  
  df_areas <- World$fetchData("Area")
  states_with_area <- states %>% # join states with df_areas to retrieve volume based on their code name (w1RS, etc)
    left_join(df_areas, by = c("Scale", "SubCompart"))
  # now filter only receiving compartments of interest
  areas_compartments <- states_with_area %>%
    filter(Abbr %in% receiving_compartments, Species == "Solid") %>%   # keep only Solid
    arrange(factor(Abbr, levels = receiving_compartments)) %>%         # keep original order
    pull(Area)
  
  #remove flows to moderate, arctic, tropic
  airflow_df <- data.frame(varName = c("AirFlow", "AirFlow", "AirFlow"), 
                           Scale = c("Arctic", "Moderate", "Tropic"),
                           Waarde = c(10^-20,  10^-20, 10^-20)) 
  World$mutateVars(airflow_df)
  World$UpdateDirty(unique(airflow_df$varName))
  
  ### SDF matrix for that region
  FracSpe_C_marine <- World$fetchData("dens_marine_species_C") #fraction of marine species living in continental sea water - Tittensor et al., 2010
  FracSpe_G_marine <- World$fetchData("dens_marine_species_RoW") #fraction of marine species living in global sea water - Tittensor et al., 2010
  FracSpe_C_aqua <- World$fetchData("dens_fw_species_C") #density of species living in continent - IUCN Red List, 2024
  FracSpe_G_aqua <- World$fetchData("dens_fw_species_RoW") #density of species living in the rest of the world  IUCN Red List, 2024
  FracSpe_C_ter <- World$fetchData("dens_ter_species_C") #density of species living in continent - IUCN Red List, 2024
  FracSpe_G_ter <- World$fetchData("dens_ter_species_RoW") #density of species living in the rest of the world  IUCN Red List, 2024
  
  SDF_ME_sw_ws_C = FracSpe_ws_marine*FracSpe_C_marine #seawater water surface
  SDF_ME_sw_wc_C = FracSpe_wc_marine*FracSpe_C_marine #seawater water column
  SDF_ME_sw_sed_C = FracSpe_sed_marine*FracSpe_C_marine #seawater sediments
  SDF_ME_sw_ws_G = FracSpe_ws_marine*FracSpe_G_marine #seawater water surface
  SDF_ME_sw_wc_G = FracSpe_wc_marine*FracSpe_G_marine #seawater water column
  SDF_ME_sw_sed_G = FracSpe_sed_marine*FracSpe_G_marine #seawater sediments
  
  SDF_AE_lw_C = FracSpe_lake*FracSpe_wc_aqua*FracSpe_C_aqua
  SDF_AE_lw_sed_C = FracSpe_lake*FracSpe_sed_aqua*FracSpe_C_aqua
  SDF_AE_fw_C = FracSpe_river*FracSpe_wc_aqua*FracSpe_C_aqua
  SDF_AE_fw_sed_C = FracSpe_river*FracSpe_sed_aqua*FracSpe_C_aqua
  SDF_AE_lw_G = FracSpe_lake*FracSpe_wc_aqua*FracSpe_G_aqua
  SDF_AE_lw_sed_G = FracSpe_lake*FracSpe_sed_aqua*FracSpe_G_aqua
  SDF_AE_fw_G = FracSpe_river*FracSpe_wc_aqua*FracSpe_G_aqua
  SDF_AE_fw_sed_G = FracSpe_river*FracSpe_sed_aqua*FracSpe_G_aqua
  
  SDF_TE_nat_soil_C = FracSpe_nat_soil*FracSpe_C_ter
  SDF_TE_agr_soil_C = FracSpe_agr_soil*FracSpe_C_ter
  SDF_TE_nat_soil_G = FracSpe_nat_soil*FracSpe_G_ter
  SDF_TE_agr_soil_G = FracSpe_agr_soil*FracSpe_G_ter
  # verification:
  total <- SDF_AE_lw_C + SDF_AE_lw_sed_C + SDF_AE_fw_C + SDF_AE_fw_sed_C +
    SDF_AE_lw_G + SDF_AE_lw_sed_G + SDF_AE_fw_G + SDF_AE_fw_sed_G + #fw SDF  1 
    SDF_ME_sw_ws_C + SDF_ME_sw_wc_C + SDF_ME_sw_sed_C + SDF_ME_sw_ws_G + SDF_ME_sw_wc_G + SDF_ME_sw_sed_G + #marine SDF = 1
    SDF_TE_nat_soil_C+SDF_TE_agr_soil_C+SDF_TE_nat_soil_G+SDF_TE_agr_soil_G #terrestrial SDF = 1
  
  #if (abs(total - 3) > 1e-8) {
  #  stop("Sum of SDF_AE terms is not equal to 3. Current value: ", total)
  #}
  
  # Create an empty 3x18 matrix
  tab_SDF <- matrix(0, nrow = 3, ncol = 20)
  
  # Name rows and columns
  rownames(tab_SDF) <- c("Marine_Ecosystem", "Freshwater_Ecosystem", "Terrestrial_Ecosystem")
  colnames(tab_SDF) <- c("a_C","fw_C","lw_C", "sw_ws_C", "sw_wc_C", "fw_sed_C","lw_sed_C",  "sw_sed_C",     #the order is slightly different than in Louvet et al. 20205
                         "nat_soil_C", "agr_soil_C", "a_G","fw_G","lw_G",  "sw_ws_G", "sw_wc_G", "fw_sed_G", 
                         "lw_sed_G", "sw_sed_G", "nat_soil_G", "agr_soil_G")
  # Fill the matrix with the SDFs
  SDF <- as.data.frame(tab_SDF)
  # Marine ecosystem
  SDF["Marine_Ecosystem", "sw_ws_C"]       <- SDF_ME_sw_ws_C
  SDF["Marine_Ecosystem", "sw_wc_C"]       <- SDF_ME_sw_wc_C
  SDF["Marine_Ecosystem", "sw_sed_C"]   <- SDF_ME_sw_sed_C
  SDF["Marine_Ecosystem", "sw_ws_G"]       <- SDF_ME_sw_ws_G
  SDF["Marine_Ecosystem", "sw_wc_G"]       <- SDF_ME_sw_wc_G
  SDF["Marine_Ecosystem", "sw_sed_G"]   <- SDF_ME_sw_sed_G
  # Freshwater ecosystem
  SDF["Freshwater_Ecosystem", "lw_C"]      <- SDF_AE_lw_C
  SDF["Freshwater_Ecosystem", "lw_sed_C"]  <- SDF_AE_lw_sed_C
  SDF["Freshwater_Ecosystem", "fw_C"]      <- SDF_AE_fw_C
  SDF["Freshwater_Ecosystem", "fw_sed_C"]  <- SDF_AE_fw_sed_C
  SDF["Freshwater_Ecosystem", "lw_G"]      <- SDF_AE_lw_G
  SDF["Freshwater_Ecosystem", "lw_sed_G"]  <- SDF_AE_lw_sed_G
  SDF["Freshwater_Ecosystem", "fw_G"]      <- SDF_AE_fw_G
  SDF["Freshwater_Ecosystem", "fw_sed_G"]  <- SDF_AE_fw_sed_G
  # Terrestrial ecosystem
  SDF["Terrestrial_Ecosystem", "nat_soil_C"] <- SDF_TE_nat_soil_C
  SDF["Terrestrial_Ecosystem", "agr_soil_C"] <- SDF_TE_agr_soil_C
  SDF["Terrestrial_Ecosystem", "nat_soil_G"] <- SDF_TE_nat_soil_G
  SDF["Terrestrial_Ecosystem", "agr_soil_G"] <- SDF_TE_agr_soil_G
  
  ###############
  #
  #### SDF based on GEP for GLAM: Same thing with GEP instead of species richness
  #
  #################
  # Create an empty 3x18 matrix
  tab_SDF_GEP <- matrix(0, nrow = 3, ncol = 20)
  
  # Name rows and columns
  rownames(tab_SDF_GEP) <- c("Marine_Ecosystem", "Freshwater_Ecosystem", "Terrestrial_Ecosystem")
  colnames(tab_SDF_GEP) <- c("a_C","fw_C","lw_C", "sw_ws_C", "sw_wc_C", "fw_sed_C","lw_sed_C",  "sw_sed_C",     #the order is slightly different than in Louvet et al. 20205
                         "nat_soil_C", "agr_soil_C", "a_G","fw_G","lw_G",  "sw_ws_G", "sw_wc_G", "fw_sed_G", 
                         "lw_sed_G", "sw_sed_G", "nat_soil_G", "agr_soil_G")
  # Fill the matrix with the SDFs
  SDF_GEP <- as.data.frame(tab_SDF_GEP)
  FracSpe_C_marine_GEP <- World$fetchData("gep_mar_C") 
  FracSpe_G_marine_GEP <- World$fetchData("gep_mar_G") 
  FracSpe_C_aqua_GEP <- World$fetchData("gep_fw_C") 
  FracSpe_G_aqua_GEP <- World$fetchData("gep_fw_G") 
  FracSpe_C_ter_GEP <- World$fetchData("gep_ter_C") 
  FracSpe_G_ter_GEP <- World$fetchData("gep_ter_G") 
  # Marine ecosystem

  
  SDF_ME_sw_ws_C_GEP = FracSpe_ws_marine*FracSpe_C_marine_GEP #seawater water surface
  SDF_ME_sw_wc_C_GEP = FracSpe_wc_marine*FracSpe_C_marine_GEP #seawater water column
  SDF_ME_sw_sed_C_GEP = FracSpe_sed_marine*FracSpe_C_marine_GEP #seawater sediments
  SDF_ME_sw_ws_G_GEP = FracSpe_ws_marine*FracSpe_G_marine_GEP #seawater water surface
  SDF_ME_sw_wc_G_GEP = FracSpe_wc_marine*FracSpe_G_marine_GEP #seawater water column
  SDF_ME_sw_sed_G_GEP = FracSpe_sed_marine*FracSpe_G_marine_GEP #seawater sediments
  
  SDF_AE_lw_C_GEP = FracSpe_lake*FracSpe_wc_aqua*FracSpe_C_aqua_GEP
  SDF_AE_lw_sed_C_GEP = FracSpe_lake*FracSpe_sed_aqua*FracSpe_C_aqua_GEP
  SDF_AE_fw_C_GEP = FracSpe_river*FracSpe_wc_aqua*FracSpe_C_aqua_GEP
  SDF_AE_fw_sed_C_GEP = FracSpe_river*FracSpe_sed_aqua*FracSpe_C_aqua_GEP
  SDF_AE_lw_G_GEP = FracSpe_lake*FracSpe_wc_aqua*FracSpe_G_aqua_GEP
  SDF_AE_lw_sed_G_GEP = FracSpe_lake*FracSpe_sed_aqua*FracSpe_G_aqua_GEP
  SDF_AE_fw_G_GEP = FracSpe_river*FracSpe_wc_aqua*FracSpe_G_aqua_GEP
  SDF_AE_fw_sed_G_GEP = FracSpe_river*FracSpe_sed_aqua*FracSpe_G_aqua_GEP
  
  SDF_TE_nat_soil_C_GEP = FracSpe_nat_soil*FracSpe_C_ter_GEP
  SDF_TE_agr_soil_C_GEP = FracSpe_agr_soil*FracSpe_C_ter_GEP
  SDF_TE_nat_soil_G_GEP = FracSpe_nat_soil*FracSpe_G_ter_GEP
  SDF_TE_agr_soil_G_GEP = FracSpe_agr_soil*FracSpe_G_ter_GEP
  
  
  SDF_GEP["Marine_Ecosystem", "sw_ws_C"]       <- SDF_ME_sw_ws_C_GEP
  SDF_GEP["Marine_Ecosystem", "sw_wc_C"]       <- SDF_ME_sw_wc_C_GEP
  SDF_GEP["Marine_Ecosystem", "sw_sed_C"]   <- SDF_ME_sw_sed_C_GEP
  SDF_GEP["Marine_Ecosystem", "sw_ws_G"]       <- SDF_ME_sw_ws_G_GEP
  SDF_GEP["Marine_Ecosystem", "sw_wc_G"]       <- SDF_ME_sw_wc_G_GEP
  SDF_GEP["Marine_Ecosystem", "sw_sed_G"]   <- SDF_ME_sw_sed_G_GEP
  # Freshwater ecosystem
  SDF_GEP["Freshwater_Ecosystem", "lw_C"]      <- SDF_AE_lw_C_GEP
  SDF_GEP["Freshwater_Ecosystem", "lw_sed_C"]  <- SDF_AE_lw_sed_C_GEP
  SDF_GEP["Freshwater_Ecosystem", "fw_C"]      <- SDF_AE_fw_C_GEP
  SDF_GEP["Freshwater_Ecosystem", "fw_sed_C"]  <- SDF_AE_fw_sed_C_GEP
  SDF_GEP["Freshwater_Ecosystem", "lw_G"]      <- SDF_AE_lw_G_GEP
  SDF_GEP["Freshwater_Ecosystem", "lw_sed_G"]  <- SDF_AE_lw_sed_G_GEP
  SDF_GEP["Freshwater_Ecosystem", "fw_G"]      <- SDF_AE_fw_G_GEP
  SDF_GEP["Freshwater_Ecosystem", "fw_sed_G"]  <- SDF_AE_fw_sed_G_GEP
  # Terrestrial ecosystem
  SDF_GEP["Terrestrial_Ecosystem", "nat_soil_C"] <- SDF_TE_nat_soil_C_GEP
  SDF_GEP["Terrestrial_Ecosystem", "agr_soil_C"] <- SDF_TE_agr_soil_C_GEP
  SDF_GEP["Terrestrial_Ecosystem", "nat_soil_G"] <- SDF_TE_nat_soil_G_GEP
  SDF_GEP["Terrestrial_Ecosystem", "agr_soil_G"] <- SDF_TE_agr_soil_G_GEP
  ###
  
  # Polymer loop: one function call per polymer, so that polymers can be run in parallel (n_cores > 1)
  run_polymer <- function(pol) {
    #Import the properties of that polymer from the df
    print(pol)
    if (n_cores > 1) {
      set.seed(parallel_seed + 1000 * match(reg, region_names) + match(pol, polymer_names))
    }
    combination_results <- list()
    vars_to_update <- c()

    polymer_df <- plastic_values[,c("varName","SubCompart",pol)]
    names(polymer_df)[ncol(polymer_df)] <- "Waarde" #rename the last column "Waarde" instead of name of pol
    polymer_df_selected <- polymer_df |>
      #slice(1:22) |>
      mutate(Waarde=as.numeric(Waarde))

    #Set the new values for RhoS, Kssdr, CorFacSSA and MinSettVel for the specific polymer
    World$mutateVars(polymer_df_selected)
    #World$UpdateDirty(unique(polymer_df$varName))
    vars_to_update <- c(vars_to_update,unique(polymer_df_selected$varName))

    World$SetConst(Degrading_enzyme = polymer_df$Waarde[polymer_df$varName == "Degrading_enzyme"])
    World$SetConst(DegApproach = polymer_df$Waarde[polymer_df$varName == "DegApproach"])

    #Import distribution on degradation data
    degradation_CI <- degradation_CI_all  %>%
      filter(pol == !!pol) #Keep only the rows where distribution is defined for pol

    varFuns <- World$makeInvFuns(degradation_CI)

    #TRWP-specific EEFs in water and soil
    eef_samples_pol <- if (pol == "TRWP") eef_monte_carlo_trwp else eef_monte_carlo

    #loop over size
    for (size in sizes) {
      print(size)
      size_df <- data.frame(varName = "RadS",
                            Waarde = size/2*1000) #convert D to R and convert um to nm (SBoo takes RadS as nm)

      World$mutateVars(size_df)
      #World$UpdateDirty(unique(variable_df$varName))
      vars_to_update <- c(vars_to_update,unique(size_df$varName))

      #Loop over shape
      for (shape in shapes) {

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
        #World$UpdateDirty(unique(c(shape_df$varName, dimensions_df$varName)))

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
        World$UpdateDirty(vars_to_update)

        #solve
        emissions <- data.frame(Abbr = c("aRS"), Emis = 1/3600/24, RUN = (1:n_samples)) #emission of 1kg/d input in kg/s - resulting steady state masses (kg) can be divided by 1 (kg/d) to get FF (d)
        World$NewSolver("KMatrixOnlySolver")
        World$Solve(emissions = emissions, var_box_df = degradation_CI, var_invFun = varFuns, nRUNs = n_samples)

        k_matrix = World$K_matrix() #New in SBoo: this returns a list of matrix for the probabilistic solver

        # Setup on first iteration (only structure)
        if(is.null(setup)) {
          setup <<- setup_fill_patterns(k_matrix[[1]], X_list)
        }

        # FFs of all Monte Carlo samples: steady state, 20 yrs and 100 yrs time horizon
        # (DYNAMIC FATE FACTORS: mass integrated from 0 to the time horizon, CFs integrate impacts from 0 to the time horizon)
        ff_matrices <- lapply(k_matrix, function(k_mat) {
          compute_ff_matrices(k_mat, setup, c(time_horizon_seconds_1, time_horizon_seconds_2))
        })

        if (is.null(rec_map)) {
          rec_map <<- build_receiving_map(rownames(ff_matrices[[1]]$ss), states)
        }

        process_ff <- function(ff_matrices_list) {
          process_monte_carlo_combination(
            ff_matrices_list = ff_matrices_list,
            eef_samples = eef_samples_pol,
            reg = reg,
            pol = pol,
            size = size,
            shape = shape,
            emission_compartments = emission_compartments,
            rec_map = rec_map,
            compartment_names = compartment_names,
            volume_compartments = volume_compartments,
            areas_compartments = areas_compartments,
            SDF = SDF,
            SDF_GEP = SDF_GEP,
            SF = SF,
            list_tot_species = list_tot_species
          )
        }

        combination_results[[length(combination_results) + 1]] <- list(
          ss = process_ff(lapply(ff_matrices, `[[`, 1)),
          yrs_20 = process_ff(lapply(ff_matrices, `[[`, 2)),
          yrs_100 = process_ff(lapply(ff_matrices, `[[`, 3))
        )

        vars_to_update = c() #reset to avoid recomputing vars that have not changed and making vars_to_update excessively long
      }
    }
    combination_results
  }

  if (n_cores > 1) {
    polymer_results <- parallel::mclapply(polymer_names, run_polymer, mc.cores = n_cores, mc.preschedule = FALSE)
    failed <- vapply(polymer_results, function(x) inherits(x, "try-error"), logical(1))
    if (any(failed)) {
      stop("Polymer(s) failed in ", reg, ": ", paste(polymer_names[failed], collapse = ", "), "\n",
           paste(unlist(polymer_results[failed]), collapse = "\n"))
    }
  } else {
    polymer_results <- lapply(polymer_names, run_polymer)
  }
  all_results <- c(all_results, unlist(polymer_results, recursive = FALSE))
}
print(Sys.time() - start_time)

# Combine results of all combinations
bind_results <- function(time_key, stats_key) {
  bind_rows(lapply(all_results, function(x) x[[time_key]][[stats_key]]))
}

results_FF <- bind_results("ss", "ff_stats")
results_CF_mid_PAF_day <- bind_results("ss", "cf_mid_stats")
results_CF_end_PDF_year <- bind_results("ss", "cf_end_pdf_stats")
results_CF_end_species_year <- bind_results("ss", "cf_end_species_stats")
results_CF_end_PDF_m2_year <- bind_results("ss", "cf_end_pdf_m2_stats")

# 20 yrs time horizon only for ReCiPe
results_FF_20_yrs <- bind_results("yrs_20", "ff_stats")
results_CF_mid_PAF_day_20_yrs <- bind_results("yrs_20", "cf_mid_stats")
results_CF_end_species_year_20_yrs <- bind_results("yrs_20", "cf_end_species_stats")

results_FF_100_yrs <- bind_results("yrs_100", "ff_stats")
results_CF_mid_PAF_day_100_yrs <- bind_results("yrs_100", "cf_mid_stats")
results_CF_end_PDF_year_100_yrs <- bind_results("yrs_100", "cf_end_pdf_stats")
results_CF_end_species_year_100_yrs <- bind_results("yrs_100", "cf_end_species_stats")
results_CF_end_PDF_m2_year_100_yrs <- bind_results("yrs_100", "cf_end_pdf_m2_stats")


# Path
####Save steady state FFs and CFs
out_file <- paste0("../../results/results_FF_CF_CI", results_suffix, ".xlsx")

# Load workbook if it exists, otherwise create a new one
wb <- if (file.exists(out_file)) {
  loadWorkbook(out_file)
} else {
  createWorkbook()
}

# Helper to fully replace a sheet
replace_sheet <- function(wb, sheet_name, data) {
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data, withFilter = FALSE)
}

# Replace result sheets
replace_sheet(wb, "FF_d", results_FF)
replace_sheet(wb, "CF_mid_PAF_d", results_CF_mid_PAF_day)
replace_sheet(wb, "CF_end_PDF_yr", results_CF_end_PDF_year)
replace_sheet(wb, "CF_end_species_yr", results_CF_end_species_year)
replace_sheet(wb, "CF_end_PDF_m2_yr", results_CF_end_PDF_m2_year)

# Save without deleting other sheets
saveWorkbook(wb, out_file, overwrite = TRUE)



####Save time explicit FFs and CFs
out_file <- paste0("../../results/results_FF_CF_CI_time_horizon", results_suffix, ".xlsx")

# Load workbook if it exists, otherwise create a new one
wb <- if (file.exists(out_file)) {
  loadWorkbook(out_file)
} else {
  createWorkbook()
}

# Helper to fully replace a sheet
replace_sheet <- function(wb, sheet_name, data) {
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data, withFilter = FALSE)
}

# Replace result sheets
replace_sheet(wb, "FF_d_20_yrs", results_FF_20_yrs)
replace_sheet(wb, "CF_mid_PAF_d_20_yrs", results_CF_mid_PAF_day_20_yrs)
#replace_sheet(wb, "CF_end_PDF_year_20_yrs", results_CF_end_PDF_year_20_yrs)
replace_sheet(wb, "CF_species_yr_20_yrs", results_CF_end_species_year_20_yrs)
#replace_sheet(wb, "CF_end_PDF_m2_year_20_yrs", results_CF_end_PDF_m2_year_20_yrs)

# Replace result sheets
replace_sheet(wb, "FF_d_100_yrs", results_FF_100_yrs)
replace_sheet(wb, "CF_mid_PAF_d_100_yrs", results_CF_mid_PAF_day_100_yrs)
replace_sheet(wb, "CF_end_PDF_yr_100_yrs", results_CF_end_PDF_year_100_yrs)
replace_sheet(wb, "CF_end_species_yr_100_yrs", results_CF_end_species_year_100_yrs)
replace_sheet(wb, "CF_end_PDF_m2_yr_100_yrs", results_CF_end_PDF_m2_year_100_yrs)


# Save without deleting other sheets
saveWorkbook(wb, out_file, overwrite = TRUE)




