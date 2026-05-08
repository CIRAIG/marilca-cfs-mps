# MarILCA Characterization Factors for Microplastics, Tire Particles and Cellulosic Fibers in Life Cycle Assessment: Regionalized impacts in marine, freshwater and terrestrial ecosystems

DOI: v1.0.0 [![DOI](https://zenodo.org/badge/1165898289.svg)](https://doi.org/10.5281/zenodo.20087936)

This repository provides the code and data used to compute fate factores (FFs) and characterization factors (CFs) based on SimpleBox object-oriented (SBoo). So far, SBoo Version 2026.3.1 was used (https://doi.org/10.5281/zenodo.19388881) 

This repository provides all scripts and input data required to:

- Compute steady-state and dynamic CFs
- Retrieve default CFs
- Generate graphs and figure

If you use the CFs, please cite:
- The publication (preprint: https://dx.doi.org/10.2139/ssrn.6727270)
- The version used (with the DOI corresponding to the release used)

---

## 1. Install SBoo

To install SBoo, open R and run:

    source("InstallSBoo.R")

This will:
- Install the selected version/branch of SBoo.
- Create a new folder called SimpleBox, which will contain SBoo and SBooScripts folders.

---

## 2. Run

### Steady-State CFs

    source("02_Run_SB_CI.R")

### Dynamic Simulation

    source("04_Run_SB_Dyn.R")

All outputs are saved in:

    /results/

Where the following are stored:
 
#### Steady sate FFs and CFs:

     /results/SI_C.xlsx 

####  Time-horizon dependent FFs and CFs:
    
     /results/SI_D.xlsx
    
---

## 3. Default CFs

To compute the **default characterization factors (CFs)**:

    source("03_Default_CF.R")

These CFs are based on:

- Emission weighted averages of CFs for unknown material CFs
- Regional emission weighted CFs of plastics for unknown emission location
- Default size (diamter or thickness) CFs for each shape/material
- A decision tree is provided in Figure S.19 to select the appropriate CF

---

## 4. Repository Structure

    ├── data/                         # Scenario-specific datasets for SBoo
    ├── input/                        # Model input files (Regionalized data and TrackMPD data)
    ├── results/                      # Model outputs
    ├── figures/                      # Generated figures shown in the publication
    ├── previous marilca cfs/         # Previous CF datasets (MarILCA)
    │
    ├── InstallSBoo.R                 # Installation script
    ├── installRequirements.R         # Installation
    ├── 02_Run_SB_CI.R                # Main steady-state model run
    ├── 03_Default_CF.R               # Default CF calculation
    ├── 04_Run_SB_Dyn.R               # Dynamic model run 
    ├── Compare_outcomes.R            # Comparison of model outputs
    ├── SI_Graphing.ipynb             # Graphing and visualization notebook
    │
    ├── LICENSE.md
    └── README.md

---

### License

Creative Commons Attribution Share Alike 4.0 International, as specified in `LICENSE.md`.
