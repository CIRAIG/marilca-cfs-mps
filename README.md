# MarILCA Characterization Factors for Microplastics, Tire Particles and Cellulosic Fibers in Life Cycle Assessment: Regionalized impacts in marine, freshwater and terrestrial ecosystems

[![DOI](https://zenodo.org/badge/1165898289.svg)](https://doi.org/10.5281/zenodo.20087936) (all versions; resolves to the latest version)

This repository provides the code and data used to compute fate factores (FFs) and characterization factors (CFs) based on SimpleBox object-oriented (SBoo). SBoo Version 2026.4.0 is used (https://github.com/rivm-syso/SBoo/releases/tag/2026.4.0). Earlier releases of this repository used SBoo 2026.3.1 (https://doi.org/10.5281/zenodo.19388881) 

This repository provides all scripts and input data required to:

- Compute steady-state and dynamic CFs
- Retrieve default CFs
- Generate graphs and figure

If you use the CFs, please cite:
- The publication: Saadi, N., Hajjar, C., Ospital, L., Quik, J.T.K., Corella-Puertas, E., Hids, A.R., Lebrun, M., Louvet, J., Tang, Y., Thonemann, N., Boulay, A.-M. (2027). MarILCA characterization factors for microplastics, tire particles and cellulosic fibers in life cycle assessment: the PEBBLE method for regionalized physical impacts in marine, freshwater and terrestrial ecosystems. *Resources, Conservation and Recycling*, 237, 109193. https://doi.org/10.1016/j.resconrec.2026.109193
- The version used, with its DOI (see Versions below)

Citation metadata is also provided in `CITATION.cff` ("Cite this repository" on GitHub).

### Versions

| Version | Date | DOI | SBoo version | Main changes |
|---|---|---|---|---|
| v1.1.0 | 2026-10-28 | to be added after the release | 2026.4.0 | TRWP-specific EEFs in water and soil, Linen soil degradation update, ReCiPe midpoint and endpoint CFs, faster code |
| v1.0.0 | 2026-05-08 | [10.5281/zenodo.20087937](https://doi.org/10.5281/zenodo.20087937) | 2026.3.1 | First public release |

All versions: [10.5281/zenodo.20087936](https://doi.org/10.5281/zenodo.20087936). The list of changes is in `CHANGELOG.md`.

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

Run settings are at the top of the script (section "Run settings"):
- `n_samples`: number of Monte Carlo runs per combination (default 100)
- `n_cores`: by default, all cores but one on macOS/Linux, where the polymers of each region are run in parallel (forked processes) with a fixed seed per polymer (`parallel_seed`), so results are reproducible. On Windows, forking is not available and the script runs serially (`n_cores = 1`). The serial run reproduces the random number stream of previous versions, so its Monte Carlo samples differ from the parallel run
- Loop subsets and `results_suffix` can be set for quick tests without overwriting the results

Tire and road wear particles (TRWP) use TRWP-specific effect factors (physical effects) in water and soil: EEF_water = 18.521 [1.688 - 203.244] and EEF_soil = 0.197 [0.038 - 2.141] PAF m3/kg (95% CI). The sediment EEF is the same as for the other polymers.

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

## 4. ReCiPe CFs

To compute the **ReCiPe CFs** for the three cultural perspectives (Individualist: 20 years, Hierarchist: 100 years, Egalitarian: steady state), run from the repository root after `03_Default_CF.R`:

    source("results/ReCiPe/ReCiPe_CF.R")

This creates `results/ReCiPe/ReCiPe_CF.xlsx` with:
- Midpoint CFs in kg PET, 1µm, sphere, rw-eq/kg (freshwater), sw-eq/kg (marine) and ns-eq/kg (terrestrial physical effects), one sheet per cultural perspective. Reference flow: PET, 1µm, sphere, World region, emitted to river water (rw), seawater surface (sw) or natural soil (ns)
- Endpoint CFs (species·yr/kg), one sheet per cultural perspective
- Midpoint to endpoint conversion factors (species·yr/kg PET, 1µm, sphere, rw/sw/ns-eq)

Midpoint and endpoint sheets have one row per flow and impact category. The variables are documented in the README sheet of the file.

---

## 5. Repository Structure

    ├── data/                         # Scenario-specific datasets for SBoo
    ├── input/                        # Model input files (Regionalized data and TrackMPD data)
    ├── results/                      # Model outputs
    │   └── ReCiPe/                   # ReCiPe midpoint and endpoint CFs (ReCiPe_CF.R, ReCiPe_CF.xlsx)
    ├── figures/                      # Figures shown in the publication and the code to make them
    │   ├── SI_Graphing.ipynb         # Graphing and visualization notebook (figures of the publication and SI)
    │   └── Plot_comparison_with_main.R  # Figure of the CF changes per material vs the previous version
    ├── previous marilca cfs/         # Previous CF datasets (MarILCA)
    │
    ├── InstallSBoo.R                 # Installation script
    ├── installRequirements.R         # Installation
    ├── 02_Run_SB_CI.R                # Main steady-state model run
    ├── 03_Default_CF.R               # Default CF calculation
    ├── 04_Run_SB_Dyn.R               # Dynamic model run 
    ├── Compare_with_main.R           # Comparison of SI_C/SI_D with another git branch (default: main)
    │
    ├── CHANGELOG.md                  # List of changes per version
    ├── CITATION.cff                  # Citation metadata (also used by Zenodo)
    ├── LICENSE.md
    └── README.md

---

### License

Creative Commons Attribution Share Alike 4.0 International, as specified in `LICENSE.md`.
