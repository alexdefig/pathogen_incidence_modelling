# Pathogen Incidence Modelling - Case Study 1

## Instructions for Use

This repository contains three scripts used to **recreate the analysis** in **Case Study 1**. These scripts are detailed in the table below.

## Installation

Before running any of the scripts, install or load the required packages using the following command in R:

``` r
source("R/packages/install_packages.R")
```

## Usage

Run the scripts in the following order:

1.  **`1_visualise_data.R`** → Maps pathogen incidence locations, incidence rates, and temperature data.\
2.  **`2_fit_gams.R`** → Fits **Generalized Additive Models** (GAMs) to analyse the relationship between temperature and pathogen incidence. Compares model performance.\
3.  **`3_fit_gmrf.R`** → Fits a **Gaussian Markov Random Field (GMRF)** using **INLA** to model pathogen incidence.

## 📂 File Structure

| **File / Script**        | **Purpose**                                                                                                                                                             |
|------------------|------------------------------------------------------|
| **`1_visualise_data.R`** | Maps pathogen incidence locations, incidence rates, and temperatures.                                                                                                   |
| **`2_fit_gams.R`**       | Fits Generalized Additive Models (GAMs) to analyse the relationship between temperature and pathogen incidence. Compares model performance.                             |
| **`3_fit_gmrf.R`**       | Fits a **Gaussian Markov Random Field (GMRF)** using **INLA**, modelling pathogen incidence.                                                |
| **`R/data/`**            | Contains a `.csv` file of the pathogen incidence data.                                                                                                                  |
| **`R/utility/`**         | Folder containing `.R` files:<br> - `mapping.R` (mapping functions)<br> - `helper.R` (helper functions)                                                                 |
| **`R/packages/`**        | Folder containing scripts to install and load required packages:<br> - `packages.R` (list of required packages)<br> - `install_packages.R` (script to install packages) |
| **`R/lib/`**             | A `.tar` file for `rgeoboundaries` for shapefiles.                                                                                                                      |
| **`R/out/`**             | Folder containing prepared figures as well as a file (**`gams/`**) of model summaries and model checks for each fitted GAM model.                                       |
