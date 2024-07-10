# cnmodel_test_workflow
running cnmodel for error tests 

# Reproducible Workflow for CN Model Using `rsofun`

This repository contains a reproducible workflow that demonstrates the setup and usage of the `rsofun` package, specifically built from the `cnmodel` branch. The workflow includes necessary scripts, data, and steps to execute a basic CN model simulation with additional processing, parameter updates, and result plotting.

## Prerequisites

Before running the scripts in this repository, ensure you have the following R packages installed:

```R
install.packages(c("dplyr", "tidyr", "ggplot2", "patchwork", "cowplot", "visdat", 
                   "here", "lubridate", "readr", "naniar", "purrr", "remotes"))
remotes::install_github("stineb/rsofun", ref = "cnmodel")

reproducible-workflow/
|-- data/
|   |-- ch0e2_drivers.rds       # Input data file
|-- scripts/
|   |-- cn_model_test_run.R              # Main workflow script
|-- README.md                   # This README file

`` 
