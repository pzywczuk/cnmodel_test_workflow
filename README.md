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

```

## Repository Structure

```R
reproducible-workflow/
|-- data/
|   |-- ch0e2_drivers.rds       # Input data file
|-- scripts/
|   |-- cn_model_test_run.R              # Main workflow script
|-- README.md                   # This README file

```

## Included Libraries

```R
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(visdat)
library(here)
library(lubridate)
library(readr)
library(naniar)
library(purrr)
library(rsofun)

```

Note: The rsofun package is specifically installed from the cnmodel branch.

## Included Libraries

```R
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(visdat)
library(here)
library(lubridate)
library(readr)
library(naniar)
library(purrr)
library(rsofun)

```

# Workflow Overview

## Load Data

The main input data for this model is sourced from FLUXNET (CH-OE2) and loaded from an RDS file:

```R
ch0e2_drivers <- readRDS(here("data", "ch0e2_drivers.rds"))

```

## N Deposition

A sample N deposition input is added to the forcing time series, specifying the quantity of N added daily.

```R
n_input_test <- function(data) {
  data <- data %>%
    mutate(forcing = purrr::map(
      forcing, 
      ~mutate(.,
              fharv = 0.0,
              dno3 = 0.002003263,
              dnh4 = 0.002017981))
    )
  return(data)
}

ch0e2_drivers <- n_input_test(ch0e2_drivers)

```

## Harvesting

Assumes zero harvesting and new seeds planted post-harvest, updating drivers accordingly.

```R
fharv_seed <- function(data, use_cseed = 5, cn_seed = 20) {
  use_nseed <- use_cseed / cn_seed
  
  data <- data %>%
    mutate(
      forcing = purrr::map(
        forcing, ~mutate(
          .,
          fharv = ifelse(month(date) == 7 & mday(date) == 15, 0.0, 0.0),
          cseed = ifelse(month(date) == 2 & mday(date) == 15, use_cseed, 0.0),
          nseed = ifelse(month(date) == 2 & mday(date) == 15, use_nseed, 0.0))
      )
    )
  
  return(data)
}

ch0e2_drivers <- fharv_seed(ch0e2_drivers)

```

## Simulation Parameters

Sets the spinup and recycle years for the model to ensure equilibrium.

```R
modify_params <- function(df_list, spinupyears_val, recycle_val) {
  df_list <- map(df_list, ~mutate(.x, spinupyears = spinupyears_val, recycle = recycle_val))
  return(df_list)
}

ch0e2_drivers$params_siml <- modify_params(ch0e2_drivers$params_siml, 2021, 2)


```

## Model Parameter Values

Defines various model parameters for the P-model and plant characteristics.

```R
pars <- list(
  # Add parameter values as per your model requirements
  kphio = 0.04998,
  # ... [other parameters] ...
)

```

## Model Parameter Values

Run the Model

```R
cnmodel_run_plot <- function(drivers, pars, save_path, df_name) {
  # Run the model
  output <- runread_cnmodel_f(drivers, par = pars)
  # ... [generate and save plots] ...
}

cnmodel_run_plot(ch0e2_drivers, pars, "CH-0E2_NTRUE")
```



