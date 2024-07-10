# Load Libraries
#----------------------------------------------------------------

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

if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github("stineb/rsofun", ref = "cnmodel")
library(rsofun)

#----------------------------------------------------------------
# Load Data
#----------------------------------------------------------------

# FLUXNET 
ch0e2_drivers <- readRDS(here("cnmodel_test_workflow", "data", "ch0e2_drivers.rds"))


#----------------------------------------------------------------
# N deposition
#----------------------------------------------------------------

# Reactive N input needs to be in gN per day 
# added to forcing time series: specify quantity of N added on which day 

# Example N input given a constant rate each day 
n_input_test <- function(data) {
  data <- data %>%
    mutate(forcing = purrr::map(
      forcing, 
      ~mutate(
        .,
        fharv = 0.0,
        dno3 = 0.002003263,
        dnh4 = 0.002017981))
    )
  return(data)
}

ch0e2_drivers <- n_input_test(ch0e2_drivers)

#----------------------------------------------------------------
# Harvesting 
#----------------------------------------------------------------

# The fraction of biomass harvested per day needs to be specified in the forcing time series 
# cseed and nseed new seeds added after harvesting 
# Example driver update assumes harvesting is 0 and new seeds planted after harvest 

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

#----------------------------------------------------------------
# Simulation parameters
#----------------------------------------------------------------
# The spinup of cn_model must be long enough to equilibrate fluxes 

# Function to modify specific columns in each dataframe
modify_params <- function(df_list, spinupyears_val, recycle_val) {
  # Map over each dataframe in the list
  df_list <- map(df_list, ~ {
    # Modify specific columns
    mutate(.x,
           spinupyears = spinupyears_val,
           recycle = recycle_val)
  })
  
  return(df_list)
}

# FLUXNET
ch0e2_drivers$params_siml <- modify_params(ch0e2_drivers$params_siml, 2021, 2)

#----------------------------------------------------------------
# Define model parameter values 
#----------------------------------------------------------------

pars <- list(
  # P-model
  kphio = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b = 1.0,
  soilm_thetastar = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim = 30.0,
  kc_jmax = 0.41,
  
  # Plant
  f_nretain = 0.500000,
  fpc_tree_max = 0.950000,
  growtheff = 0.600000,
  r_root = 2*0.913000,
  r_sapw = 2*0.044000,
  exurate = 0.003000,
  
  k_decay_leaf = 1.90000,
  k_decay_root = 1.90000,
  k_decay_labl = 1.90000,
  k_decay_sapw = 1.90000,
  
  r_cton_root = 37.0000,
  r_cton_wood = 100.000,
  r_cton_seed = 15.0000,
  nv_vcmax25 = 0.02 * 13681.77, # see ln_cn_review/vignettes/analysis_leafn_vcmax_field.Rmd, l.695; previously: 5000.0,
  ncw_min = 0.08 * 1.116222, # see ln_cn_review/vignettes/analysis_leafn_vcmax_field.Rmd, l.691; previously used: 0.056,
  r_n_cw_v = 0, # assumed that LMA is independent of Vcmax25; previously: 0.1,
  r_ctostructn_leaf = 1.3 * 45.84125, # see ln_cn_review/vignettes/analysis_leafn_vcmax_field.Rmd, l.699; previously used: 80.0000,
  kbeer = 0.500000,
  
  # Phenology (should be PFT-specific)
  gddbase = 5.0,
  ramp = 0.0,
  phentype = 2.0,
  
  # Soil physics (should be derived from params_soil, fsand, fclay, forg, fgravel)
  perc_k1 = 5.0,
  thdiff_wp = 0.2,
  thdiff_whc15 = 0.8,
  thdiff_fc = 0.4,
  forg = 0.01,
  wbwp = 0.029,
  por = 0.421,
  fsand = 0.82,
  fclay = 0.06,
  fsilt = 0.12,
  
  # Water and energy balance
  kA = 107,
  kalb_sw = 0.17,
  kalb_vis = 0.03,
  kb = 0.20,
  kc = 0.25,
  kCw = 1.05,
  kd = 0.50,
  ke = 0.0167,
  keps = 23.44,
  kWm = 220.0,
  kw = 0.26,
  komega = 283.0,
  maxmeltrate = 3.0,
  
  # Soil BGC
  klitt_af10 = 1.2,
  klitt_as10 = 0.35,
  klitt_bg10 = 0.35,
  kexu10 = 50.0,
  ksoil_fs10 = 0.021,
  ksoil_sl10 = 7.0e-04,
  ntoc_crit1 = 0.45,
  ntoc_crit2 = 0.76,
  cton_microb = 10.0,
  cton_soil = 9.77,
  fastfrac = 0.985,
  
  # N uptake
  eff_nup = 0.0001000,
  minimumcostfix = 1.000000,
  fixoptimum = 25.15000,
  a_param_fix = -3.62000,
  b_param_fix = 0.270000,
  
  # Inorganic N transformations (re-interpreted for simple ntransform model)
  maxnitr =  0.00005,
  
  # Inorganic N transformations for full ntransform model (not used in simple model)
  non = 0.01,
  n2on = 0.0005,
  kn = 83.0,
  kdoc = 17.0,
  docmax = 1.0,
  dnitr2n2o = 0.01,
  
  # Additional parameters - previously forgotten
  frac_leaf = 0.5,         # after wood allocation
  frac_wood = 0,           # highest priority in allocation
  frac_avl_labl = 0.1,
  
  # for development
  tmppar = 9999,
  
  # simple N uptake module parameters
  nuptake_kc = 600,
  nuptake_kv = 5,
  nuptake_vmax = 0.2
)

#----------------------------------------------------------------
# Run the model for these parameters using ch-oe1
#----------------------------------------------------------------

# Create output directories

# Define the full path
full_path <- "/Users/PhillipZywczuk/Documents/2023_2024_postdoc_eth/data/renku/n2o_ssa_lit_review/lit_review_modelling/p_model/data"

# Create the full path and all necessary parent directories
dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

# Create 'out' directory inside the specified full path
out_dir <- file.path(full_path, "out")
dir.create(out_dir, showWarnings = FALSE)

# Create 'vignettes' directory inside the specified full path
vignettes_dir <- file.path(full_path, "vignettes")
dir.create(vignettes_dir, showWarnings = FALSE)

# Create 'out' directory inside the 'vignettes' directory
vignettes_out_dir <- file.path(vignettes_dir, "out")
dir.create(vignettes_out_dir, showWarnings = FALSE)

# C-only run 
# Define whether to use interactive C-N cycling

# Function to add a new column 'c_only' with value TRUE to each dataframe
add_c_only_column <- function(df_list) {
  # Map over each dataframe in the list
  df_list <- map(df_list, ~ {
    # Add 'c_only' column with value TRUE
    mutate(.x, c_only = FALSE)
  })
  
  return(df_list)
}

# FLUXNET
ch0e2_drivers$params_siml <- add_c_only_column(ch0e2_drivers$params_siml)

#----------------------------------------------------------------
# Run CN model (FLUXNET DATA) 
#----------------------------------------------------------------

cnmodel_run_plot <- function(drivers, pars, df_name) {
  # Run the model
  output <- runread_cnmodel_f(drivers, par = pars)
  
  # Extract data
  model_data <- output$data[[1]] %>% as_tibble()
  
  #### LAI -----------------------------
  gg1 <- model_data %>%
    ggplot(aes(date, lai)) + 
    geom_line() +
    labs(x = "", y = expression(paste("LAI (m"^2, " m"^-2, ")"))) +
    theme_classic()
  
  #### GPP -----------------------------
  gg2 <- model_data %>%
    ggplot(aes(date, gpp)) + 
    geom_line() +
    labs(x = "", y = expression(paste("GPP (gC m"^-2, " d"^-1, ")"))) +
    theme_classic()
  
  #### NEP -----------------------------
  gg3 <- model_data %>%
    ggplot(aes(date, gpp - rleaf - rwood - rroot - rhet)) + 
    geom_line() +
    labs(x = "", y = expression(paste("NEP (gC m"^-2, " d"^-1, ")"))) +
    theme_classic()
  
  #### cumulative NEP -----------------------------
  gg4 <- model_data %>%
    ggplot(aes(date, cumsum(gpp - rleaf - rwood - rroot - rhet - rgrow))) + 
    geom_line() +
    labs(x = "", y = expression(paste("Cum. NEP (gC m"^-2, ")"))) +
    theme_classic()
  
  # Combine plots
  combined_plot <- plot_grid(
    gg1,
    gg2,
    gg3,
    gg4,
    ncol = 1
  )
  
  # Construct the filename using the dataframe name
  filename <- paste0(df_name, ".png")
  
  # Save the plot in the 'output' folder of the repository
  ggsave(filename = here("cnmodel_test_workflow", "output", filename), plot = combined_plot, height = 12, width = 8, dpi = 300)
  message("Plot saved successfully: ", here("cnmodel_test_workflow", "output", filename))
  
  # Display the plot
  print(combined_plot)
}

# Call the function with the updated parameters
cnmodel_run_plot(ch0e2_drivers, pars, "CH-0E2_NTRUE")
