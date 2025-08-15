## This script is designed to be a easily customized L1 data retrieval script.
## User-defined inputs are 1) date range, and 2) analytes of interest
## Currently available L1 datasets are listed below
## 
## 2025-07-15
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Set up environment
library(devtools)
source("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/scripts/0_setup.R")


# 0. User inputs - CHANGE THESE TO GET THE DATA YOU WANT! ----------------------

## Put all sensors you want to pull. Options include: 
### tide_gauge (water level)
### adcp (water velocity)
### ctd (water temp, salinity) ## DO not available as L1
### air_temp ## other met variables not available as L1
### windspeed (wind direction, average windspeed, max windspeed)

parameters_of_interest <- c("tide_gauge", "adcp", "ctd", "air_temp")

## Define your start and end dates in format YYYY-MM-DD
start_date = "2024-06-01"
end_date = "2024-11-01"

## Output directory - this determines where your file is written to
output_path = "L1_outputs/"


## Read in all potential datasets
tide_gauge <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250515_tidegauge_L1.csv") %>% 
  dplyr::select(-contains("tide"))
adcp <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250703_adcp_velocity_L1.csv")
ctd <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250630_ctd_water_temp_salinity_L1.csv")
air_temp <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250630_air_temp_L1.csv")
windspeed <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250630_windspeed_L1.csv")


# 2. Select and subset datasets based on user inputs ---------------------------

## Combine datasets based on parameters_of_interest
datasets <- list(tide_gauge = tide_gauge,
  adcp = adcp,
  ctd = ctd,
  air_temp = air_temp,
  windspeed = windspeed)

## Filter datasets to include only those in the parameters_of_interest
selected_datasets <- datasets[parameters_of_interest]

## Perform full_join across all selected datasets
df <- reduce(selected_datasets, function(x, y) full_join(x, y, by = "time_pst")) %>% 
  filter(time_pst >= start_date & time_pst <= end_date)

## Print the resulting tibble
print(df)


# 3. Write out your shiny new L1 dataset ---------------------------------------

## Format user inputs into condensed formats for filename
today <- format(Sys.Date(), "%y%m%d")
parameters_collapsed <- paste(parameters_of_interest, collapse = "_")
start <- format(as.Date(start_date), "%y%m%d")
end <- format(as.Date(end_date), "%y%m%d")
filename <- paste0(output_path, 
                   today, "_", 
                   parameters_collapsed, "_", 
                   start, "_", 
                   end, ".csv")

write_csv(df, filename)


