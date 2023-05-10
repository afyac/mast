# load library
pacman::p_load(httr,
               progress,
               tidyverse)

source(here::here("R/swalim_web_scrape/", "swalim_climate.R"))

# get district codes
id_df <- readRDS(here::here("R/swalim_web_scrape/", "id_df.rds"))

# download climate data for the years 2015 to 2023
climate_data_SOM <- swalim_climate(years = 2015:2023)
