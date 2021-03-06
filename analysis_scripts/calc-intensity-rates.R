library(tidyverse)

source("./parameters.R")
source("./functions.R")
source("./input_file_paths.R")

daily_values_folder <- "../processed_data/NLDAS/basin_daily_averages_by_site/"

sites <- sta2char(read.csv(site_file)$x)
nsites <- length(sites)

rate_list <- vector('list', length = nsites)
for(i in 1:nsites){
    if(i %% 50 == 0) print(i)
    rate_list [[i]] <- read.csv(paste0(daily_values_folder, sites[i], "_dailyValues.txt")) %>%
        mutate(date = as.Date(date)) %>%
        dplyr::select(date, APCP, SNOM) %>%
        mutate(STAID = sites[i])
}

names(rate_list) <- sites
saveRDS(rate_list, "../processed_data/daily_precip_snowmelt_rates.Rds")
