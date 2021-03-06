library(dplyr)
library(data.table)

source("./functions.R")
source("./input_file_paths.R")

## THIS SCRIPT:
## Saves daily basin values by site so they can be read into memory one at a time


## INPUT FILES: ----------------------------------------------------------------
daily_value_files <- paste0("../processed_data/NLDAS/basin_daily_averages/",
                           1979:2017, "dailyBasinValues.Rds")

## OUTPUT FILES: ---------------------------------------------------------------
out_folder <- "../processed_data/NLDAS/basin_daily_averages_by_site/"

sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))

nSites <- nrow(sites)
daily_basin_values <- lapply(daily_value_files, readRDS)

for(i in 1:nSites){
    if (i %% 10 == 0) print(i)
    basin_data_list <- lapply(daily_basin_values,
                               function(x) {
                                   as.data.frame(x[sites$STAID[i],,]) %>%
                                       mutate(date = dimnames(x)[[2]]) })
    basin_df <- rbindlist(basin_data_list)

    write.csv(basin_df, paste0(out_folder, sites$STAID[i], "_dailyValues.txt"), row.names = F)
}
