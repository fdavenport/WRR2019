library(tidyverse)
source("./input_file_paths.R")
source("./parameters.R")
source("./functions.R")

## -----------------------------------------------------------------------------
## THIS SCRIPT:
## Calculate summary stats for elevation in each basin based on
## NLDAS grid cell elevation and 30m DEM

## -----------------------------------------------------------------------------
## function to calculate elevation statistics for each basin
get_elev_stats <- function(grid_elev){
    ## grid_elev = vector of grid cell elevations

    elev_mean <- mean(grid_elev)
    elev_sd <- sd(grid_elev)
    elev_10 <- quantile(grid_elev, 0.1)
    elev_25 <- quantile(grid_elev, 0.25)
    elev_75 <- quantile(grid_elev, 0.75)
    elev_90 <- quantile(grid_elev, 0.9)

    data.frame(cbind(elev_mean, elev_sd, elev_10, elev_25,
                     elev_75, elev_90)) %>%
        remove_rownames()
}
## -----------------------------------------------------------------------------
sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))
nsites <- nrow(sites)

all_elev_nldas <- readRDS("../processed_data/basin_elevation_nldas.Rds")

elev_summary <- vector('list', length = nsites)
for(i in 1:nsites){
    if(i %% 10 == 0) print(i)
    elev_30m <- readRDS(paste0("../processed_data/elev_30mDEM/",
                               sites$STAID[i],
                               "_elevation_30m.Rds"))

    elev_nldas <- unlist(all_elev_nldas[sites$STAID[i]])

    elev_summary[[i]] <- rbind(get_elev_stats(elev_30m),
                               get_elev_stats(elev_nldas)) %>%
        mutate(grid = c("dem_30m", "NLDAS"),
               STAID = sites$STAID[i])

}

elev_summary <- do.call(rbind, elev_summary)

saveRDS(elev_summary, "../processed_data/basin_elev_summary.Rds")
