library(tidyverse)
library(sp)
library(raster)
source("./parameters.R")
source("./input_file_paths.R")

stations <- read.csv(snotel_site_file) %>%
    separate(site_name, into = c("site_name", "site_num"), sep = " \\(") %>%
    mutate(site_num = gsub("\\)", "", site_num))

snotel_files <- list.files(snotel_path, pattern = "snotel_data",
                           full.names = TRUE)

## READ SNOTEL DATA
snotel_data <- do.call(rbind, lapply(snotel_files, read.csv)) %>%
    mutate(site_num = as.character(Station.Id)) %>%
    merge(stations, by = "site_num") %>%
    subset(state != "AK") %>%
    mutate(Date = as.character(Date))
## NOTE: Snolite stations are dropped during merge

snotel_position <- dplyr::select(snotel_data, Station.Name, site_num, lat, lon) %>%
    distinct()
## create shapefile of station locations
snotel_sp <- SpatialPointsDataFrame(snotel_position[,c("lon", "lat")],
                                    snotel_position[,-c(3,4)])
saveRDS(snotel_sp, "../processed_data/snotel_SPDF.Rds")

## read each day of VIC and calculate SWE at grid point of station
date_seq <- seq(as.Date(as.character(nldas_start_day), format = "%Y%m%d"),
                as.Date(as.character(nldas_end_day), format = "%Y%m%d"), 1)

vic_swe <- vector('list', length = length(date_seq))

## find swe layer in raster
tmp <- brick("../processed_data/NLDAS/VIC0125_D/VIC0125_D.A19810101.grd")
layer_num <- which(names(tmp) == "WEASD")

for(i in seq_along(date_seq)){
    dt <- gsub("-", "", date_seq[i])
    ## WEASD is layer 7
    dt_raster <- raster(paste0("../processed_data/NLDAS/VIC0125_D/VIC0125_D.A",
                               dt, ".grd"), band = layer_num)
    if (names(dt_raster) != "WEASD") {
        stop("user added exception: reading wrong layer from raster")
    }

    station_swe <- raster::extract(dt_raster, snotel_sp)
    ## Note: add 1 to vic date to get start of next day
    vic_swe[[i]] <- data.frame(site_num = snotel_sp$site_num,
                               Date = as.character(date_seq[i] + 1),
                               vic_swe = station_swe)

    if (i %% 100 == 0) print(i)
}

vic_swe <- do.call(rbind, vic_swe) %>%
    mutate(site_num = as.character(site_num),
           Date = as.character(Date))

combined_data <- left_join(vic_swe, snotel_data, by = c("site_num", "Date"))

saveRDS(combined_data, "../processed_data/snotel_station_SWE.Rds")
