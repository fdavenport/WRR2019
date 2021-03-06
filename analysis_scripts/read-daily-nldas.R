library(dplyr)
library(stringr)
library(tidyr)
library(sp)
library(raster)

## THIS SCRIPT: ----------------------------------------------------------------
## Reads in daily raster files
## Computes mean within basin boundaries
## Saves output
## One year at a time (year provided as arguement on command line)
## -----------------------------------------------------------------------------

source("./functions.R")
source("./input_file_paths.R")
source("./parameters.R")

args <- commandArgs(trailingOnly = TRUE)
YEAR <- as.numeric(args[1])
print("year:")
print(YEAR)

## -----------------------------------------------------------------------------
## FILENAMES:
outfile <- paste0("../processed_data/NLDAS/basin_daily_averages/",
                   YEAR, "dailyBasinValues.Rds")

## -----------------------------------------------------------------------------
days <- seq.Date(as.Date(as.character(nldas_start_day), format = "%Y%m%d"),
                 as.Date(as.character(nldas_end_day), format = "%Y%m%d"),
                 "days")

sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))

nsite <- nrow(sites)

boundaries <- rbind(shapefile(ref_boundary_file), shapefile(nonref_boundary_file1))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file2))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file3))
boundaries <- boundaries[which(boundaries$GAGE_ID %in% sites$STAID),]
## reorder boundaries to match order of sites ##
boundaries <- boundaries[match(sites$STAID, boundaries$GAGE_ID),]
usgsCRS <- crs(boundaries)
## transform watershed boundaries to nldas
nldasCRS <- CRS("+proj=longlat +a=6371200 +b=6371200 +no_defs")
bound_transform <- spTransform(boundaries, nldasCRS)

fora_vars <- c("TMP", "APCP")
model_vars <- c("ASNOW", "ARAIN", "SNOM", "SOILM5", "WEASD")

## define days and result matrix for current year
current_days <- days[which(strftime(days, "%Y") == YEAR)]
nDays <- length(current_days)

result_matrix <- array(, dim = c(nsite, nDays, length(c(fora_vars, model_vars))),
                       dimnames = list(sites$STAID,
                                       as.character(current_days),
                                       c(fora_vars, model_vars)))

for(j in seq_along(current_days)) {

    dt <- gsub("-", "", current_days[j])

    vic_data <- extract_raster_daily(bound_transform,
                                     "../processed_data/NLDAS/VIC0125_D/VIC0125_D.A",
                                     dt, dt,
                                     vars = model_vars, res = 4)

    fora_data <- extract_raster_daily(bound_transform,
                                      "../processed_data/NLDAS/FORA0125_D/FORA0125_D.A",
                                      dt, dt, vars = fora_vars, res = 4)
    result_matrix[,j,model_vars] <- vic_data
    result_matrix[,j,fora_vars] <- fora_data
    if((j%%50) == 0) saveRDS(result_matrix, outfile)
}

saveRDS(result_matrix, outfile)

