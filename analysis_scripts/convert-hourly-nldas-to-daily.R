library(dplyr)
library(stringr)
library(tidyr)
library(sp)
library(raster)

## THIS SCRIPT: ----------------------------------------------------------------
## Reads in hourly NLDAS files and saves select variables as daily raster files
## One year at a time (year provided as argument on command line)
##
## Note: Aggregation method depends on variable (see function inputs)
## -----------------------------------------------------------------------------

source("./parameters.R")
source("./functions.R")
source("./input_file_paths.R")

## Input (from command line) ---------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
YEAR <- as.numeric(args[1])
print("year:")
print(YEAR)

## -----------------------------------------------------------------------------

## define variables to aggregate
fora_vars <-  c("TMP", "APCP")
vic_vars <- c("ASNOW", "ARAIN", "SNOM", "WEASD", "SOILM5")

## create vector of dates within current year and between start-end dates
days <- seq.Date(as.Date(as.character(nldas_start_day), format = "%Y%m%d"),
                 as.Date(as.character(nldas_end_day), format = "%Y%m%d"),
                 "days")
days <- days[which(strftime(days, "%Y") == YEAR)]

## loop through all days and aggregate from hour to day
for(i in seq_along(days)){
    dt <- gsub("-", "", days[i])
    cat(sprintf("date = %s\n", dt))

    fora_outfile <- paste0(nldas_FORA_D_path, "FORA0125_D.A", dt, ".grd")
    fora_data <- hrGRIB2dayRaster(nldas_FORA_H_path,
                                  "NLDAS_FORA0125_H.A", dt, "FORA",
                                 vars = fora_vars,
                                 fun = c("mean", "sum"),
                                 outdirectory = "../processed_data/NLDAS/FORA0125_D/FORA0125_D.A")

    vic_outfile <- paste0(nldas_VIC_D_path, "VIC0125_D.A", dt, ".grd")
    vic_data <- hrGRIB2dayRaster(nldas_VIC_H_path,
                                 "NLDAS_VIC0125_H.A", dt, "VIC",
                                vars = vic_vars,
                                fun = c("sum", "sum", "sum", "inst", "mean"),
                                outdirectory = "../processed_data/NLDAS/VIC0125_D/VIC0125_D.A")
}
