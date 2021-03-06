library(tidyverse)
library(ncdf4)
library(raster)
library(stringr)
library(velox)

## THIS SCRIPT:-----------------------------------------------------------------
## Reads in basin shapefiles and STAID list
## Extracts basin variables from monthly NLDAS for set of years/months

source("./functions.R")
source("./input_file_paths.R")
source("./parameters.R")

args <- commandArgs(trailingOnly = TRUE)
YEAR <- as.numeric(args[1])
print("year:")
print(YEAR)

## FILEPATHS -------------------------------------------------------------------
outfile <- paste0("../processed_data/NLDAS/basin_monthly_averages/monthly_nldas_",
                  YEAR, ".Rds")

##------------------------------------------------------------------------------
## READ IN SITE LIST AND WATERSHED BOUNDARIES-----------------------------------
sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))

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


##------------------------------------------------------------------------------
## READ IN DATA FOR ALL YEARS --------------------------------------------------

nyears <- length(YEAR)
mon <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08", "09")
vic_vars <- c("ASNOW", "ARAIN")
fora_vars <- c("TMP", "APCP")
nvars <- length(vic_vars) + length(fora_vars)

monthly_vic_data <- extractNLDASmonGRIB(bound_transform,
                                        filepath = nldas_VIC_M_path, prefix = "NLDAS_VIC0125_M.A",
                                        startYear = YEAR, endYear = YEAR,
                                      model = "VIC",
                                      mon = mon,
                                      vars = vic_vars, res = 4)


monthly_fora_data <- extractNLDASmonGRIB(bound_transform,
                                         filepath = nldas_FORA_M_path,
                                         prefix = "NLDAS_FORA0125_M.A",
                                          startYear = YEAR, endYear = YEAR,
                                       model = "FORA",
                                       mon = mon,
                                       vars = fora_vars, res = 4)

monthly_data <- array(dim = list(length(sites$STAID),
                                 nyears,
                                 length(mon),
                                 length(c(vic_vars, fora_vars))))

## -----------------------------------------------------------------------------
## COMBINE AND SAVE
monthly_data[,,,c(1, length(fora_vars))] <- monthly_fora_data
monthly_data[,,,c(length(fora_vars)+1,nvars)] <- monthly_vic_data

dimnames(monthly_data) <- list(dimnames(monthly_fora_data)[[1]],
                               dimnames(monthly_fora_data)[[2]],
                               dimnames(monthly_fora_data)[[3]],
                               c(dimnames(monthly_fora_data)[[4]],
                                 dimnames(monthly_vic_data)[[4]]))


saveRDS(monthly_data, outfile)


