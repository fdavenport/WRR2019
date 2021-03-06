library(dplyr);
library(tidyr);

source("./input_file_paths.R")
source("./parameters.R")
source("./functions.R")

## THIS SCRIPT: ------------------------------------------------------------------
## Reads in site data from GAGES-II csv files
## Calculates additional statistics from GAGES-II files
## Saves as Rds object for future use

## OUTPUT FILES------------------------------------------------------------------
gagesII_outfile <- "../processed_data/gagesIIdata.Rds"

## -----------------------------------------------------------------------------

basin_id <- read.csv(basin_id_file) %>%
    dplyr::select(STAID, STANAME,
                  DRAIN_SQKM, HUC02,
                  LAT_GAGE, LNG_GAGE, STATE)

basin_class <- read.csv(basin_class_file) %>%
    dplyr::select(STAID, CLASS, AGGECOREGION,
                  HYDRO_DISTURB_INDX,
                  WR_REPORT_REMARKS,
                  SCREENING_COMMENTS)

climate <- read.csv(climate_file) %>%
    dplyr::select(STAID, PPTAVG_BASIN)

topo <- read.csv(topo_file) %>%
    dplyr::select(STAID, ELEV_MEAN_M_BASIN,
                  ELEV_SITE_M, RRMEDIAN,
                  SLOPE_PCT)

hydro_dam <- read.csv(hydro_dam_file) %>%
    dplyr::select(STAID, STOR_NID_2009)


## -----------------------------------------------------------------------------

gagesII_data <- basin_id %>%
    merge(basin_class, by = "STAID") %>%
    mutate(ncei_region = find_ncei_region(STATE)) %>%
    merge(climate, by = "STAID") %>%
    merge(topo, by = "STAID") %>%
    merge(hydro_dam, by = "STAID")

## -----------------------------------------------------------------------------

saveRDS(gagesII_data, file = gagesII_outfile)



