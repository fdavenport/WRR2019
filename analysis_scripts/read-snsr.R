library(tidyverse)
library(raster)
library(velox)
library(ncdf4)

source("./parameters.R")
source("./input_file_paths.R")
source("./functions.R")

## THIS SCRIPT:

## -----------------------------------------------------------------------------
## INPUTS (from command line)
args <- commandArgs(trailingOnly = TRUE)
YEAR <- as.numeric(args[1])
print(YEAR)

## -----------------------------------------------------------------------------
sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))
nSites <- nrow(sites)

gagesIIdata <- readRDS("../processed_data/gagesIIdata.Rds") %>%
    mutate(STAID = sta2char(STAID))

boundaries <- rbind(shapefile(ref_boundary_file), shapefile(nonref_boundary_file1))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file2))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file3))
boundaries <- boundaries[which(boundaries$GAGE_ID %in% sites$STAID),]
## reorder boundaries to match order of sites ##
boundaries <- boundaries[match(sites$STAID, boundaries$GAGE_ID),]
bound_transform <-  spTransform(boundaries,
                                CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
bound_transform$STAID <- sites
bound_transform <- bound_transform %>%
    merge(gagesIIdata[,c("STAID", "STATE")])
## -----------------------------------------------------------------------------
## read in static topo data
snsr_static <- nc_open(paste0(snsr_path, "SN_static_data.h5"))
snsr_lat <- ncvar_get(snsr_static, varid = "lat")
snsr_lon <- ncvar_get(snsr_static, varid = "lon")
snsr_DEM <- ncvar_get(snsr_static, varid = "DEM")
nc_close(snsr_static)
static_raster <- raster(snsr_DEM,  xmn = min(snsr_lon), xmx = max(snsr_lon),
                     ymn = min(snsr_lat), ymx = max(snsr_lat))
static_raster <- calc(static_raster, function(x) ifelse(x<0, NA, x))
## create Velox raster for overlap calculations, set cells inside coverage area to 1
static_velox <- velox(calc(static_raster, function(x) ifelse(is.na(x), NA, 1)))


## reduce to snsr domain
bound_transform <- bound_transform %>%
    subset(STATE %in% c("CA", "NV"))

## check for which watersheds overlap
bound_overlap <- static_velox$extract(sp = bound_transform)
overlap_sum <- sapply(bound_overlap,
                      function(x) sum(x, na.rm = TRUE))
overlap_index <- which(overlap_sum > 0)
bound_subset <- bound_transform[overlap_index,]
nSites <- nrow(bound_subset)

## -----------------------------------------------------------------------------

h5file <- paste0(snsr_path, "SN_SWE_WY", YEAR, ".h5")
temp_file <- nc_open(h5file)
nRow <- as.numeric((temp_file$dim)$phony_dim_2["len"])
nCol <- as.numeric((temp_file$dim)$phony_dim_1["len"])
nDays <- as.numeric((temp_file$dim)$phony_dim_0["len"])

result <- matrix(,nrow = nSites, ncol = nDays)
for(j in 1:nDays){
    if(j %% 50 == 0) print(j)
    tempSWE <- ncvar_get(temp_file, varid = "SWE", start = c(1, 1, j),
                         count = c(nRow, nCol, 1))
    tempRaster <- raster(tempSWE, xmn = min(snsr_lon), xmx = max(snsr_lon),
                         ymn = min(snsr_lat), ymx = max(snsr_lat))
    ## assume cells outside of raster have 0mm snow for averaging
    tempRaster <- velox(calc(tempRaster, function(x) ifelse(x<0, 0, x)))
    result[,j] <- tempRaster$extract(sp = bound_subset, fun = mean)
}

col_dates <- as.character(seq.Date(as.Date(paste0(YEAR-1, "-10-01")),
                             as.Date(paste0(YEAR, "-09-30")), by = 1))
colnames(result) <- col_dates
result_long <- as.data.frame(result) %>%
    mutate(STAID = bound_subset$GAGE_ID) %>%
    gather(date, snsr_swe, -STAID)

saveRDS(result_long, paste0("../processed_data/SNSR/basin_snsr_swe_", YEAR, ".Rds"))




