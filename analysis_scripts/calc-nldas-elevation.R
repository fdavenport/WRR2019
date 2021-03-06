library(dplyr)
library(raster)

source("./input_file_paths.R")
source("./parameters.R")
source("./functions.R")

## FILES: ----------------------------------------------------------------------
## see param_file

sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))

boundaries <- rbind(shapefile(ref_boundary_file), shapefile(nonref_boundary_file1))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file2))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file3))
boundaries <- boundaries[which(boundaries$GAGE_ID %in% sites$STAID),]
## reorder boundaries to match order of sites ##
boundaries <- boundaries[match(sites$STAID, boundaries$GAGE_ID),]
nldasCRS <- CRS("+proj=longlat +a=6371200 +b=6371200 +no_defs")
bound_transform <- spTransform(boundaries, nldasCRS)
nsites <- nrow(bound_transform)

## -----------------------------------------------------------------------------
## extract elevation of grid points within each basin

elev_nldas <- raster(nldas_elev_file, varname = "NLDAS_elev")

basin_elev_nldas <- extract(disaggregate(elev_nldas, 4), bound_transform)
names(basin_elev_nldas) <- bound_transform$GAGE_ID

saveRDS(basin_elev_nldas, "../processed_data/basin_elevation_nldas.Rds")

