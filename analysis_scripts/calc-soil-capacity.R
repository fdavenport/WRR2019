library(dplyr)
library(raster)

source("./parameters.R")
source("./input_file_paths.R")
source("./functions.R")

sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))

boundaries <- rbind(shapefile(ref_boundary_file), shapefile(nonref_boundary_file1))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file2))
boundaries <- rbind(boundaries, shapefile(nonref_boundary_file3))
boundaries <- boundaries[which(boundaries$GAGE_ID %in% sites$STAID),]
## reorder boundaries to match order of sites ##
boundaries <- boundaries[match(sites$STAID, boundaries$GAGE_ID),]
nldas_crs <- CRS("+proj=longlat +a=6371200 +b=6371200 +no_defs")
bound_transform <- spTransform(boundaries, nldas_crs)

## -----------------------------------------------------------------------------
## READ IN SOIL DATA AND CALCULATE STORAGE

soil_depth1 <- raster(nldas_soil_1depth)
soil_por1 <- raster(nldas_soil_1por)

soil_depth2 <- raster(nldas_soil_2depth)
soil_por2 <- raster(nldas_soil_2por)

soil_depth3 <- raster(nldas_soil_3depth)
soil_por3 <- raster(nldas_soil_3por)

soil_cap1 <- calc(stack(soil_depth1, soil_por1), fun = prod)
soil_cap2 <- calc(stack(soil_depth2, soil_por2), fun = prod)
soil_cap3 <- calc(stack(soil_depth3, soil_por3), fun = prod)

soil_cap_tot <- calc(stack(soil_cap1, soil_cap2, soil_cap3), fun = sum)


basin_soil_cap <- extract(disaggregate(soil_cap_tot, 4),
                          bound_transform,
                          fun = mean, na.rm = TRUE)

soil_cap_df <- data.frame(STAID = bound_transform$GAGE_ID,
                          soil_cap_m = basin_soil_cap)

saveRDS(soil_cap_df, "../processed_data/basin_soil_capacity.Rds")
