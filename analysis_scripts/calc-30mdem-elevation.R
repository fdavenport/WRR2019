library(dplyr)
library(raster)
library(velox)

source("./input_file_paths.R")
source("./parameters.R")
source("./functions.R")

args <- commandArgs(trailingOnly = TRUE)
ARRAY_ID <- as.numeric(args[1])
N_JOBS <- as.numeric(args[2])

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
total_sites <- nrow(boundaries)

## find index of sites for this job within the array
if(ARRAY_ID == 1){
    site_index <- 1:floor(ARRAY_ID*(total_sites/N_JOBS))
} else {
    site_index <- ceiling((ARRAY_ID-1)*(total_sites/N_JOBS)):floor(ARRAY_ID*(total_sites/N_JOBS))
}
nsites <- length(site_index)

bound_latlon <- spTransform(boundaries[site_index,], CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
nsites <- nrow(bound_latlon)

## -----------------------------------------------------------------------------
## extract elevation within each basin using 30M DEM

basin_bbox <- lapply(1:nsites, function(x) bbox(bound_latlon[x,]))

for(i in 1:nsites){
    print(i)
    print(bound_latlon$GAGE_ID[i])

    ## find which tiles overlap with watershed
    ## tiles are 1deg by 1deg
    lons <- abs(floor(basin_bbox[[i]]["x",1])):abs(floor(basin_bbox[[i]]["x",2]))
    lats <- ceiling(basin_bbox[[i]]["y",1]):ceiling(basin_bbox[[i]]["y",2])

    tile_coords <- expand.grid(lon = lons, lat = lats)
    tiles <-  paste0(dem_path, "/grdn", tile_coords$lat, "w", str_pad(tile_coords$lon, 3, "left", "0"), "_1/w001001.adf")
    ## check which tiles exist (not all tiles within bbox necessarily exist)
    missing_tiles <- which(!file.exists(tiles))
    if(length(missing_tiles) > 0) {
        tiles <- tiles[-missing_tiles]
    }

    ## read and combine tiles
    if(length(tiles) > 1){
        elev_30m <- velox(do.call(merge, lapply(tiles, function(x) raster(x))))
    } else {
        elev_30m <- velox(raster(tiles))
    }

    basin_elev <- elev_30m$extract(sp = bound_latlon[i,], legacy = TRUE)[[1]]
    saveRDS(basin_elev, paste0("../processed_data/elev_30mDEM/",
                               bound_latlon$GAGE_ID[i], "_elevation_30m.Rds"))

}
