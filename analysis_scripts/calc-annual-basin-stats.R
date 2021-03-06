library(tidyverse)
library(trend)

source("./functions.R")
source("./input_file_paths.R")
source("./parameters.R")

sites <- sta2char(read.csv(site_file)$x)

## -----------------------------------------------------------------------------
## read and format monthly data
monthly_files <- list.files("../processed_data/NLDAS/basin_monthly_averages",
                            full.names = TRUE)
dims <- dim(readRDS(monthly_files[1]))
dnames <- dimnames(readRDS(monthly_files[1]))
monthly_data <- array(dim = c(dims[1], dims[2]*length(monthly_files),
                              dims[3], dims[4]))
dimnames(monthly_data) <- list(dnames[[1]],
                               start_year:end_year,
                               dnames[[3]],
                               dnames[[4]])
for(i in seq_along(monthly_files)){
    monthly_data[,i,,] <- readRDS(monthly_files[i])
}

## calculate annual NDJFM rain precipitation percentage for each basin
rpp_data <- vector('list', length = length(sites))

for(i in seq_along(sites)){
    site_data <- monthly_data[sites[i],,,]
    annual_rpp <- rowSums(site_data[,NDJFM,"ARAIN"])/
        rowSums(site_data[,NDJFM,c("ARAIN", "ASNOW")])*100
    mean_T <- mean(site_data[,NDJFM,"TMP"])
    mean_ppt <- mean(site_data[,,"APCP"])*12
    rpp_data[[i]] <- data.frame(year = start_year:end_year,
                                annual_rpp, mean_T, mean_ppt, STAID = sites[i])
}
rpp_data <- do.call(rbind, rpp_data)

## calculate mean NDJFM rain percentage and thiel-sen slope
rpp_summary <- rpp_data %>%
    group_by(STAID) %>%
    summarize(mean_rpp = mean(annual_rpp),
              mean_T = mean(mean_T),
              mean_ppt = mean(mean_ppt),
              sen_slope = sens.slope(annual_rpp)$estimates,
              trend_mag = sen_slope*length(annual_rpp),
              sen_pval = sens.slope(annual_rpp)$p.value)

## save statistics
saveRDS(rpp_summary, "../processed_data/basin_rpp_stats.Rds")






