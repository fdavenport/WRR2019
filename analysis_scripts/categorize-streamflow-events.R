library(tidyverse)
source("./input_file_paths.R")
source("./functions.R")
source("./parameters.R")

flow_event_df <- readRDS("../processed_data/streamflow_event_data.Rds")


## -----------------------------------------------------------------------------
## streamflow event categorization
cats <- c("Rain", "Snowmelt", "ROS", "unclassified") ## categories
cat_thr = 10 ##threshold (in mm) for Rain and Snowmelt categories
## ROS is defined by both Rain and Snowmelt (negative delta_SWE) exceeding cat_thr/2
## See Methods in paper for more details

flow_event_df <- flow_event_df %>%
    mutate(arain = ifelse(precip_7day==0, 0, precip_7day*(1-snow_precip_pct_7day/100)),
           asnow = ifelse(precip_7day==0, 0, precip_7day*snow_precip_pct_7day/100),
           SWE_melt = ifelse(delta_SWE < 0, -delta_SWE, 0)) %>%
    mutate(Snowmelt = ifelse(arain < cat_thr/2 & SWE_melt >= cat_thr, 1, 0),
           Rain = ifelse(arain >= cat_thr & SWE_melt < cat_thr/2, 1, 0),
           ROS = ifelse(arain >= cat_thr/2 & SWE_melt >= cat_thr/2, 1, 0),
           unclassified = ifelse(Snowmelt+Rain+ROS == 0, 1, 0)) %>%
    gather(category, dummy, cats) %>%
    subset(dummy == 1) %>%
    dplyr::select(STAID, date, peakflow_cfs, peakflow_cfs_std_by_area_km, category)

saveRDS(flow_event_df, "../processed_data/streamflow_event_categories.Rds")
write.csv(flow_event_df, "../processed_data/streamflow_event_categories.csv")
