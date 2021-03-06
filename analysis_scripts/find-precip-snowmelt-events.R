library(dplyr)
library(data.table)
library(zoo)
library(ggplot2)

source("./parameters.R")
source("./input_file_paths.R")
source("./functions.R")

## THIS SCRIPT: ----------------------------------------------------------------
## Reads in daily NLDAS values for basins
## Identifies precipitation events and combined
## precipitation+snowmelt events ("runoff events")

## INPUT FILES: ----------------------------------------------------------------
daily_value_files <- paste0("../processed_data/NLDAS/basin_daily_averages/",
                           1979:2017, "dailyBasinValues.Rds")

## OUTPUT FILES: ---------------------------------------------------------------
precip_event_outfile <- paste0("../processed_data/", event_length, "day_precip_events_",
                               event_window, "dayWindow.Rds")
precip_snom_event_outfile <- paste0("../processed_data/", event_length,
                                    "day_precip_snom_events_",
                                    event_window, "dayWindow.Rds")

## -----------------------------------------------------------------------------

all_days <- seq.Date(as.Date(as.character(nldas_start_day), format = "%Y%m%d"),
                 as.Date(as.character(nldas_end_day), format = "%Y%m%d"),
                 "days")
good_dates <- seq.Date(as.Date(paste0(start_year-1, "-10-01")),
                       as.Date(paste0(end_year, "-09-30")), "days")

sites <- as.data.frame(read.csv(site_file)) %>%
    dplyr::rename(staInt = x) %>%
    mutate(STAID = sta2char(staInt))
nsites <- nrow(sites)

## read in daily basin values, extract APCP and convert to data.frame
daily_basin_values <- lapply(daily_value_files, readRDS)

## create daily SWE df
daily_swe_values <- as.data.frame(rbindlist(lapply(daily_basin_values,
                                                   function(mtx) as.data.frame(t(mtx[,,"WEASD"]))), fill = T))
rownames(daily_swe_values) <- as.character(all_days)

## create daily ppt df
daily_basin_values <- as.data.frame(rbindlist(lapply(daily_basin_values, function(mtx) as.data.frame(t(mtx[,,"APCP"]))), fill = T))
rownames(daily_basin_values) <- as.character(all_days)

## subset for good_dates
daily_basin_values <- daily_basin_values[as.character(good_dates),]
daily_swe_values <- daily_swe_values[as.character(good_dates),]

## calculate net SWE change over each three day period
swe_change <- apply(daily_swe_values, 2, function(x) x-lag(x, event_length))
swe_change <- apply(swe_change, 2, function(x) ifelse(is.na(x), 0,
                                             ifelse(x >= 0, 0, 0-x)))
swe_change <- swe_change[event_length:length(good_dates),]
rownames(swe_change) <- as.character(good_dates[event_length:length(good_dates)])


## set ppt = 0 for days that <1mm ppt
daily_ppt_thr <- 1
ppt_over_thr <- apply(daily_basin_values, 2, function(x) ifelse(x >= daily_ppt_thr, x, 0))

## calculate rolling PPT sum for each station
ppt_sums <- apply(ppt_over_thr, 2, function(x) rollsum(x, event_length, align = "right"))
rownames(ppt_sums) <- sumDates <- as.character(good_dates[event_length:length(good_dates)])

## combine ppt_sums and swe_change
precip_snom_sums <- ppt_sums + swe_change

## how many days of precipitation are there at each station?
nDaysPrecip <- sapply(1:nsites, function(x) length(which(ppt_over_thr[,x] > 0)))

## find ppt events for each station
all_ppt_events <- lapply(1:nsites,
                    function(x) {
                        findPrecipEvents(ppt_sums[,x], nEvents = "all", event_window) %>%
                            mutate(STAID = sites$STAint[x])})
names(all_ppt_events) <- sites$STAID
saveRDS(all_ppt_events, precip_event_outfile)

## find precip_snom events for each station (across all months!)
all_precip_snom_events <- lapply(1:nsites,
                    function(x) {
                        findPrecipEvents(precip_snom_sums[,x],
                                         nEvents = "all", event_window) %>%
                            mutate(STAID = sites$STAint[x])})
names(all_precip_snom_events) <- sites$STAID
saveRDS(all_precip_snom_events, precip_snom_event_outfile)
