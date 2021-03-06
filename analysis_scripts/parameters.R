## -----------------------------------------------------------------------------
## SET PARAMETERS FOR DATA PROCESSING
## -----------------------------------------------------------------------------

## NLDAS DATE RANGE
nldas_start_day <- 19790102
nldas_end_day <- 20171231

## PROJECT DATE PARAMETERS
start_year <- 1980
end_year <- 2016
start_date <- as.Date(paste0(start_year-1, "-10-01"))
end_date <- as.Date(paste0(end_year, "-09-30"))
NDJFM <- c("11", "12", "01", "02", "03")

## STREAMFLOW PEAK PARAMETERS
flow_window <- 8
min_flow_thr <- 0.5 ## 50th percentile, i.e. peak must exceed median

## PRECIP and SNOWMELT EVENT PARAMETERS
event_length <- 3 ## number of days for precip/snowmelt event
event_window <- 8 ## number of days between precip/snowmelt events
flow_lag <- 5 ## maximum lag for flow after precip/snowmelt events
