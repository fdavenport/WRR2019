library(dplyr)
library(data.table)
library(foreach)
library(doParallel)
source("./input_file_paths.R")
source("./functions.R")
source("./parameters.R")

args <- commandArgs(trailingOnly = TRUE)
nWorkers <- as.numeric(args[1])
event_var <- args[2]
if(is.na(nWorkers)) nWorkers <- 1

## FILEPATHS:  -----------------------------------------------------------------

daily_values_folder <- "../processed_data/NLDAS/basin_daily_averages_by_site/"

event_list <- paste0("../processed_data/", event_length, "day_", event_var,
                     "_events_peakflow_", event_window, "dayWindow.Rds")
outfile <- paste0("../processed_data/", event_var, "_event_data.Rds")
outfile_csv<- paste0("../processed_data/", event_var, "_event_data.csv")

## -----------------------------------------------------------------------------
sites <- read.csv(site_file) %>%
    mutate(STAint = x,
           STAID = sta2char(STAint))
nSites <- nrow(sites)

rpp_stats <- readRDS("../processed_data/basin_rpp_stats.Rds")
soil_cap_df <- readRDS("../processed_data/basin_soil_capacity.Rds")
gagesIIdata <- readRDS("../processed_data/gagesIIdata.Rds") %>%
    mutate(STAID = sta2char(STAID)) %>%
    filter(STAID %in% sites$STAID)

event_data <- readRDS(event_list)

packageList <- c("dplyr")
c1 <- makeCluster(nWorkers, outfile = "")
registerDoParallel(c1)
print(getDoParWorkers())
print(getDoParName())

variableList <- foreach(i=1:nSites, .inorder = TRUE, .packages = packageList) %dopar% {

    DA <- gagesIIdata[which(gagesIIdata$STAID == sites$STAID[i]),"DRAIN_SQKM"]
    soil_cap <- soil_cap_df[which(soil_cap_df$STAID == sites$STAID[i]),"soil_cap_m"]
    mean_ppt <- unique(subset(rpp_stats, STAID == sites$STAID[i])$mean_ppt)/10 ## mm to cm
    siteEvents <- event_data[[sites$STAID[i]]]

    ## read in daily Basin Values
    dailyVarDF <- read.csv(paste0(daily_values_folder, sites$STAID[i],
                                  "_dailyValues.txt")) %>%
        mutate(date = as.Date(date))

    ## create matrix for each day within PPT event
    dateMatrix <- t(mapply(function(x) seq.Date(siteEvents$Date[x] - (event_length),
                                                siteEvents$Date[x], 1), 1:nrow(siteEvents)))

    ## define column indices to subset results based on values before, during or at the end of each event
    eventInd <- 2:(event_length+1)
    endInd <- event_length+1
    preInd <- 1

    ## matrix of basin values for each date within PPT event
    results <- apply(dateMatrix, c(1, 2), function(x)
        as.numeric(dailyVarDF[which(dailyVarDF$date ==as.Date(x, "1970-01-01")),]))
    results <- aperm(results, c(2, 3, 1))
    dimnames(results) <- list(as.character(siteEvents$Date), NULL, names(dailyVarDF))

    siteEvents <- siteEvents %>%
        mutate(peakmon = strftime(Date, "%m")) %>%
        mutate(streamflow_std_by_area= streamflow/DA) %>%
        mutate(streamflow_std_by_area_precip = streamflow_std_by_area/mean_ppt) %>%
        mutate(ARAIN = rowSums(results[,eventInd,"ARAIN"]),
               ASNOW = rowSums(results[,eventInd,"ASNOW"]),
               snow_precip_pct = (rowSums(results[,eventInd,"ASNOW"])/
                      rowSums(results[,eventInd,c("ASNOW","ARAIN")])*100)) %>%
        mutate(rain_precip_pct = 100-snow_precip_pct) %>%
        mutate(SOILM = rowMeans(results[,eventInd,"SOILM5"])) %>%
        mutate(SOILMpre = results[,preInd,"SOILM5"],
               SOILM_AVAIL = soil_cap*1000 - SOILMpre) %>%
        mutate(SWE = results[,endInd,"WEASD"]) %>%
        mutate(delta_SWE = results[,endInd,"WEASD"]-results[,1,"WEASD"]) %>%
        mutate(SWE_melt = ifelse(delta_SWE >= 0, 0, -delta_SWE)) %>%
        ## rain percentage of 'precipitation + snowmelt'
        mutate(rain_pct = ARAIN/(ARAIN + ASNOW + SWE_melt)*100)

    if(event_var == "precip") {
        siteEvents <- siteEvents %>%
            dplyr::rename(precip = value)
    } else if(event_var == "precip_snom"){
        siteEvents <- siteEvents %>%
            dplyr::rename(precip_snom = value)
    }

    return(siteEvents)
}

variableDF <- rbindlist(variableList)
saveRDS(variableDF, file = outfile)
write.csv(variableDF, file = outfile_csv)

stopCluster(c1)
