library(dplyr)
library(data.table)
library(foreach)
library(doParallel)
source("./input_file_paths.R")
source("./functions.R")
source("./parameters.R")

args <- commandArgs(trailingOnly = TRUE)
nWorkers <- as.numeric(args[1])
if(is.na(nWorkers)) nWorkers <- 1

## -----------------------------------------------------------------------------
## FILEPATHS
daily_values_folder <- "../processed_data/NLDAS/basin_daily_averages_by_site/"
peak_folder <- "../processed_data/streamflow_peaks/"
outfile <- "../processed_data/streamflow_event_data.Rds"
outfile_csv <- "../processed_data/streamflow_event_data.csv"

## -----------------------------------------------------------------------------
sites <- read.csv(site_file) %>%
    mutate(STAint = x,
           STAID = sta2char(STAint))
nSites <- nrow(sites)

gagesIIdata <- readRDS("../processed_data/gagesIIdata.Rds") %>%
    mutate(STAID = sta2char(STAID)) %>%
    filter(STAID %in% sites$STAID)
rpp_stats <- readRDS("../processed_data/basin_rpp_stats.Rds")

peakData <- rbindlist(lapply(1:nrow(sites),
                             function(x) read.csv(paste0(peak_folder,
                                                         sites$STAID[x],"_",
                                                         flow_window,
                                                         "_MeanPeaks.txt")))) %>%
    dplyr::rename(peak_va = flow)

## -----------------------------------------------------------------------------

varNames <- c("STAID", "date",
              "peakflow_cfs", ## event peak streamflow
              "peakflow_cfs_std_by_area_km", ## flow standardized by drainage area
              "peakflow_cfs_std_by_area_km_precip_cm", ## flow standardized by DA and mean precip
              "precip_1day", "precip_3day", "precip_7day", ## cumulative precip
              "snow_precip_pct_7day", ## snow percentage of total precip
              "soilm_1day", "soilm_7day", ## soil moisture
              "SWE", ## Snow water equivalent
              "delta_SWE") ##  net change in SWE over 7 days
nDays <- 8
goodYears <- start_year:end_year ## list of water years included (not calendar years)

packageList <- c("dplyr", "data.table")
c1 <- makeCluster(nWorkers, outfile = "")
registerDoParallel(c1)
print(getDoParWorkers())
print(getDoParName())

variableList <- foreach(i=1:nSites, .inorder = TRUE, .packages = packageList) %dopar% {
    ## read in peaks for this site
    sitePeaks <- subset(peakData, site_no %in% sites$STAint[i]) %>%
        mutate(peak_dt  = as.Date(peak_dt))
    ##find water year for each peak
    peakMon <- strftime(sitePeaks$peak_dt, "%m")
    peakYear <- ifelse(peakMon >=10,
                       as.character(as.numeric(strftime(sitePeaks$peak_dt, "%Y"))+1),
                       strftime(sitePeaks$peak_dt, "%Y"))
    sitePeaks <- sitePeaks[which(peakYear %in% goodYears),]
    peakYear <- peakYear[which(peakYear %in% goodYears)]
    peakMon <- peakMon[which(peakYear %in% goodYears)]

    nPeaks <- nrow(sitePeaks)
    mean_ppt <- unique(subset(rpp_stats, STAID == sites$STAID[i])$mean_ppt)/10 # mm to cm
    DA <- gagesIIdata[which(gagesIIdata$STAID == sites$STAID[i]),"DRAIN_SQKM"]

    print(sites$STAint[i])
    if(nPeaks <= 1) { ## remaining analysis does not work if only one peak,
                      ## those sites won't be used anyways
        peakVarDF <- NULL
    } else {
        dateMatrix <- t(mapply(function(x) seq.Date(sitePeaks$peak_dt[x] - (nDays-1),
                                                    sitePeaks$peak_dt[x], 1), 1:nPeaks))

        ## read in daily basin variables for this site
        dailyVarDF <- read.csv(paste0(daily_values_folder, sites$STAID[i],
                                      "_dailyValues.txt")) %>%
            mutate(date = as.Date(date))

        rownames(dailyVarDF) <- dailyVarDF$date

        results <- apply(dateMatrix, c(1, 2), function(x)
            as.numeric(dailyVarDF[which(dailyVarDF$date ==as.Date(x, "1970-01-01")),]))
        results <- aperm(results, c(2, 3, 1))
        dimnames(results) <- list(as.character(sitePeaks$peak_dt), NULL, names(dailyVarDF))

        ## create variable DF for this site
        peakVarDF <- data.frame(matrix(NA, nrow = nPeaks, ncol = length(varNames),
                                       dimnames = list(NULL, varNames)))

        peakVarDF[,"STAID"] <- sites$STAID[i]
        peakVarDF[,c("date", "peakflow_cfs")] <- sitePeaks[,c("peak_dt", "peak_va")]
        peakVarDF$date[] <- sitePeaks$peak_dt
        peakVarDF[,"peakflow_cfs_std_by_area_km"] <- sitePeaks[,"peak_va"]/DA
        peakVarDF[,"peakflow_cfs_std_by_area_km_precip_cm"] <- peakVarDF[, "peakflow_cfs_std_by_area_km"]/mean_ppt

        peakVarDF[,"precip_1day"] <- rowSums(results[,nDays,c("ASNOW","ARAIN")])
        peakVarDF[,"precip_3day"] <- rowSums(results[,(nDays-2):nDays,c("ASNOW","ARAIN")])
        peakVarDF[,"precip_7day"] <- rowSums(results[,(nDays-6):nDays,c("ASNOW","ARAIN")])
        peakVarDF[,"snow_precip_pct_7day"] <- (rowSums(results[,(nDays-6):nDays,"ASNOW"])/
                                   rowSums(results[,(nDays-6):nDays,c("ASNOW","ARAIN")])*100)
        peakVarDF[,"soilm_1day"] <- results[,nDays,"SOILM5"]
        peakVarDF[,"soilm_7day"] <- rowMeans(results[,(nDays-6):nDays,"SOILM5"])
        peakVarDF[,"SWE"] <- results[,nDays-1,"WEASD"]
        peakVarDF[,"delta_SWE"] <- results[,nDays,"WEASD"]-results[,nDays-6,"WEASD"]

        peakVarDF$date <- as.Date(peakVarDF$date, origin = "1970-01-01")

        peakVarDF ## return data
    }
}

if (length(which(sapply(variableList, is.null))) > 0) {
    variableList <- variableList[-which(sapply(variableList, is.null))]
}
variableDF <- rbindlist(variableList)
saveRDS(variableDF, file = outfile)
write.csv(variableDF, outfile_csv)

stopCluster(c1)
