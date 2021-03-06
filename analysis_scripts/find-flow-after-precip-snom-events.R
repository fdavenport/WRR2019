library(dplyr)
library(foreach)
library(doParallel)
source("./input_file_paths.R")
source("./functions.R")
source("./parameters.R")

findFlowbyDate <- function(xdate, flowData, flowColumn){
    vec1 <- sapply(1:length(xdate),
                   function(y) flowData[which(flowData$Date == xdate[y]), flowColumn])
    vec1 <- unlist(lapply(vec1, function(x) ifelse(length(x)==0, NA, x)))
}

## THIS CODE: ------------------------------------------------------------------
## Finds maximum streamflow following peak precipitation event
## -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
nWorkers <- as.numeric(args[1])
if(is.na(nWorkers)) nWorkers <- 1
event_var <- args[2]

## FILES: ----------------------------------------------------------------------
if(event_var == "precip"){
    eventFile <- paste0("../processed_data/", event_length, "day_precip_events_",
                        event_window, "dayWindow.Rds")

    outfile <- paste0("../processed_data/", event_length, "day_precip_events_peakflow_",
                        event_window, "dayWindow.Rds")

} else if (event_var == "precip_snom") {
    eventFile <- paste0("../processed_data/", event_length, "day_precip_snom_events_",
                        event_window, "dayWindow.Rds")
    outfile <- paste0("../processed_data/", event_length,
                      "day_precip_snom_events_peakflow_", event_window, "dayWindow.Rds")
}

## -----------------------------------------------------------------------------
event_data <- readRDS(eventFile)

sites <- read.csv(site_file) %>%
    mutate(STAID = sta2char(x))

packageList <- c("dplyr")
c1 <- makeCluster(nWorkers, outfile = "")
registerDoParallel(c1)
print(getDoParWorkers())
print(getDoParName())

eventList <- foreach(i=1:nrow(sites), .inorder = TRUE, .packages = packageList) %dopar% {
        print(i)
        flowData <- read.csv(paste0(daily_streamflow_folder, sites$STAID[i],
                                    "_dailyFlow.txt")) %>%
            mutate(Date = as.Date(Date))

        ## check for multiple streamgage records at site
        if(ncol(flowData) == 7) {
            ## gage with most recent data supercedes
            if (max(is.na(flowData[,4])) < max(is.na(flowData[,6]))) {
                flowData[,"X_00060_00003"] <- ifelse(!is.na(flowData[,4]),
                                                     flowData[,4], flowData[,6])
            } else {
                flowData[,"X_00060_00003"] <- ifelse(!is.na(flowData[,6]),
                                                     flowData[,4], flowData[,4])
            }
        }

        siteEvents <- event_data[[sites$STAID[i]]] %>%
            mutate(Date = as.Date(Date)) %>%
            mutate(endDate = Date+(event_window-1)) %>%
            filter(Date >= flowData$Date[1] & Date <=
                   flowData$Date[nrow(flowData)]-flow_lag)

        nEvents <- nrow(siteEvents)

        if(nEvents == 0) {
            siteEvents <- NULL
        } else {
            ## create date matrix for peak events
            dateMatrix <- t(mapply(function(x) seq.Date(siteEvents$Date[x]-2,
                                                        siteEvents$Date[x] + flow_lag,
                                                        1),
                                   1:nEvents))

            ## find flows within ppt window for each event
            flowMatrix <- matrix(findFlowbyDate(dateMatrix, flowData, "X_00060_00003"),
                                 ncol = (flow_lag+event_length))

            ## calculate flow before precip event
            preEventFlows <- findFlowbyDate(siteEvents$Date-3, flowData, "X_00060_00003")

            ## remove events with more than two missing days in flow record
            ## (for stations without continous data)
            missingEvents <- which(sapply(1:nrow(flowMatrix),
                                          function(x) {
                                              length(which(!is.na(flowMatrix[x,]))) < 6}))
            if(length(missingEvents) > 0){
                flowMatrix <- flowMatrix[-missingEvents,]
                siteEvents <- siteEvents[-missingEvents,]
                preEventFlows <- preEventFlows[-missingEvents]
            }

            print(head(preEventFlows))
            if(nrow(siteEvents) <= 1){
                siteEvents <- NULL
            } else {
                ## store max of flow and max lag day corresponding to peak event
                ## calculate min flow starting the day before event and leading
                ## up to the peak flow
                siteEvents <- siteEvents %>%
                    mutate(streamflow = apply(flowMatrix, 1,
                                              function(x) max(x, na.rm = TRUE))) %>%
                    mutate(peaklag = apply(flowMatrix, 1, which.max)-1) %>%
                    ## calculate minimum flow before peak
                    mutate(minFlow =
                               sapply(1:nrow(siteEvents),
                                      function(x) if(is.na(preEventFlows[x])){ NA
                                                  } else if(peaklag[x] != 0){
                                                      min(c(preEventFlows[x],
                                                            flowMatrix[x,1:peaklag[x]]),
                                                          na.rm = TRUE)
                                                  } else {preEventFlows[x]})) %>%
                     ## calculate delta_streamflow (max flow - min flow)
                    mutate(delta_streamflow = streamflow - minFlow,
                           STAID = sites$STAID[i])
            }
        }

        siteEvents
}

names(eventList) <- sites$STAID
if (length(which(sapply(eventList, is.null))) > 0) {
    eventList <- eventList[-which(sapply(eventList, is.null))]
}
saveRDS(eventList, outfile)
