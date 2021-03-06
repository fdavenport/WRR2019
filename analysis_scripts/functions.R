library(stringr)
library(raster)
library(velox)
library(rgdal)

## FUNCTIONS USED IN DATA PROCESSING AND ANALYSIS

## -----------------------------------------------------------------------------
## assign NCEI region based on state abbreviation
swStates <- c("CO", "UT", "NM", "AZ")
wStates <- c("CA", "NV")
nwStates <- c("WA", "OR", "ID")
nrpStates <- c("WY", "MT")
southStates <- c("TX")

find_ncei_region <- function(st){
    ifelse(st %in% swStates, "Southwest",
    ifelse(st %in% wStates, "West",
    ifelse(st %in% nwStates, "Northwest",
    ifelse(st %in% nrpStates, "Northern Rockies",
    ifelse(st %in% southStates, "South", "otherNCEI")))))
}

## -----------------------------------------------------------------------------
## convert USGS IDs to 8-digit character format
sta2char <- function(site_vec){
    ## INPUT
    ## site_vec is an integer vector of USGS station IDs

    ## RETURN
    ## character vector of station IDs in 8-digit character format

    for(i in 1:length(site_vec)){
        station <- toString(site_vec[i])
        len <- nchar(station)
        ## add leading zeros for station with <8 char
        if (len < 8){
            station <- str_pad(station, 8, pad = "0")
        }
        ## two stations have 9-digit format
        if(station == "94196783") station <- "094196783"
        if(station == "94985005") station <- "094985005"
        site_vec[i] <- station
    }
    site_vec
}

## -----------------------------------------------------------------------------
hrGRIB2dayRaster <- function(filepath, fileprefix, dt,
                       model, vars = NULL, fun = "sum", outdirectory){
    ## WHAT: reads hourly NLDAS GRIB files and saves as daily raster files
    ## INPUT
    ## filepaths = path to files
    ## fileprefix = prefix of filename before date
    ## dt = date to aggregate, YearMonDay format: 19801001
    ## model = FORA or VIC
    ## vars = variables to include
    ## fun = function to aggregate each variable ("sum", "mean", or "inst")
    ##       either vector with function for each variable or one function to use for all variables
    ## outdirectory = directory to save new daily files
    ## RETURN
    ## NULL, saves day file as raster file

    if (length(fun) == 1) fun <- rep(fun, nVars)

    if (model == "FORA") {
        GRIBvars <- c("TMP", "SPFH", "PRES", "UGRD", "VGRD", "DLWRF",
                      "var153", "CAPE", "PEVAP", "APCP", "DSWRF")
        suffix <- "00.002.grb"
    } else if (model == "VIC"){
        GRIBvars <- c("ASNOW", "ARAIN", "EVP", "SSRUN", "BGRUN", "SNOM", "SNOT", "AVSFT",
                      "RADT", "WEASD", "TSOIL1", "TSOIL2", "TSOIL3", "SOILM5", "RZS",
                      "SOILM1", "SOILM2", "SOILM3", "LSOIL1", "LSOIL2", "LSOIL3", "MSTAV5",
                      "TRANS", "SBSNO", "LAI", "SNOD", "SNOWC")
        suffix <- "00.002.grb.SUB.grb"
    }

    hours <- str_pad(0:23, 2, pad = "0")

    yr <- as.integer(dt) %/% 10000 ## determine year for correct subfolder

    hour.raster.list <- vector("list", length=24)
    for (j in seq_along(hours)){
        ## chose file
        nldasFilename <- paste0(filepath, yr, "/", fileprefix, dt,
                                ".", hours[j], suffix)

        ## read in rasters and assign variable names
        oldw <- getOption("warn")
        options(warn = -1)
        hour.raster.list[[j]] <- readGDAL(nldasFilename)
        options(warn = oldw)
        names(hour.raster.list[[j]]) <- GRIBvars
    }
    var.raster.list <- vector("list", length = length(vars))
    for (k in seq_along(vars)){
        temp.raster.list <- lapply(1:24,
                                   function(x) raster(hour.raster.list[[x]][vars[k]]))
        if(fun[k] == "mean") {
            temp <- mean(brick(temp.raster.list), na.rm = TRUE)
        } else if (fun[k] == "sum") {
            temp <- sum(brick(temp.raster.list), na.rm = TRUE)
        } else if (fun[k] == "inst") {
            temp <- temp.raster.list[[24]]
        } else {
            stop("incorrect function used. options are sum, mean or inst")
        }
        names(temp) <- vars[k]
        var.raster.list[[k]] <- temp
    }
    dayRaster <- brick(var.raster.list)
    dayOut <- writeRaster(dayRaster, filename = paste0(outdirectory, model, "0125_D.A", dt,
                                                        ".grd"), overwrite = TRUE)
    return(1)
}


## -----------------------------------------------------------------------------
extract_raster_daily <- function(boundaries, fileprefix, startDay = 19801001,
                               endDay = 20160930, vars, res = 1){
    ## WHAT: calculates mean value of NLDAS variable within basin boundary for hourly data from
    ##       daily raster files created in hrGRIB2dayRaster()
    ## INPUT
    ## boundaries = SpatialPolygonsDataFrame with feature boundaries to use for extraction
    ## fileprefix = path and prefix of raster files,
    ##              eg "/scratch/users/fvdav/NLDAS/VIC0125_D/VIC0125_D.A"
    ## startDay = starting day,YearMonDay format: 19801001
    ## endDay =  end day, YearMonDay format: 20160930
    ## vars = character vector listing variables (short form) to extract
    ## res = resolution to increase raster by, default of 1 is no change

    ## RETURN
    ## array of mean variable value for each year/mon for each station
    ## with dimensions of (watersheds) X (days) X (variables)
    if(is.na(startDay)) return(NA)

    days <- seq.Date(as.Date(as.character(startDay), format = "%Y%m%d"),
                     as.Date(as.character(endDay), format = "%Y%m%d"),
                     "days")
    climateData <- array(,dim=c(nrow(boundaries), length(days), length(vars)),
                    dimnames=list(boundaries$GAGE_ID, days, vars))

    for (i in seq_along(days)){
        cat(sprintf("day = %i\n", days[i]))
        dt <- gsub("-", "", days[i])

        ## chose file for correct year/month combo
        rasterFilename <- paste0(fileprefix, dt, ".grd")

        ## initialize raster list
        rasters <- brick(rasterFilename)
        rasters <- rasters[[vars]]

        rasters <- velox(disaggregate(rasters, res))
        tempData.list <- rasters$extract(sp=boundaries, small=TRUE)
        ## mean is calculated outside of extract to allow for NA remove
        meanData.list <- lapply(1:length(tempData.list),
                                function(x) colMeans(tempData.list[[x]], na.rm = TRUE))
        ## convert mean Data list into array and store in climateData array
        climateData[,i,] <- t(simplify2array(meanData.list))
    }
    climateData
}

## -----------------------------------------------------------------------------

extractNLDASmonGRIB <- function(boundaries, filepath, prefix, model, startYear = 1980,
                             endYear = 2016, mon, vars, res = 1){
    ## WHAT: calculates mean value of NLDAS variable within basin boundary
    ## INPUT
    ## boundaries = SpatialPolygonsDataFrame with feature boundaries to use for extraction
    ## filepath = path to files including model/forcing to use;
    ##              eg: "/scratch/user/NLDAS/FORA0125_M/"
    ##              NLDAS files should be in GRIB format
    ## prefix = prefix of file name, eg: "NLDAS_FORA0125_M.A"
    ## model = FORA or VIC
    ## startYear = first water year (default 1980)
    ## endYear = last water year (default 2016)
    ## mon = 2-digit character vector of months to include;
    ## eg: c("10", "11", "12", "01") in water year order
    ## vars = character vector listing variables (short form) to extract from NLDAS
    ## res = resolution to increase raster by, default of 1 is no change

    ## RETURN
    ## array of mean variable value for each year/mon for each station
    ## with dimensions of (watersheds) X (years) X (months) X (variables)

    years <- startYear:endYear
    nYears <- length(years)
    climateData <- array(,dim=c(nrow(boundaries), nYears, length(mon), length(vars)),
                    dimnames=list(boundaries$GAGE_ID, years, mon, vars))

    ## NOTE: name and order of variables in grib files determined using the
    ## wgrib command line utility
    if (model == "FORA") {
        GRIBvars <- c("TMP", "SPFH", "PRES", "UGRD", "VGRD", "DLWRF",
                  "var153", "CAPE", "PEVAP", "APCP", "DSWRF")
    } else if (model == "VIC"){
        GRIBvars <- c("NSWRS", "NLWRS", "LHTFL", "SHTFL", "GFLUX",
                      "SNOHF", "DSWRF", "DLWRF", "ASNOW", "ARAIN",
                      "EVP", "SSRUN", "BGRUN", "SNOM", "SNOT",
                      "AVSFT", "var139", "ALBDO", "WEASD", "CNWAT",
                      "TSOIL1", "TSOIL2", "TSOIL3", "SOILM5",
                      "var250", "SOILM100cm", "SOILM1", "SOILM2",
                      "SOILM3", "LSOIL1", "LSOIL2", "LSOIL3",
                      "MSTAV5", "MSTAV100cm", "EVCW", "TRANS", "EVBS",
                      "SBSNO", "ACOND", "LAI", "SNOD", "SNOWC",
                      "MXSALB")
    }

    for (i in seq_along(years)){
        cat(sprintf("year = %i\n", years[i]))
        for (j in seq_along(mon)){
            ## adjust year to fit water year
            yr <- ifelse(mon[j]>="10", years[i]-1, years[i])

            ## chose file for correct year/month combo
            nldasFilename <- paste0(filepath, prefix, yr, mon[j], ".002.grb")

            ## read in rasters and convert to velox format
            oldw <- getOption("warn")
            options(warn = -1)
            rasters <- readGDAL(nldasFilename)
            options(warn = oldw)
            names(rasters) <- GRIBvars
            rasters <- brick(rasters[vars])
            rasters <- velox(disaggregate(rasters, res))
            ## disaggregate x4 yields 1/32 degree grid cells

            tempData.list <- rasters$extract(sp=boundaries, small=TRUE)
            ## mean is calculated outside of extract to allow for NA remove
            meanData.list <- lapply(1:length(tempData.list), function(x) colMeans(tempData.list[[x]],
                                                                              na.rm = TRUE))
            ## convert mean Data list into array and store in precipData array
            climateData[,i,j,] <- t(simplify2array(meanData.list))
        }
    }
    climateData
}

## -----------------------------------------------------------------------------

findPeaks <- function(flowData, timeData, peak_window = 7, thr = 0, allowEndPeaks = TRUE){
    ## WHAT: returns date and magnitude of peak flow events
    ## INPUT:
    ## flowData = time series of flow data
    ## timeData = timesteps corresponding to flow data
    ##            NOTE: assumes time series is in order and has uniform spacing
    ## window = minimum time separation between peaks (whatever unit of timeData is)
    ## thr = minimum threshold (quantile) for peaks
    ## allowEndPeaks = whether or not first or last day in time series can be peak
    ## RETURN:
    ## data.frame with peak magnitude and date of occurence

    ## create list of indices within window of peak, e.g. -6:-1,1:6
    windowInd <- -(peak_window-1):(peak_window-1)

    peakThresh <- quantile(flowData, thr, na.rm = TRUE)

    ## max must be greater than previous day, and >= following day
    localMaxCheck <- sapply(1:length(flowData),
                            function(x) flowData[x]>flowData[x-1] &&
                                        flowData[x]>=flowData[x+1])
    if(allowEndPeaks){
        ## check for max at first or last data point
        localMaxCheck[1] <- ifelse(flowData[1]>=flowData[2],TRUE,FALSE)
        localMaxCheck[length(flowData)] <- ifelse(flowData[length(flowData)] >
                                                  flowData[length(flowData)-1],
                                                  TRUE, FALSE)
    } else {
            localMaxCheck[1] <- FALSE
            localMaxCheck[length(flowData)] <- FALSE
    }
    peakIndex <- which(localMaxCheck & flowData > peakThresh)
    j <- 1
    goodPeaks <- vector(,length(peakIndex))

    while(length(peakIndex)>0){
        tempMaxInd <- peakIndex[which.max(flowData[peakIndex])]
        goodPeaks[j] <- tempMaxInd
        ## remove peaks within 7 days
        peakIndex <- peakIndex[-(which(peakIndex %in% (windowInd+tempMaxInd)))]
        j <- j+1
    }
    result <- data.frame(timeData[sort(goodPeaks)], flowData[sort(goodPeaks)])
    names(result) <- c("peak_dt", "flow")
    return(result)
}

## -----------------------------------------------------------------------------
findPrecipEvents <- function(varTS, nEvents, window){
    ## WHAT: finds peaks from time series
    ## varTS = vector with variable values at regular time intervals (ex. days)
    ## nEvents = number of events to find, or "all" to find all events
    ## window = minimum separation between peaks

    windowInd <- -(window-1):(window-1)

    tmpTS <- varTS

    if(nEvents == "all"){
        goodEvents <- as.data.frame(matrix(,nrow=length(varTS), ncol = 2,
                                           dimnames = list(NULL, c("Date", "value"))))
        i <- 1
        while(length(tmpTS) > 0 & max(tmpTS) > 1){
            tmpMaxInd <- which.max(tmpTS)
            goodEvents[i,"Date"] <- names(tmpTS)[tmpMaxInd]
            goodEvents[i,"value"] <- tmpTS[tmpMaxInd]
            if(tmpMaxInd < window) {
                remi <- 0:(tmpMaxInd+window-1)
            } else if (tmpMaxInd > (length(tmpTS) - window)) {
                remi <- (tmpMaxInd-window+1):length(tmpTS)
            } else { remi <- windowInd+tmpMaxInd }

            tmpTS <- tmpTS[-remi]
            i <- i+1
        }
        goodEvents <- na.omit(goodEvents)
    } else {
        goodEvents <- as.data.frame(matrix(,nrow=nEvents, ncol = 2,
                                           dimnames = list(NULL, c("Date", "value"))))
        for(i in 1:nEvents){
            tmpMaxInd <- which.max(tmpTS)
            goodEvents[i,"Date"] <- names(tmpTS)[tmpMaxInd]
            goodEvents[i,"value"] <- tmpTS[tmpMaxInd]
            if(tmpMaxInd < window) {
                remi <- 0:(tmpMaxInd+window-1)
            } else if (tmpMaxInd > (length(tmpTS) - window)) {
                remi <- (tmpMaxInd-window+1):length(tmpTS)
            } else { remi <- windowInd+tmpMaxInd }

            tmpTS <- tmpTS[-remi]
        }
    }
    return(goodEvents)
}


## -----------------------------------------------------------------------------
## function to read in bootstrapping results:
readBootData <- function(fileList, path, coeffnames, bw){
    ## works to read in piecewise linear results
    nb = 100/bw
    rawResults <- lapply(fileList,
                         function(x) readRDS(paste0(path, x)))
    coeffResults <- as.data.frame(rbindlist(lapply(rawResults,
                                                   function(x) rbindlist(lapply(x, function(z) {
                                                       as.data.frame(z[[1]][coeffnames, "Estimate"])%>%
                                                           setNames(., c("Estimate")) %>%
                                                           rownames_to_column(var = "rpBin") %>%
                                                           spread(rpBin, Estimate) %>%
                                                           dplyr::select(coeffnames)})))))

    n <- nrow(coeffResults)
    coeffMatrix <- matrix(, ncol = nb+1, nrow = nrow(coeffResults))
    coeffMatrix[,1] <- 0
    coeffMatrix[,2:(nb+1)] <- as.matrix(coeffResults)
    yMatrix <- t(as.matrix(cumsum(as.data.frame(t(bw*coeffMatrix)))))

    lastColVar <- paste0("V", n)
    allLinesDF <- data.frame(seq(0, 100, bw), t(yMatrix)) %>%
        gather(iteration, y, "V1":lastColVar) %>%
        setNames(., c("x", "iteration", "y"))

    summaryDF <- as.data.frame(matrix(unlist(lapply(2:(nb+1), function(x) quantile(yMatrix[,x], prob = c(0.025, 0.5, 0.975)))),
                                      ncol = 3, byrow = TRUE)) %>%
        setNames(., c("low", "mid", "high")) %>%
        mutate(x = seq(bw, 100, bw)) %>%
        rbind(., c(0, 0, 0, 0)) %>%
        mutate(lowEXP = exp(low), midEXP = exp(mid), highEXP = exp(high))

    return(list(allLinesDF, summaryDF))
}


read_boot_subset <- function(bin_name, nbins = 3, bw = 20){
    nb <- 100/bw
    boot_files <- lapply(1:nbins,
                         function(x) list.files(path = "../processed_data/bootstrapping/",
                                                pattern = paste0("bootstrapResults_bw", bw,
                                                                 "_piecewise_", bin_name,
                                                                 x)))
    boot_df <- do.call(rbind,
                       lapply(1:nbins, function(i) {
                           readBootData(boot_files[[i]],
                                        path = "../processed_data/bootstrapping/",
                                        paste0("rp", 1:nb) , bw)[[2]] %>%
                               mutate(bin = i)}))
}
