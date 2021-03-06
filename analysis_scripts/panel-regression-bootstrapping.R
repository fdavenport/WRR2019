library(lfe)
library(dplyr)
library(tidyr)
source("./input_file_paths.R")
source("./functions.R")
source("./parameters.R")

args <- commandArgs(trailingOnly = TRUE)
arrayID <- as.numeric(args[1])
nBootPerJob <- as.numeric(args[2])
model_choice <- args[3]
bw <- as.numeric(args[4]) ## bin width
subset_var <- args[5]

## INPUT FILES: ----------------------------------------------------------------
sites <- sta2char(read.csv(site_file)$x)

gagesIIdata <- readRDS("../processed_data/gagesIIdata.Rds") %>%
    mutate(STAID = sta2char(STAID))  %>%
    filter(STAID %in% sites)

eventDFfile <- ("../processed_data/precip_snom_event_data.Rds")

soil_cap <- readRDS("../processed_data/basin_soil_capacity.Rds") %>%
    filter(STAID %in% sites)

cat(sprintf("Bin width = %i", bw))
nb <- 100/bw ## number of bins

## -----------------------------------------------------------------------------
eventDF <- readRDS(eventDFfile) %>%
    mutate(log_delta_streamflow= ifelse(delta_streamflow <= 0, log(.01),
                                        log(delta_streamflow))) %>%
    mutate(wateryear = ifelse(strftime(Date, "%m") >= 10,
                              as.numeric(strftime(Date, "%Y")) + 1,
                              as.numeric(strftime(Date, "%Y")))) %>%
    left_join(gagesIIdata[,c("STAID", "CLASS", "DRAIN_SQKM", "PPTAVG_BASIN",
                             "ncei_region", "SLOPE_PCT")], by = "STAID") %>%
    mutate(STAIDmon = paste0(STAID, "_", peakmon)) %>%
    left_join(soil_cap, by = "STAID") %>%
    mutate(RP = rain_pct,
           ## rain percentage of event runoff (RAIN + SNOW + SNOWMELT)
           PPT = ARAIN + ASNOW)

eventDF <- eventDF %>%
    ## remove lines where RP is NA
    subset(!is.na(RP))

print("Model Choice:")
print(model_choice)
print("Subset variable:")
print(subset_var)

## SELECT MODEL FORM: ----------------------------------------------------------
if(model_choice == "piecewise"){
    ## piecewise linear with watershed-specific time trends
    ## create non-parametric RP bins
    rpModelMatrix <- data.frame(eventDF$RP,
                                 matrix(sort(rep(seq(bw, 100, bw), nrow(eventDF))),
                                        ncol = nb, nrow = nrow(eventDF))) %>%
        setNames(., c("RP", paste0("rp", 1:nb))) %>%
        mutate(remainder = RP %% bw) %>%
        mutate_at(vars(paste0("rp", 1:nb)),
                  list(~ifelse(RP > ., bw, ifelse(RP < (.-bw), 0, remainder)))) %>%
        dplyr::select(-remainder, -RP)
    eventDF <- eventDF %>%
        cbind(., rpModelMatrix)
    ## model formula
    modelForm <- formula(paste("log_delta_streamflow ~ PPT + SOILM_AVAIL + SWE_melt +",
                               "as.factor(STAID):wateryear +",
                               paste(paste0("rp", 1:nb), collapse = "+"), " | STAID + STAIDmon"))
    outvar <- c("PPT", "SOILM_AVAIL", "SWE_melt", paste0("rp", 1:nb))
    linearModelForm <- formula(paste0("log_delta_streamflow ~ SOILM_AVAIL + SWE_melt + PPT + RP + as.factor(STAID):wateryear | STAID + STAIDmon"))
    linear_outvar <- c("PPT", "SOILM_AVAIL", "SWE_melt", "RP")
} else if(model_choice == "binned"){
    rpDummyMatrix <- data.frame(eventDF$RP, 1:nrow(eventDF)) %>%
        setNames(., c("RP", "eventID")) %>%
        mutate(rpDummy = "rpDummy", ones = 1) %>%
        mutate(dummyBin = cut(RP, seq(0, 100, bw), include.lowest = TRUE, labels = FALSE)) %>%
        mutate(rpDummy = paste0(rpDummy, dummyBin)) %>%
        dplyr::select(-dummyBin) %>%
        spread(rpDummy, ones, fill = 0) %>%
        dplyr::select(-RP, -eventID)

    eventDF <- eventDF %>%
        cbind(., rpDummyMatrix)
    ## model formula
    modelForm <- formula(paste("log_delta_streamflow ~ SOILM_AVAIL + SWE_melt + PPT +",
                               "as.factor(STAID):wateryear +",
                               paste(paste0("rpDummy", 2:(nb)), collapse = "+"), " | STAID + STAIDmon"))
    outvar <- c("PPT", "SOILM_AVAIL", "SWE_melt", paste0("rpDummy", 2:nb))
    linearModelForm <- formula(paste0("log_delta_streamflow ~ SOILM_AVAIL + SWE_melt + PPT + RP + as.factor(STAID):wateryear | STAID + STAIDmon"))
    linear_outvar <- c("PPT", "SOILM_AVAIL", "SWE_melt", "RP")
}

print("model form:")
print(modelForm)
print("linear model form:")
print(linearModelForm)

## -----------------------------------------------------------------------------
## SUBSET DATA:

if(subset_var == "none"){
    nsub <- 1
    subDF <- vector('list', length = nsub)
    subDF[[1]] <- eventDF
} else if(subset_var == "class"){
    subDF <- split(eventDF, eventDF$CLASS)
    nsub <- length(subDF)
} else if(subset_var == "region"){
    subDF <- split(eventDF, eventDF$nceiRegion)
    nsub <- length(subDF)
} else if(subset_var == "precip"){
    precip_bins <- c(0, quantile(gagesIIdata$PPTAVG_BASIN, c(1/3, 2/3)), 500)
    nsub <- length(precip_bins)-1
    eventDF <- eventDF %>% mutate(precip_bin = cut(PPTAVG_BASIN, precip_bins, labels = 1:nsub))
    subDF <- split(eventDF, eventDF$precip_bin)
} else if(subset_var == "area"){
    area_bins <- c(0, quantile(gagesIIdata$DRAIN_SQKM, c(1/3, 2/3)), 20000)
    nsub <- length(area_bins)-1
    eventDF <- eventDF %>% mutate(area_bin = cut(DRAIN_SQKM, area_bins, labels = 1:nsub))
    subDF <- split(eventDF, eventDF$area_bin)
} else if(subset_var == "slope"){
    slope_bins <- c(0, quantile(gagesIIdata$SLOPE_PCT, c(1/3, 2/3)), 200)
    nsub <- length(slope_bins)-1
    eventDF <- eventDF %>% mutate(slope_bin = cut(SLOPE_PCT, slope_bins, labels = 1:nsub))
    subDF <- split(eventDF, eventDF$slope_bin)
}

## for each subset, complete entire bootstrapping
for(j in 1:nsub){
    print("## ----------------------------------------##")
    print("Subset Number:")
    print(j)
    print("Subset Name:")
    print(names(subDF)[j])
    ## assign output file
    if(subset_var != "none"){
        bootResultFile <- paste0("../processed_data/bootstrapping/bootstrapResults_bw", bw, "_",
                                 model_choice, "_", subset_var, j, "_", arrayID, ".Rds")
    } else {
        bootResultFile <- paste0("../processed_data/bootstrapping/bootstrapResults_bw", bw, "_",
                                 model_choice, "_", arrayID, ".Rds")
    }

    sites <- unique(subDF[[j]]$STAID)
    nSites <- length(sites)
    print("Number of sites:")
    print(nSites)
    if(nSites < 10) next

    print("Bootstrapping progress:")
    ## -----------------------------------------------------------------------------
    nGrab <- nSites

    ## generate random samples
    set.seed(100*arrayID)
    siteGrabList <- lapply(1:nBootPerJob, function(x) sample(1:nSites, nGrab, replace = TRUE))
    ## check for sites that are included more than once in a given replicate
    dupCheck <- lapply(siteGrabList, function(x) duplicated(x))


    ## -----------------------------------------------------------------------------
    ## calculate model for each replicate
    bootCoeffResults <- vector('list', length = nBootPerJob)

    for(i in 1:nBootPerJob) {
        if(i%%10 == 0) print(i)
        currentData <- do.call(rbind, lapply(1:nGrab, function(x) {
            if(dupCheck[[i]][x]) {
                ##modify STAID so watersheds can be included more than once
                subset(subDF[[j]], STAID == sites[siteGrabList[[i]]][x]) %>%
                    mutate(STAID = paste0(STAID, "-", x))
            } else {
                subset(subDF[[j]], STAID == sites[siteGrabList[[i]]][x])

            }}))
        modelRep <- felm(formula = modelForm, data = currentData)
        coeff <- summary(modelRep)$coefficients[outvar,]

        linearModelRep <- felm(formula = linearModelForm, data = currentData)
        linear_coeff <- summary(linearModelRep)$coefficients[linear_outvar,]
        bootCoeffResults[[i]] <- list(coeff, linear_coeff)
    }

    saveRDS(bootCoeffResults, file = bootResultFile)

}
