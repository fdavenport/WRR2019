## analysis

See Methods section of the paper for a detailed description of the analysis. Some notes about computation times are indicated below for steps that take longer to run. 

* **input_file_paths.R**: filepaths specifying location of input data 
* **parameters.R**: defines parameters used throughout analysis
* **functions.R**: defines functions used throughout analysis

Read/calculate watershed averages (daily and climatological): 
* **read-gagesII-data.R**: read watershed data from raw GAGES-II files and combine into data.frame
* **convert-hourly-nldas-to-daily.R**: read and save daily values from NLDAS for one year at a time (>1 hour)
* **read-daily-nldas.R**: calculate watershed average daily values from NLDAS (>12 hours on 6 cores)
* **save-daily-basin-nldas-by-site**: save watershed daily averages by station
* **read-monthly-nldas.R**: calculate monthly averages for each watershed from NLDAS (~30 minutes on 2 cores)
* **calc-intensity-rates.R**: combine data on daily precipitation and snowmelt rates
* **calc-annual-basin-stats.R**: calc mean NDJFM rain percentages and trends
* **calc-soil-capicity.R**: calculate soil moisture capacity for each basin 

Analyze precipitation and snowmelt events: 
* **find-precip-snowmelt-event.R**: calculate 3-day precipitation and snowmelt events from NLDAS precipitation and SWE timeseries
* **find-flow-after-precip-snom-events.R**: calculate maximum streamflow after each precipitation/snowmelt event  
* **calc-precip-snom-event-data.R**: calculate variables for each 3-day precipitation/snowmelt event
* **panel-regression-bootstrapping.R**: calculate panel regression model 

Analyze streamflow peak events: 
* **find-streamflow-peaks.R**: calculate streamflow peak events from daily mean streamflow timeseries
* **calc-streamflow-peak-data.R**: calculate variables for each streamflow peak event
* **categorize-streamflow-peaks.R**: identify rain, snowmelt, and rain-on-snow streamflow peaks 

Analysis scripts for supporting information: 
* **read-snotel.R**: calculate SWE at Snotel sites (~30min on 2 cores)
* **read-snsr.R**: calculate daily watershed SWE from SNSR dataset (>3 hours on 3 cores)
* **calc-30mDEM-elevation.R**: extract watershed elevation data from 30m DEM 
* **calc-nldas-elevation.R**: extract watershed elevation data from NLDAS
* **calc-elevation-stats.R**: calculate elevation statistics for each watershed
