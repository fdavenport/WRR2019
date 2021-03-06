## Flood Size Increases Nonlinearly Across the Western United States in Response to Lower Snow‐Precipitation Ratios

Supporting code for [_Davenport, Herrera-Estrada, Burke and Diffenbaugh (2020)_ ](https://doi.org/10.1029/2019WR025571)

#### To cite: 

Davenport, F. V.,  Herrera‐Estrada, J. E.,  Burke, M., &  Diffenbaugh, N. S. (2020).  Flood size increases non‐linearly across the western United States in response to lower snow‐precipitation ratios. *Water Resources Research*,  56, e2019WR025571. https://doi.org/10.1029/2019WR025571

If you find meaningful errors in the code or have questions, please contact Frances Davenport at fvdav@stanford.edu.

## Organization of repository: 

* **input_data**: small (<<1GB) input files for analysis 
* **analysis**: scripts to read and analyze data 
* **processed_data**: output from analysis 

## Data

Data used for the analysis comes from publicly available datasets. Most input data used in the analysis is not included here due to size, but can be accessed at the following locations: 

* **US Geological Survey Streamflow** data can be found at the [Surface Water for the Nation](https://waterdata.usgs.gov/nwis/sw) page. For this study, daily streamflow data was downloaded using the [dataRetrieval](https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html) package in R.  
* **GAGES-II** dataset is available at [https://doi.org/10.3133/70046617](https://doi.org/10.3133/70046617)
* **NLDAS-2 Forcing Data**: [monthly](https://disc.gsfc.nasa.gov/datasets/NLDAS_FORA0125_M_002/summary?keywords=NLDAS) and [hourly](https://doi.org/10.5067/6J5LHHOHZHN4) (monthly data is used to calculate climatological statistics)
* **NLDAS-2 VIC Output**: [monthly](https://disc.gsfc.nasa.gov/datasets/NLDAS_VIC0125_M_002/summary?keywords=NLDAS) and [hourly](https://doi.org/10.5067/6J5LHHOHZHN4) (monthly data is used to calculate climatological statistics)

Supporting Information Figures S2 & S3 require additional data: 
* **30m DEM from the National Elevation Dataset**: [https://catalog.data.gov/dataset/usgs-national-elevation-dataset-ned](https://catalog.data.gov/dataset/usgs-national-elevation-dataset-ned)
* **Sierra Nevada Snow Reanalysis**: [https://margulis-group.github.io/data/](https://margulis-group.github.io/data/)
