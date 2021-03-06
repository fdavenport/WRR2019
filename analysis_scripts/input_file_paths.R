## -----------------------------------------------------------------------------
## SET FILEPATHS FOR INPUT DATA
## -----------------------------------------------------------------------------

## smaller raw data files are included in repository
## larger files must be downloaded separately

input_data_folder <- "../input_data"


## -----------------------------------------------------------------------------
## FILES INCLUDED IN REPOSITORY:
## -----------------------------------------------------------------------------

## list of stations to use:
site_file <- paste0(input_data_folder, "/site_list.txt")

## NLDAS-2 elevation and soil capacity data:
nldas_elev_file <- paste0(input_data_folder, "/NLDAS_elevation.nc4")
nldas_soil_1depth <- paste0(input_data_folder, "/soil_ldas_cal_depth1.asc")
nldas_soil_2depth <- paste0(input_data_folder, "/soil_ldas_cal_depth2.asc")
nldas_soil_3depth <- paste0(input_data_folder, "/soil_ldas_cal_depth3.asc")
nldas_soil_1por <- paste0(input_data_folder, "/soil_ldas_cal_porosity1.asc")
nldas_soil_2por <- paste0(input_data_folder, "/soil_ldas_cal_porosity2.asc")
nldas_soil_3por <- paste0(input_data_folder, "/soil_ldas_cal_porosity3.asc")

## Snow telemetry (SNOTEL) data:
snotel_site_file <- paste0(input_data_folder, "/snotel_station_list.csv")
snotel_path <- input_data_folder


## -----------------------------------------------------------------------------
## FILES NOT INCLUDED IN REPOSITORY (MUST BE DOWNLOADED SEPARATELY)
## -----------------------------------------------------------------------------

## GAGES-II data:
basin_id_file <- paste0(input_data_folder, "/GAGES-II_data/conterm_basinid.txt")
climate_file <- paste0(input_data_folder, "/GAGES-II_data/conterm_climate.txt")
basin_class_file <- paste0(input_data_folder, "/GAGES-II_data/conterm_bas_classif.txt")
topo_file <- paste0(input_data_folder, "/GAGES-II_data/conterm_topo.txt")
hydro_dam_file <- paste0(input_data_folder, "/GAGES-II_data/conterm_hydromod_dams.txt")

## GAGES-II watershed shapefiles:
ref_boundary_file <- paste0(input_data_folder, "/GAGES-II_data/bas_ref_all.shp")
nonref_boundary_file1 <- paste0(input_data_folder,
                                "/GAGES-II_data/bas_nonref_WestMnts.shp")
nonref_boundary_file2 <- paste0(input_data_folder,
                                "/GAGES-II_data/bas_nonref_WestPlains.shp")
nonref_boundary_file3 <- paste0(input_data_folder,
                                "/GAGES-II_data/bas_nonref_WestXeric.shp")

## daily USGS streamflow data:
daily_streamflow_folder <- paste0(input_data_folder, "/daily_streamflow_data/")

## NLDAS-2 monthly data:
nldas_FORA_M_path <- paste0(input_data_folder, "/FORA0125_M/")
nldas_VIC_M_path <- paste0(input_data_folder, "/VIC0125_M/")

## NLDAS-2 hourly data:
nldas_FORA_H_path <- paste0(input_data_folder, "/FORA0125_H/")
nldas_VIC_H_path <- paste0(input_data_folder, "/VIC0125_H/")

## 30m DEM files (USGS 1_arc_second files):
dem_path <- paste0(input_data_folder, "/DEM_USGS/1_arc_second")

## Sierra Nevada Snow Reanalysis data:
snsr_path <- paste0(input_data_folder, "/SNSR/")

