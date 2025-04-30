## ---------------------------
##
## Script name: habitat covariates
##
## Purpose of script: attach habitat data to eBird data and make prediction surface
##
## Author: Rachel Drake
##
## Date Created: 07/04/2025
##
## Email: rld3@st-andrews.ac.uk
##
## ---------------------------
##
## Notes: Code taken from repository of eBird best practices, with a couple
##         highlighted sections taken from Johnston 2021
##   
## https://github.com/CornellLabofOrnithology/ebird-best-practices/blob/master/03_covariates.Rmd
## https://github.com/ali-johnston/ebird_sdms_DD_paper/tree/master
## download modis landcover data
## calculate pland within buffer around each checklist
## create prediction surface
##
## ---------------------------

## set working directory for Mac and PC

#setwd("~/R Coding/birdlearning")  		# working directory (mac)
setwd("G:/R Coding/birdlearning")  	# working directory (windows)


## ---------------------------

## load up the packages we will need:

library(sf)
library(raster)
library(MODIS)
library(exactextractr)
library(viridis)
library(tidyverse)

# resolve namespace conflicts
select <- dplyr::select
map <- purrr::map
projection <- raster::projection

## ---------------------------

## - Setup                                                                       ####

# string used to identify datasets
data_string <- 'datahistorical_23'

# list of data extractions (this should match ebird/dataset processing)
data_tags <- paste0(data_string, c('_2016_19_full', '_2022_24_full', '_2016_19_new', '_2022_24_new'))

# where I can find my datasets
data_extr_path <- 'proc_data/'

# where I can find and store downloaded spatial info
data_spat_path <- 'data/modis/'

# where I want processed data to end up
data_path <- 'proc_data/' 

# information about landcover classes
lc_classes <- readr::read_csv(paste0(data_spat_path, "modis_umd_classes.csv"))

# BCR shape file
bcr <- st_read(file.path(data_spat_path, 'BCR_Terrestrial_master.shp')) %>% 
  filter(BCR == 23) %>% 
  st_transform(crs = paste("+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
                           "+a=6371007.181 +b=6371007.181 +units=m +no_defs"))

for (data_tag in data_tags){
  # load in eBird data
  ebird <- read_csv(paste0(data_extr_path, data_tag, '.csv'))
  
  
  #- Downloading MODIS data                                                         ####
  
  tiles <- bcr %>% 
    st_transform(crs = 4326) %>% 
    st_bbox() %>% 
    getTile()
  
  # earliest year of ebird data
  begin_year <- format(min(ebird$observation_date), "%Y.01.01")
  # end date for ebird data
  end_year <- format(max(ebird$observation_date), "%Y.12.31")
  
  # function to reset MODIS login if having download problems
  # MODISoptions(MODISserverOrder = "LPDAAC", quiet = FALSE)
  
  # download tiles and combine into a single raster for each year
  tifs <- runGdal(product = "MCD12Q1", collection = "061", SDSstring = "01", 
                  extent = bcr %>% st_buffer(dist = 10000), 
                  begin = begin_year, end = end_year, 
                  outDirPath = "data", job = "modis",
                  MODISserverOrder = "LPDAAC", overwrite = TRUE) %>% 
    pluck("MCD12Q1.061") %>% 
    unlist()
  
  # rename tifs more descriptively
  for (i in seq_along(tifs)) {
    yr <- format(as.Date(names(tifs)[i]), "%Y")
    f <- file.path(data_spat_path, "modis_mcd12q1_umd_{yr}.tif") %>% 
      str_glue()
    file.copy(tifs[i], f)
  }
  
  #- Attach data to checklists                                                     ####
  
  # load the landcover data
  landcover <- list.files(data_spat_path, "^modis_mcd12q1_umd", 
                          full.names = TRUE) %>% 
    stack()
  
  # this section on naming files from Johnston 2021
  f_tifs <- list.files(data_spat_path, "^modis_mcd12q1_umd.*tif$", full.names = TRUE)
  layer_year <- str_extract(f_tifs, "(?<=modis_mcd12q1_umd_)[0-9]{4}") %>% 
    paste0("y", .)
  landcover <- stack(f_tifs) %>% 
    setNames(layer_year)
  
  # find most recent year of data MODIS has available (needed for data > 2020)
  max_lc_year <- names(landcover) %>% 
    str_extract("[0-9]{4}") %>% 
    as.integer() %>% 
    max()
  
  # make 3x3km buffer zone around ebird checklists
  neighborhood_radius <- 5 * ceiling(max(res(landcover))) / 2
  ebird_buff <- ebird %>% 
    distinct(year = format(observation_date, "%Y"),
             locality_id, latitude, longitude) %>% 
    # for 2019 use 2018 landcover data
    mutate(year_lc = if_else(as.integer(year) > max_lc_year, 
                             as.character(max_lc_year), year),
           year_lc = paste0("y", year_lc)) %>% 
    # convert to spatial features
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
    # transform to modis projection
    st_transform(crs = projection(landcover)) %>% 
    # buffer to create neighborhood around each point
    st_buffer(dist = neighborhood_radius) %>% 
    # nest by year
    nest(data = c(year, locality_id, geometry))
  
  # extract spatial data onto 3x3km buffers
  lc_extract <- NULL
  loop <- ebird_buff$year_lc[ebird_buff$year_lc %in% names(landcover)]
  for (yr in loop) {
    # get the buffered checklists for a given year
    regions <- ebird_buff$data[[which(yr == ebird_buff$year_lc)]]
    # get landcover values within each buffered checklist area
    ee <- exact_extract(landcover[[yr]], regions, progress = FALSE)
    # count the number of each landcover class for each checklist buffer
    ee_count <- map(ee, ~ count(., landcover = value))
    # attach the year and locality id back to the checklists
    ee_summ <- tibble(st_drop_geometry(regions), data = ee_count) %>% 
      unnest(data)
    # bind to results
    lc_extract <- bind_rows(lc_extract, ee_summ)
  }
  
  # calculate percentage landcover
  pland <- lc_extract %>% 
    # calculate proporiton
    group_by(locality_id, year) %>% 
    mutate(pland = n / sum(n)) %>% 
    ungroup() %>% 
    select(-n) %>% 
    # remove NAs after tallying so pland is relative to total number of cells
    filter(!is.na(landcover))
  
  # convert names to be more descriptive
  lc_names <- tibble(landcover = 0:15,
                     lc_name = c("pland_00_water", 
                                 "pland_01_evergreen_needleleaf", 
                                 "pland_02_evergreen_broadleaf", 
                                 "pland_03_deciduous_needleleaf", 
                                 "pland_04_deciduous_broadleaf", 
                                 "pland_05_mixed_forest",
                                 "pland_06_closed_shrubland", 
                                 "pland_07_open_shrubland", 
                                 "pland_08_woody_savanna", 
                                 "pland_09_savanna", 
                                 "pland_10_grassland", 
                                 "pland_11_wetland", 
                                 "pland_12_cropland", 
                                 "pland_13_urban", 
                                 "pland_14_mosiac", 
                                 "pland_15_barren"))
  pland <- pland %>% 
    inner_join(lc_names, by = "landcover") %>% 
    arrange(landcover) %>% 
    select(-landcover)
  
  # tranform to wide format, filling in implicit missing values with 0s%>% 
  pland <- pland %>% 
    pivot_wider(names_from = lc_name, 
                values_from = pland, 
                values_fill = list(pland = 0))
  
  # save
  write_csv(pland, paste0(data_path, data_tag, "_modis_pland_checklists", ".csv"))
  
  #- Prediction surface                                                           ####
  
  # essentially same as above but instead we attach to a 3x3km grid over the entire
  # BCR
  
  
  agg_factor <- round(2 * neighborhood_radius / res(landcover))
  r <- raster(landcover) %>% 
    aggregate(agg_factor) 
  r <- bcr %>% 
    st_transform(crs = projection(r)) %>% 
    rasterize(r, field = 1) %>% 
    # remove any empty cells at edges
    trim()
  r <- writeRaster(r, filename = paste0(data_spat_path, "/prediction-surface.tif"), overwrite = TRUE)
  
  # get cell centers and create neighborhoods
  r_centers <- rasterToPoints(r, spatial = TRUE) %>% 
    st_as_sf() %>% 
    transmute(id = row_number())
  r_cells <- st_buffer(r_centers, dist = neighborhood_radius)
  
  # get the buffered checklists for a given year
  lc_extract_p <- NULL
  # get landcover values within each buffered checklist area
  ee_p <- exact_extract(landcover[[yr]], r_cells, progress = FALSE)
  # count the number of each landcover class for each checklist buffer
  ee_count_p <- map(ee_p, ~ count(., landcover = value))
  # attach the year and locality id back to the checklists
  ee_summ_p <- tibble(st_drop_geometry(r_cells), data = ee_count_p) %>% 
    unnest(data)
  # bind to results
  lc_extract_p <- bind_rows(lc_extract_p, ee_summ_p)
  
  # calculate the percent for each landcover class
  pland_pred <- lc_extract_p %>% 
    group_by(id) %>% 
    mutate(pland = n / sum(n)) %>% 
    ungroup() %>% 
    select(-n) %>% 
    # remove NAs after tallying so pland is relative to total number of cells
    filter(!is.na(landcover))
  
  # convert names to be more descriptive
  pland_pred <- pland_pred %>% 
    inner_join(lc_names, by = "landcover") %>% 
    arrange(landcover) %>% 
    select(-landcover)
  
  # tranform to wide format, filling in implicit missing values with 0s
  pland_pred <- pland_pred %>% 
    pivot_wider(names_from = lc_name, 
                values_from = pland, 
                values_fill = list(pland = 0)) %>% 
    mutate(year = max_lc_year) %>% 
    select(id, year, everything())
  
  # join in coordinates
  pland_coords <- st_transform(r_centers, crs = 4326) %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    cbind(id = r_centers$id, .) %>% 
    rename(longitude = X, latitude = Y) %>% 
    inner_join(pland_pred, by = "id")
  
  # save prediction surface
  write_csv(pland_coords, paste0(data_path, data_tag, "_modis_pland_prediction-surface.csv"))
}
