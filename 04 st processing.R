## ---------------------------
##
## Script name: status and trends processing
##
## Purpose of script: extract eBird status and trends for species in analysis 3
##
## Author: Rachel Drake
##
## Date Created: original 10/01/25, updated 01/04/2025
##
## Email: rld3@st-andrews.ac.uk
##
## ---------------------------
##
## Notes: 
##
##  
##
##
##
##
## Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, S. Ligocki, O. Robinson, 
## W. Hochachka, L. Jaromczyk, C. Crowley, K. Dunham, A. Stillman, I. Davies, 
## A. Rodewald, V. Ruiz-Gutierrez, C. Wood. 2023. eBird Status and Trends, 
## Data Version: 2022; Released: 2023. Cornell Lab of Ornithology, Ithaca, 
## New York. https://doi.org/10.2173/ebirdst.2022
##
##
## ---------------------------

## set working directory for Mac and PC

#setwd("~/Google Drive/")  		# working directory (mac)
setwd("G:/R Coding/birdlearning")  	# working directory (windows)

## ---------------------------

## load up the packages we will need:

library(tidyverse)
library(auk)
library(ebirdst)

# resolve namespace conflicts
select <- dplyr::select
map <- purrr::map
projection <- raster::projection

## ---------------------------

# prep all of our data locations

# where the species accumulation scores are kept (see script 03)
data_path <- 'SAC/'
# where the spatial covariate data is kept (see script 02)
data_spat_path <- 'proc_data/'

data_tag <- 'datahistorical_23'

data_proc_path <- 'datast/'

# load in SAC scores to grab species we need trends for
SAC_species <- read_csv(paste0(data_path, 'spec_score_', data_tag, '.csv')) 
species <- SAC_species$species

# load in years we need trends for (easiest to grab from corresponding
# spatial data as this is pretty small)
years <- read_csv(paste0(data_spat_path, data_tag, "_2016_19_full_modis_pland_checklists", ".csv"))
start_year <- min(unique(years$year))
end_year <- max(unique(years$year))



