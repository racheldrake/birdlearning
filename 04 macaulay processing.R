## ---------------------------
##
## Script name: Macaulay library processing
##
## Purpose of script: use extracted numbers of photos and audio recordings from
##            the macaulay online library to construct an audio index per species
##
## Author: Rachel Drake
##
## Date Created: 08/05/2025
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
##
##
## ---------------------------

## set working directory for Mac and PC

#setwd("~/Google Drive/")  		# working directory (mac)
setwd("G:/R Coding/birdlearning")  	# working directory (windows)

## ---------------------------

## load up the packages we will need:

library(tidyverse)

# resolve namespace conflicts
select <- dplyr::select

## ---------------------------

# prep all of our data locations

# where the macaulay info is kept
data_path <- 'data/'

# what file is called
data_tag <- 'macaulay_complete'

# what taxonomy is called
taxonomy_tag <- 'ebird_taxonomy'

# where I want the processed file to end up
proc_data <- 'proc_data/'

# load data

raw <- read_csv(paste0(data_path, data_tag, '.csv'))
taxonomy <- read_csv(paste0(data_path, taxonomy_tag, '.csv')) %>% select(species = SPECIES_CODE, common_name = PRIMARY_COM_NAME, scientific_name = SCI_NAME)

raw %>% 
  #calculate index
  mutate(audio_index = audio/photo) %>% 
  # join taxonomy
  left_join(taxonomy, by = 'species') %>% 
  # remove unneccessary columns
  select(-c(photo, video, audio, species)) %>%
  # write to csv
  write_csv(paste0(proc_data, 'audio_index.csv'))

# note that this does not have a normal distribution, I tend to log and scale it
# when used in a model but doing this now runs into all sorts of NaN errors
# for some odd species that I didn't want to deal with if I didn't have to
