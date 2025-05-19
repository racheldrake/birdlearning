## ---------------------------
##
## Script name: dataset summaries
##
## Purpose of script: find no. observers and no. records in each group.
##
## Author: Rachel Drake
##
## Date Created: 19/05/2025
##
## Email: rld3@st-andrews.ac.uk
##
## ---------------------------
##
## Notes: This could have been included previously but I only
##          realised I needed it later on.
##
## ---------------------------

## set working directory for Mac and PC

#setwd("~/R Coding/birdlearning")  		# working directory (mac)
setwd("G:/R Coding/birdlearning")  	# working directory (windows)


## ---------------------------

## load up the packages we will need:

library(tidyverse)

# resolve namespace conflicts
select <- dplyr::select

## ---------------------------

## - Setup                                                                       ####

# string used to identify datasets
data_string <- 'bcr23_2025'

# list of data extractions (this should match ebird/dataset processing)
data_tags <- paste0(data_string, c('_2016_19_full', '_2022_24_full', '_2016_19_new', '_2022_24_new'))

# where I can find my datasets
data_extr_path <- 'proc_data/'

# where I want processed summaries to end up
data_path <- 'proc_data/' 

# set up column titles
dataset_summary <- c('Dataset', 'No. Observers', 'No. Checklists', 'No. Observations')

# run through datasets and extract info we want
for (data_tag in data_tags){
  # load in eBird data
  ebd <- read_csv(paste0(data_extr_path, data_tag, '.csv'))
  dataset_summary <- rbind(dataset_summary, c(data_tag, length(unique(ebd$observer_id)), length(unique(ebd$checklist_id)), length(ebd$checklist_id)))
}   

# save to CSV
write_csv(as.data.frame(dataset_summary), file = paste0(data_path, data_string, '_model_summary.csv'))


  