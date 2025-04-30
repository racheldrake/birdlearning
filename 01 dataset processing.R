## ---------------------------
##
## Script name: dataset processing
##
## Purpose of script: extract subset of eBird data needed for analysis
##
## Author: Rachel Drake
##
## Date Created: original 10/01/25, updated 01/04/2025
##
## Email: rld3@st-andrews.ac.uk
##
## ---------------------------
##
## Notes: splitting the processed ebird data into the four data subsets
##   - 2016-19 and 2022-2024 complete datasets
##   - 2016-19 and 2022-2024 new observer datasets
##
## ---------------------------

## set working directory for Mac and PC

#setwd("~/Google Drive/")  		# working directory (mac)
setwd("G:/R Coding/birdlearning")  	# working directory (windows)


## ---------------------------

## load up the packages we will need:

library(tidyverse)

## ---------------------------

# where to find eBird csv from last script
raw_data_path <- 'data/'

# where I want processed datasets to end up
data_path <- 'proc_data/' 

# name of data extraction
data_tag <- "datahistorical_23"

# ebird data
ebd <- read_csv(paste0(raw_data_path, data_tag, ".csv"))

# full datasets
# 2016-19
ebd %>% 
  filter(year(ymd(observation_date)) %in% c(2004, 2005, 2006, 2007)) %>%
  filter(month(ymd(observation_date)) %in% 3:8) %>% 
  write_csv(paste0(data_path, data_tag, '_2016_19_full.csv'))

# 2022-24
ebd %>% 
  filter(year(ymd(observation_date)) %in% c(2009, 2010, 2011, 2012)) %>%
  filter(month(ymd(observation_date)) %in% 3:8) %>%
  write_csv(paste0(data_path, data_tag, '_2022_24_full.csv'))

# new observer datasets
  
# 2016-19
ebd %>% 
  # split up multi-observer records to have one checklist copy per observer
  separate_rows(observer_id, sep = ',') %>%
  # group by checklist and observer so we get each checklist for each observer
  group_by(checklist_id, observer_id) %>% 
  # take one record of each checklist, as we don't want each record enumerated separately
  slice(1) %>% ungroup() %>% 
  # take each observers history and put it in chronological order
  group_by(observer_id) %>% arrange(observation_date, .by_group = TRUE) %>%
  # then assign the checklist number
  mutate(checklist.no = row_number()) %>% 
  filter(year(ymd(observation_date)) %in% c(2004, 2005, 2006, 2007)) %>%
  filter(all(checklist.no %in% 1:100)) %>%
  ungroup() %>% 
  # keep only the information we need from the dataset
  select(-checklist.no) %>%
  # filter to breeding season
  filter(month(ymd(observation_date)) %in% 3:8) %>%
  write_csv(paste0(data_path, data_tag, '_2016_19_new.csv'))
  
# 2022-24
ebd %>% 
  # split up multi-observer records to have one checklist copy per observer
  separate_rows(observer_id, sep = ',') %>%
  # group by checklist and observer so we get each checklist for each observer
  group_by(checklist_id, observer_id) %>% 
  # take one record of each checklist, as we don't want each record enumerated separately
  slice(1) %>% ungroup() %>% 
  # take each observers history and put it in chronological order
  group_by(observer_id) %>% arrange(observation_date, .by_group = TRUE) %>%
  # then assign the checklist number
  mutate(checklist.no = row_number()) %>% 
  filter(year(ymd(observation_date)) %in% c(2009, 2010, 2011, 2012)) %>%
  filter(all(checklist.no %in% 1:100)) %>%
  ungroup() %>% 
  # keep only the information we need from the dataset
  select(-checklist.no) %>%
  # filter to breeding season
  filter(month(ymd(observation_date)) %in% 3:8) %>%
  write_csv(paste0(data_path, data_tag, '_2022_24_new.csv'))

