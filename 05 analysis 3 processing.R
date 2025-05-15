## ---------------------------
##
## Script name: status and trends processing
##
## Purpose of script: extract BBS annual species indices, matching to eBird 
##                    taxonomy
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

# where the species accumulation scores are kept (see script 03)
data_path <- 'SAC/'

data_string <- 'bcr23_2025'

data_proc_path <- 'stdata/'

# eBird path
ebd_path <- 'proc_data/'

# where I want to save results
results_path <- 'analysis_3/'

# load in audio index
audio_index <- read_csv('audio_index.csv')

#- grab count per hour data ####

full_data_tags <- paste0(data_string, c('_2016_19_new', '_2022_24_new'))

total_N <- function(species, ebd){
  sum(ebd$observation_count[ebd$common_name == species])
}

N <- NULL
for (data_tag in full_data_tags){
  # going to load each dataset one by one and calculate each species count/hour
  ebd <- read_csv(paste0(ebd_path, data_tag, '.csv'))
  
  # filter to only checklists of length 1 hour and above
  ebd_1hr <- ebd %>%
    # filter(duration_minutes >= 60) %>%
    filter(observation_count != 'X') %>%
    mutate(observation_count = as.numeric(observation_count))
  
  # calculate the total number of hours spent birding
  total_duration <- ebd_1hr %>% distinct(checklist_id, .keep_all = TRUE)
  total_duration <- sum(total_duration$duration_minutes) / 60
  
  species_list <- unique(ebd_1hr$common_name)
  
  output <- sapply(species_list, total_N, ebd = ebd_1hr)
  N_data <- data.frame(common_name = names(output), N = as.numeric(output)/total_duration, time = ifelse(str_detect(data_tag, '2016'), 'pre', 'post'))
  N <- rbind(N, N_data)
}

N_species <- N %>% filter(time == 'pre') %>% distinct(common_name)
N <- N %>% filter(common_name %in% N_species$common_name)

# load in SAC scores to grab species we need trends for
SAC_species <- unique(N$common_name)
start_year <- 2016
end_year <- 2023
pre <- c(2016, 2017, 2018, 2019)
post <- c(2022, 2023)

# load in the trends from BBS data
BBS <- read_csv(paste0(data_proc_path, 'BBS_Indices.csv'))

BBS_codes <- read_csv(paste0(data_proc_path, 'bbs_codes.csv'))

# 4 species here that don't have a match
main_join <- data.frame(common_name = SAC_species) %>% left_join(BBS_codes, join_by(common_name == BBS_common))

# multiple regions then pivot this wide for the join
test <- BBS %>% filter(Region %in% 'US1') 
joined <- main_join %>% mutate(Year = list(start_year:end_year)) %>% 
  unnest(Year) %>% left_join(test, by = c('AOU', 'Year')) %>% 
  # do weighted sum across regions here
  mutate(time = ifelse(Year %in% pre, 'pre', NA),
         time = ifelse(Year %in% post, 'post', time),
         time = as.factor(time)) %>% 
  drop_na(time) %>% group_by(common_name, time) %>%
  summarise(Index = mean(Index, na.rm = FALSE)) %>% 
  pivot_wider(names_from = time, values_from = Index) %>%
  mutate(trend = post - pre) %>% select(-c(pre, post)) 

model_data <- N %>% left_join(joined, by = 'common_name') %>% 
  left_join(audio_index, by = 'common_name') %>% 
  select(-scientific_name) %>% drop_na() %>% write_csv(paste0(results_path, data_string, '_N_model_new.csv'))




