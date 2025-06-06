## ---------------------------
##
## Script name: species accumulation curves
##
## Purpose of script: extract subset of eBird data needed for analysis
##
## Author: Rachel Drake
##
## Date Created: 07/04/2025
##
## Email: rld3@st-andrews.ac.uk
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC

#setwd("~/Google Drive/")  		# working directory (mac)
setwd("G:/R Coding/birdlearning")  	# working directory (windows)

## ---------------------------

## load up the packages we will need:

# library(geodata)
library(tidyverse)
library(concstats)
library(mgcv)
library(purrr)
library(plotrix)
# resolve namespace conflicts
select <- dplyr::select
map <- purrr::map
projection <- raster::projection

## ---------------------------

# data tag
data_string <- "bcr23_2025"

# habitat path
spat_path <- 'proc_data/'

# eBird path
ebd_path <- 'proc_data/'

# where I want processed data to end up
data_path <- 'SAC/' 

data_tags <- paste0(data_string, c('_2022_24_full', '_2016_19_new', '_2022_24_new'))

for (data_tag in data_tags){
  # load in habitat data
  habitat <- read_csv(paste0(spat_path, data_tag, '_modis_pland_checklists.csv' ))
  
  # normalise habitat covariates and apply GSI index function
  habitat <- habitat %>% rowwise() %>%
    mutate(across(starts_with('pland'), ~ . / sum(c_across(starts_with('pland'))), .names = '{col}')) %>%
    mutate(GSI = as.numeric(concstats_simpson(c_across(starts_with('pland')))))
  
  # load in eBird observation data
  ebd <- read_csv(paste0(ebd_path, data_tag, '.csv'))
  
  # select relevant columns
  data <- ebd %>% select(checklist_id, observer_id, observation_date, locality_id, 
                         latitude, longitude, scientific_name, 
                         time_observations_started, duration_minutes, 
                         effort_distance_km, number_observers, protocol_name) %>%
    # extract only stationary and traveling survey protocols (newer datasets this is already done)
    filter(protocol_name %in% c('Stationary', 'Traveling')) %>%
    # extract year, day of year and time of day to numerical values
    mutate(year = year(ymd(observation_date)),
           doy = yday(ymd(observation_date)),
           tod = hour(hms(time_observations_started)) + 
             minute(hms(time_observations_started))/60) %>%
    # add the number of species reported per checklist
    group_by(checklist_id) %>% mutate(no.species = n()) %>% ungroup() %>%
    # add habitat information
    left_join(habitat, by = c('year', 'locality_id')) %>%
    # split up multi-observer records to have one checklist copy per observer
    separate_rows(observer_id, sep = ',')
  
  # checklist indices tell us the order of submission of checklists
  checklist_indices <- data %>% select(checklist_id, observer_id, observation_date) %>%
    # group by checklist and observer so we get each checklist for each observer
    group_by(checklist_id, observer_id) %>% 
    # take one record of each checklist, as we don't want each record enumerated separately
    slice(1) %>% ungroup() %>% 
    # take each observers history and put it in chronological order
    group_by(observer_id) %>% arrange(observation_date, .by_group = TRUE) %>%
    # then assign the checklist number
    mutate(checklist.no = row_number()) %>% ungroup() %>% 
    # keep only the information we need for the join
    select(checklist_id, observer_id, checklist.no)
  
  # add the indices into the full checklist dataset
  data <- data %>% left_join(checklist_indices, by = c('checklist_id', 'observer_id'))
  
  # format data to run model on number of species
  # remove scientific_name as it's the only column that varies within checklists
  data_model <- data %>% select(-scientific_name) %>% 
    # summarise to find number of species associated with each checklist
    group_by(checklist_id, observer_id) %>% summarise(
    no.species = n(),
    across(everything(), ~ first(.)),
    .groups = "drop") %>%
    mutate(effort_distance_km = ifelse(protocol_name == 'Stationary', 0, effort_distance_km)) %>%
    # remove any data NAs (missing checklist metadata)
    drop_na() %>% 
    # keep only observers with at least 10 checklists (helps model convergence)
    group_by(observer_id) %>% filter(n() > 10) %>% ungroup() %>%
    # turn protocol name into a factor
    mutate(protocol_name = as.factor(protocol_name)) 
  
  # create an average checklist to use to predict no. species each observer 
  # detects in the same setting
  pred_spat <- read_csv(paste0(spat_path, data_tag, '_modis_pland_prediction-surface.csv'))
  
  pred_1hr <- pred_spat %>% ungroup() %>%
    # remove everything that's not a model covariate
    select(-c('id', 'longitude', 'latitude', 'year')) %>% 
    # summarise all of these variables to get the average of the landcovers
    summarise(across(everything(), mean, na.rm = TRUE)) %>% rowwise() %>%
    # normalise the new landcover
    mutate(across(starts_with('pland'), ~ . / sum(c_across(starts_with('pland'))), .names = '{col}')) %>%
    # calculate the new GSI index
    mutate(GSI = as.numeric(concstats_simpson(c_across(starts_with('pland'))))) %>%
    # average coefficient, 100th checklist
    mutate(checklist.no = 100,
           # 1st september
           doy = 244,
           # walked 1km
           effort_distance_km = 1,
           # 7am
           tod = 7,
           # 1 hour
           duration_minutes = 60,
           # 1 observer
           number_observers = 1,
           # travelling
           protocol_name = as.factor('Traveling'),
           # allows me to join this data to the indiviudals in the model for
           # predicting the random effects
           join = 1)
  
  # split data into a list for each observer
  observer_groups <- data_model %>%
    group_by(observer_id) %>%
    group_split()
  
  if (length(observer_groups) >= 300){
    # set group size
    group_size = 300
    
    # regroup the lists to have 300 observers
    grouped_data <- split(observer_groups, ceiling(seq_along(observer_groups)/group_size))
    # bin each group of lists into a dataframe
    for (i in 1:length(grouped_data)){
      grouped_data[[i]] <- do.call(rbind, lapply(grouped_data[[i]], as.data.frame))
    }
    
    # find the number of groups
    n_groups <- length(grouped_data)
    # the last group will have the 'leftovers', we split these again
    leftovers <- grouped_data[[n_groups]] %>% group_by(observer_id) %>% group_split()
    
    # redistribute them between the other groups
    for (i in seq_along(leftovers)){
      # assign group ID by their position in the loop
      group_idx <- ((i - 1) %% n_groups) + 1
      # bind new row to it's group
      grouped_data[[group_idx]] <- bind_rows(grouped_data[[group_idx]], leftovers[i])
    }
    # remove the final group we redistributed
    grouped_data[[n_groups]] <- NULL
    # re-assign the numner of groups
    n_groups <- length(grouped_data)
    
    # run the models for each group
    for (i in 1:n_groups){
      # we factor the observer ID's now for the random effect, doing it before makes
      # the model try to fit lots of empty RE's for the observers not in this group
      grouped_data[[i]]$observer_id <- as.factor(grouped_data[[i]]$observer_id)
      
      # model specification
      # basic SAC, increase with time
      model_temp <- bam(no.species ~ duration_minutes + I(sqrt(duration_minutes)) + 
                          # species availability covariates
                          pland_00_water + pland_01_evergreen_needleleaf + pland_03_deciduous_needleleaf + pland_04_deciduous_broadleaf + pland_05_mixed_forest + 
                          pland_08_woody_savanna + pland_09_savanna + pland_10_grassland + pland_12_cropland + pland_13_urban + 
                          pland_15_barren + GSI +
                          # effort expended for a given checklist
                          s(doy, bs = 'cc') + protocol_name + effort_distance_km + 
                          number_observers + s(tod, bs = 'tp') + 
                          I(log(checklist.no)) + 
                          # observer-specific effects for the intercept and the slope
                          s(observer_id, bs = 're') + 
                          s(observer_id, duration_minutes, bs = 're'), 
                        data = grouped_data[[i]], 
                        family = poisson, 
                        discrete = TRUE)
      
      # create prediction dataframe, with same checklist variables and different
      # observer ID's
      pred_observers <- data.frame(observer_id=unique(grouped_data[[i]]$observer_id))   
      pred_observers$join <- 1
      pred_observers$observer_id <- factor(as.character(pred_observers$observer_id))
      # join with dataframe of standardised checklists (1hr, 1km, 7am, 1 observer, travelling, mid-september, etc.)
      pred_group <- left_join(pred_observers, pred_1hr, by="join")
      
      # run prediction
      p_group <- predict(model_temp, newdata=pred_group, type='link', se.fit=TRUE)
      # extract variables we need
      pred_obs <- data.frame(observer_id=pred_group$observer_id, fit=p_group$fit, se=p_group$se.fit)
      # convert back from the log link - already done by type = link
      # pred_obs$est <- exp(pred_obs$fit)
      
      #variables for storing
      pred_obs$batch <- i
      pred_obs$observer_id <- as.character(pred_obs$observer_id)
      
      # find durations for each observer
      durations <- grouped_data[[i]] %>% select(observer_id, duration_minutes) %>% 
        group_by(observer_id) %>%
        # find duration
        summarise(duration = ifelse(max(duration_minutes >= 300), 
                                    300, max(duration_minutes)))
      
      # make df for predictions on each observer for each timestep (1 min : max_obs)
      pred_durations <- pred_group %>%
        left_join(durations, by = "observer_id") %>%
        select(-duration_minutes) %>%   # drop old static duration_minutes
        rowwise() %>%
        mutate(duration_minutes = list(1:duration)) %>%  # create list of durations
        unnest(duration_minutes) %>%    # unnest to expand
        ungroup()
      
      pred_t <- predict(model_temp, newdata=pred_durations, type='link', se.fit=TRUE )
      
      pred_time <- data.frame(observer_id = pred_durations$observer_id, duration_minutes = pred_durations$duration_minutes, fit = pred_t$fit)
      
      # store all the predictions together
      if(i==1) all_pred <- pred_obs
      if(i>1) all_pred <- rbind(all_pred, pred_obs)
      
      # and all the time predictions together
      if(i==1) all_time <- pred_time
      if(i>1) all_time <- rbind(all_time, pred_time)
    }
  } else {
    observer_groups <- do.call(rbind, lapply(observer_groups, as.data.frame))
    
    # we factor the observer ID's now for the random effect
    observer_groups$observer_id <- as.factor(observer_groups$observer_id)
    
    # run the model
    # model specification, basic SAC, increase with time
    model_temp <- bam(no.species ~ duration_minutes + I(sqrt(duration_minutes)) + 
                        # species availability covariates
                        pland_00_water + pland_01_evergreen_needleleaf + pland_03_deciduous_needleleaf + pland_04_deciduous_broadleaf + pland_05_mixed_forest + 
                        pland_08_woody_savanna + pland_09_savanna + pland_10_grassland + pland_12_cropland + pland_13_urban + 
                        pland_15_barren + GSI +
                        # effort expended for a given checklist
                        s(doy, bs = 'cc') + protocol_name + effort_distance_km + 
                        number_observers + s(tod, bs = 'tp') + 
                        I(log(checklist.no)) + 
                        # observer-specific effects for the intercept and the slope
                        s(observer_id, bs = 're') + 
                        s(observer_id, duration_minutes, bs = 're'), 
                      data = observer_groups, 
                      family = poisson, 
                      discrete = TRUE)
    
    # create prediction dataframe, with same checklist variables and different
    # observer ID's
    pred_observers <- data.frame(observer_id=unique(observer_groups$observer_id))   
    pred_observers$join <- 1
    pred_observers$observer_id <- factor(as.character(pred_observers$observer_id))
    # join with dataframe of standardised checklists (1hr, 1km, 7am, 1 observer, travelling, mid-september, etc.)
    pred_group <- left_join(pred_observers, pred_1hr, by="join")
    
    # run prediction
    p_group <- predict(model_temp, newdata=pred_group, type='link', se.fit=TRUE)
    # extract variables we need
    pred_obs <- data.frame(observer_id=pred_group$observer_id, fit = p_group$fit)
    # convert back from the log link
    pred_obs$est <- exp(pred_obs$fit)
    
    #variables for storing
    pred_obs$observer_id <- as.character(pred_obs$observer_id)
    
    # find durations for each observer
    durations <- observer_groups %>% select(observer_id, duration_minutes) %>% 
      group_by(observer_id) %>%
      # find duration
      summarise(duration = ifelse(max(duration_minutes >= 300), 
                                  300, max(duration_minutes)))
    
    # make df for predictions on each observer for each timestep (1 min : max_obs)
    pred_durations <- pred_group %>%
      left_join(durations, by = "observer_id") %>%
      select(-duration_minutes) %>%   # drop old static duration_minutes
      rowwise() %>%
      mutate(duration_minutes = list(0:duration)) %>%  # create list of durations
      unnest(duration_minutes) %>%    # unnest to expand
      ungroup()
    
    pred_t <- predict(model_temp, newdata=pred_durations, type='link', se.fit=TRUE )
    
    pred_time <- data.frame(observer_id = pred_durations$observer_id, duration_minutes = pred_durations$duration_minutes, fit = pred_t$fit)
    
    # store all the predictions together, matching alternate format above
    all_pred <- pred_obs
    
    # store all the predictions together, matching alternate format above
    all_time <- pred_time
  }
  
  # save this before I lose it
  all_pred %>% write_csv(paste0(data_path, data_tag, '_SAC.csv'))
  all_time %>% write_csv(paste0(data_path, data_tag, '_time.csv'))
  
  if (str_detect(data_tag, 'full')){
  
    # number of observers predicted for
    n_rows = length(all_pred$fit)
  
    # top and bottom 25% of observer scores to compare for species
    top_quantile <- all_pred %>% arrange(fit) %>% filter(row_number() > n_rows*0.75)
    bottom_quantile <- all_pred %>% arrange(fit) %>% filter(row_number() < n_rows*0.25 + 1)
  
    # checklists submitted by the top and bottom 25% of observer scores
    top_checklists <- data %>% filter(observer_id %in% top_quantile$observer_id)
    bottom_checklists <- data %>% filter(observer_id %in% bottom_quantile$observer_id)
  
    # species in the top and bottom checklists
    top_species <- unique(top_checklists$scientific_name)
    bottom_species <- unique(bottom_checklists$scientific_name)
    # all species listed
    all_species <- unique(data$scientific_name)
  
    # function to return the proportion of checklists containing a specific bird
    zeros <- function(species, ebird, ...){
      ebird <- ebird %>%
        group_by(checklist_id, observer_id) %>%
        mutate(obs = ifelse(any(scientific_name == species), 1, 0)) %>%
        ungroup() %>%
        distinct(checklist_id, .keep_all = TRUE)
      prop <- length(ebird$obs[ebird$obs == 1])/length(ebird$obs)
      return(prop)
    }
  
    all_prop <- sapply(all_species, zeros, ebird = data)
    all <- data.frame(species = all_species, prop = all_prop)
    all_filter <- all %>% filter(prop >= 0.01)
    target_species <- unique(all_filter$species)
  
    top_prop <- sapply(target_species, zeros, ebird = top_checklists)
    bottom_prop <- sapply(target_species, zeros, ebird = bottom_checklists)
  
    top <- data.frame(species = target_species, prop = top_prop)
    bottom <- data.frame(species = target_species, prop = bottom_prop)
  
    ## THESE BOOTS ARE MADE FOR STRAPPIN'
    top_observers <- top_quantile$observer_id
    bottom_observers <- bottom_quantile$observer_id
  
    for (j in 1:200){
      print(paste('Bootstrap no.', j))
      # Resample Observers
      observers_top <- sample(top_observers, size = length(top_observers), replace = TRUE)
      observers_bottom <- sample(bottom_observers, size = length(bottom_observers), replace = TRUE)
  
      # Obtain checklists for those observers
      checklists_top <- top_checklists %>% filter(observer_id %in% observers_top)
      checklists_bottom <- bottom_checklists %>% filter(observer_id %in% observers_bottom)
  
      # Calculate species reporting rate for subsample for species in top/bottom
      prop_top <- sapply(target_species, zeros, ebird = checklists_top)
      prop_bottom <- sapply(target_species, zeros, ebird = checklists_bottom)
  
      # Append result as new row so each column is species
      if(j==1) {
        bootstrap_top <- prop_top
        bootstrap_bottom <- prop_bottom
      }
      if(j>1){
        bootstrap_top <- rbind(bootstrap_top, prop_top)
        bootstrap_bottom <- rbind(bootstrap_bottom, prop_bottom)
      }
    }
  
    top_df <- bootstrap_top %>% as.data.frame()
    bottom_df <- bootstrap_bottom %>% as.data.frame()
  
    top_df <- top_df %>% pivot_longer(cols = everything(), names_to = 'species', values_to = 'Metric') %>%
      group_by(species) %>%
      summarise(Mean = mean(Metric),
                SE = std.error(Metric)) %>% mutate(group = 'top')
  
    bottom_df <- bottom_df %>% pivot_longer(cols = everything(), names_to = 'species', values_to = 'Metric') %>%
      group_by(species) %>%
      summarise(Mean = mean(Metric),
                SE = std.error(Metric)) %>% mutate(group = 'bottom')
  
    all_df <- rbind(top_df, bottom_df)
  
    all_df <- all_df %>% pivot_wider(values_from = c(Mean, SE), names_from = group) %>%
      mutate(diff = Mean_top - Mean_bottom,
             ratio = Mean_bottom/Mean_top,
             SE_ratio = sqrt((SE_top/Mean_top)^2 + (SE_bottom/Mean_bottom)^2)*ratio,
             SE_diff = sqrt(SE_top^2 + SE_bottom^2),
             diff_new =  abs(ratio - 1))
  
    common_names <- ebd %>% select(species = scientific_name, common_name) %>% distinct()
    all_df <- all_df %>% left_join(common_names, by = 'species')
  
    # save this before I lose it too
    all_df %>% write_csv(paste0(data_path, data_tag, '_SAC_species.csv'))
    
  } else if (str_detect(data_tag, 'new')){
    # same approach as above but without splitting into quantiles as new observers
    # don't have the same layout of novice to expert
    
    # number of observers predicted for
    n_rows = length(all_pred$fit)
    
    # all species listed
    all_species <- unique(data$scientific_name)
    
    # function to return the proportion of checklists containing a specific bird
    zeros <- function(species, ebird, ...){
      ebird <- ebird %>%
        group_by(checklist_id, observer_id) %>%
        mutate(obs = ifelse(any(scientific_name == species), 1, 0)) %>%
        ungroup() %>%
        distinct(checklist_id, .keep_all = TRUE)
      prop <- length(ebird$obs[ebird$obs == 1])/length(ebird$obs)
      return(prop)
    }
    
    all_prop <- sapply(all_species, zeros, ebird = data)
    all <- data.frame(species = all_species, prop = all_prop)
    all_filter <- all %>% filter(prop >= 0.01)
    target_species <- unique(all_filter$species)
    
    prop <- sapply(target_species, zeros, ebird = data)
    
    result <- data.frame(species = target_species, prop = prop)
    
    ## THESE BOOTS ARE MADE FOR STRAPPIN'
    observers <- all_pred$observer_id
    
    for (j in 1:200){
      print(paste('Bootstrap no.', j))
      # Resample Observers
      observers_samp <- sample(observers, size = length(observers), replace = TRUE)
      
      # Obtain checklists for those observers
      checklists <- data %>% filter(observer_id %in% observers_samp)
      
      # Calculate species reporting rate for subsample for species in top/bottom
      prop <- sapply(target_species, zeros, ebird = checklists)
      
      # Append result as new row so each column is species
      if(j==1) {
        bootstrap <- prop
      }
      if(j>1){
        bootstrap <- rbind(bootstrap, prop)
      }
    }
    
    df <- bootstrap %>% as.data.frame()
    
    all_df <- df %>% pivot_longer(cols = everything(), names_to = 'species', values_to = 'Metric') %>%
      group_by(species) %>%
      summarise(Mean = mean(Metric),
                SE = std.error(Metric))
  
    common_names <- ebd %>% select(species = scientific_name, common_name) %>% distinct()
    all_df <- all_df %>% left_join(common_names, by = 'species')
    
    # save this before I lose it too
    all_df %>% write_csv(paste0(data_path, data_tag, '_SAC_species.csv'))
    
  }
}
