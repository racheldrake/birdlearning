## ---------------------------
##
## Script name: Analysis 2
##
## Purpose of script: Comparing SAC for species
##
## Author: Rachel Drake
##
## Date Created: 01/05/2025
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

library(tidyverse)

# resolve namespace conflicts
select <- dplyr::select

## ---------------------------

# data tag
data_string <- "bcr23_2025"

# where I find my output data
data_path <- 'SAC/' 

# eBird path
ebd_path <- 'proc_data/'

# where I want to save results
results_path <- 'analysis_2/'

# coef_labels <- c('int' = 'Random Effect Observer', 'slope' = 'Random Effect Observer:Duration')
dataset_labels <- c('bcr23_2025_2016_19_full' = '2016-2019', 'bcr23_2025_2022_24_full' = '2022-2024', 'bcr23_2025_2016_19_new' = '2016-2019', 'bcr23_2025_2022_24_new' = '2022-2024')
quantile_labels <- c('bottom' = 'Lower Quantile of Observers', 'top' = 'Upper Quantile of Observers')

# load in audio index
audio_index <- read_csv('audio_index.csv')
audio_index$audio_index <- log(audio_index$audio_index)
#- FULL ####

full_data_tags <- paste0(data_string, c('_2016_19_full', '_2022_24_full'))

# load in the bootstrapped species reporting rate results
SAC_data = NULL
for (data_tag in full_data_tags){
  # load RE data
  SAC_data_data <- read_csv(paste0(data_path, data_tag, '_SAC_species.csv')) %>% 
    mutate(dataset = data_tag)
  SAC_data <- rbind(SAC_data, SAC_data_data)
}

# summarise the 20 least similar species between novices and experts
least_similar_old <- SAC_data %>% filter(dataset == 'bcr23_2025_2016_19_full') %>% arrange(-diff_new) %>% slice_head(n = 20) %>% select(c(species_pre = species, common_name_pre = common_name, ratio_pre = ratio))
least_similar_new <- SAC_data %>% filter(dataset == 'bcr23_2025_2022_24_full') %>% arrange(-diff_new) %>% slice_head(n = 20) %>% select(c(species_post = species, common_name_post = common_name, ratio_post = ratio))
cbind(least_similar_old, least_similar_new) %>% write_csv(paste0(results_path, 'full_species_low.csv'))

# summarise the 20 most similar species between novices and experts
most_similar_old <- SAC_data %>% filter(dataset == 'bcr23_2025_2016_19_full') %>% arrange(diff_new) %>% slice_head(n = 20) %>% select(c(species_pre = species, common_name_pre = common_name, ratio_pre = ratio))
most_similar_new <- SAC_data %>% filter(dataset == 'bcr23_2025_2022_24_full') %>% arrange(diff_new) %>% slice_head(n = 20) %>% select(c(species_pre = species, common_name_pre = common_name, ratio_pre = ratio))
cbind(most_similar_old, most_similar_new) %>% write_csv(paste0(results_path, 'full_species_high.csv'))

# plot of the relative reporting rate for the 20 least similar species in 2016
least <- SAC_data %>% filter(species %in% least_similar_old$species_pre) %>% 
  mutate(ratio = bottom/top, # calculate relative reporting rate
         # factor the dataset variable so I can make everything plot the way I want
         dataset = factor(dataset, levels = c("bcr23_2025_2022_24_full", "bcr23_2025_2016_19_full"))) %>%
  # fill in blanks so that all the bars are the same size
  complete(common_name, dataset, fill = list(ratio = 0)) %>%
  # add an ordering variable so the bars are aesthetically pleasing
  group_by(common_name) %>%
  mutate(ordering_val = ratio[dataset == "bcr23_2025_2016_19_full"]) %>%
  ungroup() %>%
  # reorder to ~descend~
  mutate(common_name = fct_reorder(common_name, ordering_val, .desc = TRUE)) %>%
  # plot time
  ggplot(aes(y = reorder(common_name, ordering_val), x = ratio, fill = dataset)) +
  # dodge makes them plot next to each other nicely
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Relative reporting rate of bottom/top quantile", y = 'Species', fill = 'Years') +
  theme_minimal() +
  coord_cartesian(expand = FALSE, xlim = c(0, 0.32)) + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()  
  ) +
  # whole load of nonsense here to make the colours and values the right way round
  scale_fill_manual(
    breaks = c("bcr23_2025_2016_19_full", "bcr23_2025_2022_24_full"),  # legend order
    values = c(
      "bcr23_2025_2016_19_full" = "grey40",  # 2016–2019
      "bcr23_2025_2022_24_full" = "grey70"   # 2022–2024
    ),
    labels = c("2016–2019", "2022–2024")
  ) +
  ggtitle('Relative reporting rate for all observers: least similar species')

# exactly the same again but this time the 20 most similar species
most <- SAC_data %>% filter(species %in% most_similar_old$species_pre) %>% 
  mutate(ratio = bottom/top, # calculate relative reporting rate
         # factor the dataset variable so I can make everything plot the way I want
         dataset = factor(dataset, levels = c("bcr23_2025_2022_24_full", "bcr23_2025_2016_19_full"))) %>%
  # fill in blanks so that all the bars are the same size
  complete(common_name, dataset, fill = list(ratio = 0)) %>%
  # add an ordering variable so the bars are aesthetically pleasing
  group_by(common_name) %>%
  mutate(ordering_val = ratio[dataset == "bcr23_2025_2016_19_full"]) %>%
  ungroup() %>%
  # reorder to ~descend~
  mutate(common_name = fct_reorder(common_name, ordering_val, .desc = TRUE)) %>%
  # plot time
  ggplot(aes(y = reorder(common_name, ordering_val), x = ratio, fill = dataset)) +
  # dodge makes them plot next to each other nicely
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Relative reporting rate of bottom/top quantile", y = 'Species', fill = 'Years') +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  coord_cartesian(expand = FALSE) + 
  # whole load of nonsense here to make the colours and values the right way round
  scale_fill_manual(
    breaks = c("bcr23_2025_2016_19_full", "bcr23_2025_2022_24_full"),  # legend order
    values = c(
      "bcr23_2025_2016_19_full" = "grey40",  # 2016–2019
      "bcr23_2025_2022_24_full" = "grey70"   # 2022–2024
    ),
    labels = c("2016–2019", "2022–2024")
  ) +
  ggtitle('Relative reporting rate for all observers: most similar species')

# comparing the reporting rate for each quantile before/after merlin
point_full <- SAC_data %>%
  # combine quantiles into one variable
  pivot_longer(cols = c(top, bottom), names_to = "quantile", values_to = "SAC") %>%
  select(species, common_name, quantile, dataset, SAC) %>%
  # add the audio ID index for each species
  left_join(audio_index, by = 'common_name') %>%
  mutate(audio_index = as.numeric(scale(audio_index))) %>%
  # make each dataset its own column
  pivot_wider(names_from = dataset, values_from = SAC) %>% ggplot() +
  # datasets are axis
  geom_point(aes(bcr23_2025_2016_19_full, bcr23_2025_2022_24_full, colour = audio_index)) + 
  # each plot is quantile
  facet_wrap(vars(quantile), labeller = as_labeller(quantile_labels)) + theme_bw() +
  # nice 1-1 line to help
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  # make those audio ID colours friendly
  scale_colour_viridis_c() +
  scale_x_log10() + 
  scale_y_log10() +
  # make plots 1-1
  coord_fixed(ratio = 1) +
  labs(x = 'Reporting rate 2016-2019', y = 'Reporting rate 2022-2024', colour = 'Audio Index') + 
  ggtitle('Reporting rate comparison across years for full dataset')

# also the same as one above but now one plot and the axis are a ratio
point_full_combined <- SAC_data %>%
  # make the ratio
  mutate(SAC = bottom/top) %>%
  select(species, common_name, dataset, SAC) %>%
  # add in the audio index for each species
  left_join(audio_index, by = 'common_name') %>%
  mutate(audio_index = as.numeric(scale(audio_index))) %>%
  # make each dataset a column for the axis
  pivot_wider(names_from = dataset, values_from = SAC) %>% 
  # plot time
  ggplot() +
  geom_point(aes(bcr23_2025_2016_19_full, bcr23_2025_2022_24_full, colour = audio_index)) + theme_bw() +
  # another dashed line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  # make really square
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, ratio = 1) +
  # make audio colours pretty-ish
  scale_colour_viridis_c() + 
  labs(x = 'Relative reporting rate of bottom/top quantile 2016-2019', 
       y = 'Relative reporting rate of bottom/top quantile 2022-2024', 
       colour = 'Audio Index') + ggtitle('Relative reporting rate across years for full dataset')

# save everything
ggsave(paste0(results_path, data_string, '_least.png'), least, width = 8, height = 4)
ggsave(paste0(results_path, data_string, '_most.png'), most, width = 8, height = 4)
ggsave(paste0(results_path, data_string, '_point_full.png'), point_full, width = 10, height = 5)
ggsave(paste0(results_path, data_string, '_point_full_comb.png'), point_full_combined, width = 6, height = 6)

#- NEW ####

# this code is less commented because it's the same as the stuff above but I made
# comments after I'd copy and pasted it

# biggest difference is that we don't have quantiles so I just use the reporting
# rate for each species recorded in column `Mean`

# load in the bootstrapped species reporting rate results for the new datasets
new_data_tags <- paste0(data_string, c('_2016_19_new', '_2022_24_new'))

SAC_data = NULL
for (data_tag in new_data_tags){
  # load RE data
  SAC_data_data <- read_csv(paste0(data_path, data_tag, '_SAC_species.csv')) %>% 
    mutate(dataset = data_tag)
  SAC_data <- rbind(SAC_data, SAC_data_data)
}

# summarise the 20 least similar species between novices and experts
lowest_old <- SAC_data %>% filter(dataset == 'bcr23_2025_2016_19_new') %>% arrange(Mean) %>% slice_head(n = 20) %>% select(c(species_pre = species, common_name_pre = common_name, mean_pre = Mean))
lowest_new <- SAC_data %>% filter(dataset == 'bcr23_2025_2022_24_new') %>% arrange(Mean) %>% slice_head(n = 20) %>% select(c(species_post = species, common_name_post = common_name, mean_post = Mean))
cbind(lowest_old, lowest_new) %>% write_csv(paste0(results_path, 'new_species_low_RR.csv'))

# summarise the 20 least similar species between novices and experts
highest_old <- SAC_data %>% filter(dataset == 'bcr23_2025_2016_19_new') %>% arrange(-Mean) %>% slice_head(n = 20) %>% select(c(species_pre = species, common_name_pre = common_name, mean_pre = Mean))
highest_new <- SAC_data %>% filter(dataset == 'bcr23_2025_2022_24_new') %>% arrange(-Mean) %>% slice_head(n = 20) %>% select(c(species_post = species, common_name_post = common_name, mean_post = Mean))
cbind(highest_old, highest_new) %>% write_csv(paste0(results_path, 'new_species_high_RR.csv'))

# same plots as above but we have no ratio so we just use the reporting rate
new_low <- SAC_data %>% filter(species %in% lowest_old$species_pre) %>% 
  mutate(dataset = factor(dataset, levels = c("bcr23_2025_2022_24_new", "bcr23_2025_2016_19_new"))) %>%
  complete(common_name, dataset, fill = list(Mean = 0)) %>%
  group_by(common_name) %>%
  mutate(ordering_val = Mean[dataset == "bcr23_2025_2016_19_new"]) %>%
  ungroup() %>%
  mutate(common_name = fct_reorder(common_name, ordering_val, .desc = TRUE)) %>%
  ggplot(aes(y = reorder(common_name, ordering_val), x = Mean, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Reporting rate", y = 'Species', fill = 'Years') +
  theme_minimal() +
  coord_cartesian(expand = FALSE) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank() 
  ) + 
  scale_fill_manual(
    breaks = c("bcr23_2025_2016_19_new", "bcr23_2025_2022_24_new"),  # legend order
    values = c(
      "bcr23_2025_2016_19_new" = "grey40",  # 2016–2019
      "bcr23_2025_2022_24_new" = "grey70"   # 2022–2024
    ),
    labels = c("2016–2019", "2022–2024")
  ) +  # Custom colors for Low and High
  ggtitle('Reporting rate for new observers: least reported species in 2016-19')

# and again
new_high <- SAC_data %>% filter(species %in% highest_old$species_pre) %>% 
  mutate(dataset = factor(dataset, levels = c("bcr23_2025_2022_24_new", "bcr23_2025_2016_19_new"))) %>%
  complete(common_name, dataset, fill = list(Mean = 0)) %>%
  group_by(common_name) %>%
  mutate(ordering_val = Mean[dataset == "bcr23_2025_2016_19_new"]) %>%
  ungroup() %>%
  mutate(common_name = fct_reorder(common_name, ordering_val, .desc = TRUE)) %>%
  ggplot(aes(y = reorder(common_name, ordering_val), x = Mean, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(x = "Reporting rate", y = 'Species', fill = 'Years') +
  theme_minimal() +
  coord_cartesian(expand = FALSE) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank() 
  ) + 
  scale_fill_manual(
    breaks = c("bcr23_2025_2016_19_new", "bcr23_2025_2022_24_new"),  # legend order
    values = c(
      "bcr23_2025_2016_19_new" = "grey40",  # 2016–2019
      "bcr23_2025_2022_24_new" = "grey70"   # 2022–2024
    ),
    labels = c("2016–2019", "2022–2024")
  ) +  # Custom colors for Low and High
  ggtitle('Reporting rate for new observers: most reported species in 2016-19')

# obviously only one point graph as the second one for the full data was using 
# the ratio of quantiles which we don't have but this is otherwise the same
point_new <- SAC_data %>%
  select(species, common_name, dataset, Mean) %>%
  left_join(audio_index, by = 'common_name') %>%
  mutate(audio_index = as.numeric(scale(audio_index))) %>%
  pivot_wider(names_from = dataset, values_from = Mean) %>% ggplot() +
  geom_point(aes(bcr23_2025_2016_19_new, bcr23_2025_2022_24_new, colour = audio_index)) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  scale_x_log10() + 
  scale_y_log10() +
  coord_fixed(ratio = 1) +
  labs(x = 'Reporting rate 2016-2019', y = 'Reporting rate 2022-2024', colour = 'Audio Index') + 
  ggtitle('Reporting rate across years for new dataset')

# save it all again
ggsave(paste0(results_path, data_string, '_new_low.png'), new_low, width = 8, height = 4)
ggsave(paste0(results_path, data_string, '_new_high.png'), new_high, width = 8, height = 4)
ggsave(paste0(results_path, data_string, '_point_new.png'), point_new, width = 6, height = 6)
