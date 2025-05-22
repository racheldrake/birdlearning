## ---------------------------
##
## Script name: Analysis 1
##
## Purpose of script: Comparing observer CCI random effects
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
results_path <- 'analysis_1/'

data_tags <- paste0(data_string, c('_2016_19_full', '_2022_24_full', '_2016_19_new', '_2022_24_new'))

coef_labels <- c('full' = 'All Observers', 'new' = 'New Observers')
dataset_labels <- c('bcr23_2025_2016_19_full' = '2016-2019', 'bcr23_2025_2022_24_full' = '2022-2024', 'bcr23_2025_2016_19_new' = '2016-2019', 'bcr23_2025_2022_24_new' = '2022-2024')

#- time data analysis                                                            ####

# each dataset is stored differently so I want to load and combine them
time_data = NULL
for (data_tag in data_tags){
  # load RE data
  time_data_data <- read_csv(paste0(data_path, data_tag, '_time.csv')) %>% 
    mutate(dataset = data_tag)
  time_data <- rbind(time_data, time_data_data)
}

time_data$fit <- exp(time_data$fit)

# plot for the full model, with a line for each observer
full_lines <- time_data %>% filter(str_ends(dataset, 'full')) %>% 
  ggplot() + 
  geom_line(aes(duration_minutes, fit, group = observer_id), alpha = 0.1) + 
  facet_wrap(vars(dataset), labeller = as_labeller(dataset_labels)) + theme_bw() + 
  coord_fixed(ratio = 2) + 
  labs(x = 'Duration (Minutes)', y = 'Estimated No. Species') + ggtitle('Species accumulation curves for all observers')

# plot for the new model, with a line for each observer
new_lines <- time_data %>% 
  filter(str_ends(dataset, 'new')) %>% ggplot() + 
  geom_line(aes(duration_minutes, fit, group = observer_id), alpha = 0.1) + 
  facet_wrap(vars(dataset), labeller = as_labeller(dataset_labels)) + theme_bw() + 
  coord_fixed(ratio = 1.8) + 
  labs(x = 'Duration (Minutes)', y = 'Estimated No. Species') + ggtitle('Species accumulation curves for new observers')

# summary plot for both full and new model
lines_iqr <- time_data %>% 
  mutate(type = ifelse(str_detect(dataset, 'full'), 'full', 'new'),
         time = ifelse(str_detect(dataset, '2016'), '2016-19', '2022-24')) %>%
  group_by(type, time, duration_minutes) %>%
  summarise(lowest = min(fit),
            low = quantile(fit, 0.25),
            mid = median(fit),
            high = quantile(fit, 0.75),
            highest = max(fit)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = duration_minutes, ymin = low, ymax = high, fill = time), position = 'identity', alpha = 0.2, colour = NA) +
  geom_line(aes(duration_minutes, mid, colour = time), linewidth = 1, alpha = 2) +
  facet_wrap(vars(type), labeller = as_labeller(coef_labels)) + theme_bw() + 
  coord_fixed(4.5) +
  geom_vline(xintercept = 60, linetype = 'dashed', colour = 'darkgrey') + 
  scale_colour_manual(values = c("2022-24" = "#003366",
                                 "2016-19" = "#660033")) +  
  labs(x = 'Duration (Minutes)', y = 'Estimated No. Species', fill = 'Time Period', colour = 'Time Period') + ggtitle('Average species accumulation curve for all observers')


# same as the histogram plot at just 60 minutes below but at each time interval
# from 1 to 120 minutes - I then externally transform this into a gif
for (i in 1:120){
  means <- time_data %>%
    filter(duration_minutes == i) %>%
    mutate(subset = ifelse(str_detect(dataset, 'full'), 'full', 'new'),
           time = ifelse(str_detect(dataset, '2016_19'), 'pre', 'post')) %>%
    group_by(subset, time) %>%
    summarise(mean_fit = mean(fit),
              low_q = quantile( fit, 0.25),
              up_q = quantile( fit, 0.75), .groups = "drop")
  
  hist <- time_data %>%
    filter(duration_minutes == i) %>%
    mutate(subset = ifelse(str_detect(dataset, 'full'), 'full', 'new'),
           time = ifelse(str_detect(dataset, '2016_19'), 'pre', 'post')) %>%
    ggplot() + 
    geom_vline(data = means,
               aes(xintercept = mean_fit, group = time),
               colour = c("pre" = "#660033", "post" = "#003366")[means$time],
               show.legend = FALSE, linetype = 'dashed') +
    # geom_rect(data = means,
    #           aes(xmin = low_q, xmax = up_q, ymin = -Inf, ymax = Inf),
    #           fill = c("pre" = "#F8766D", "post" = "#00BFC4")[means$time],
    #           alpha = 0.1,
    #           inherit.aes = FALSE,
    #           show.legend = FALSE) +
    geom_density(aes(fit,
                     fill = time),
                 position = 'identity', 
                 alpha = 0.3) + 
    facet_wrap(vars(subset), labeller = as_labeller(coef_labels)) + 
    xlim(c(0, 60)) + 
    ylim(c(0, 0.305)) +
    coord_cartesian(expand = FALSE) + 
    theme_bw() + labs(x = 'Checklist Calibration Index (CCI)', y = 'Density', fill = 'Time Period') + 
    ggtitle(paste0('Checklist Calibration Index (CCI) at ', i, ' minutes' )) + 
    scale_fill_manual(
      values = c("pre" = "#F8766D", "post" = "#00BFC4"),
      labels = c("pre" = "2016-2019", "post" = "2022-2024")
    )
    
  ggsave(paste0(results_path, data_string,'time_hist/', i, '_hist.png'), hist, width = 10, height = 5)
  
}


# loading checklist calibration index data same as the SAC data
index_data = NULL
for (data_tag in data_tags){
  # load RE data
  index_data_data <- read_csv(paste0(data_path, data_tag, '_SAC.csv')) %>% 
    mutate(dataset = data_tag)
  index_data <- rbind(index_data, index_data_data)
}
index_data$fit <- exp(index_data$fit)

# plot of histograms comparing across pre/post and full/new
hist <- index_data %>%
  mutate(subset = ifelse(str_detect(dataset, 'full'), 'full', 'new'),
         time = ifelse(str_detect(dataset, '2016_19'), 'pre', 'post')) %>%
  ggplot() + 
  geom_density(aes(fit,
                   fill = time),
               position = 'identity', 
               alpha = 0.3) + 
  facet_wrap(vars(subset), labeller = as_labeller(coef_labels)) + 
  coord_cartesian(expand = FALSE, ylim = c(0, 0.08)) + 
  theme_bw() + labs(x = 'Checklist Calibration Index (CCI)', y = 'Density', fill = 'Time Period') + 
  ggtitle('Checklist Calibration Index (CCI) estimates for all and new observers') + 
  scale_fill_manual(
    values = c("pre" = "#F8766D", "post" = "#00BFC4"),
    labels = c("pre" = "2016-2019", "post" = "2022-2024")
  )

# save all results
ggsave(paste0(results_path, data_string, '_full_lines.png'), full_lines, width = 8, height = 5)
ggsave(paste0(results_path, data_string, '_new_lines.png'), new_lines, width = 8, height = 5)
ggsave(paste0(results_path, data_string, '_lines_IQR.png'), lines_iqr, width = 8, height = 5)
ggsave(paste0(results_path, data_string, 'hist.png'), hist, width = 10, height = 5)

# table with data summary plotted for reference
time_data %>%
  group_by(dataset, duration_minutes) %>%
  summarise(lowest = min(fit),
            low = quantile(fit, 0.25),
            mid = median(fit),
            high = quantile(fit, 0.75),
            highest = max(fit)) %>% write_csv(paste0(results_path, data_string, 'SAC_time.csv' ))

# table with data summary for reference
index_data %>%
  group_by(dataset) %>% 
  summarise(lowest = min(fit),
            low = quantile(fit, 0.25),
            mid = median(fit),
            high = quantile(fit, 0.75),
            highest = max(fit), 
            mean = mean(fit)) %>% write_csv(paste0(results_path, data_string, 'SAC_index.csv' ))

