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
library(lme4)
library(broom.mixed)

# resolve namespace conflicts
select <- dplyr::select

# data tag
data_string <- "bcr23_2025"

# where I find my trend data
data_path <- 'stdata/' 

# eBird path
ebd_path <- 'proc_data/'

# where I want to save results
results_path <- 'analysis_3/'

# FULL DATA ----------------

### DATA PREP ---------------------------

# load in model data
data <- read_csv(paste0(results_path, data_string, '_N_model.csv')) %>%
  mutate(merlin = factor(time, levels = c('pre', 'post'))) %>% select(-time)

# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(trend, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

# scale parameters, make species name factor for Random Effect
data <- data %>% group_by(common_name) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>% ungroup() %>% select(-n)

data$audio_index = as.numeric(scale(log(data$audio_index)))
data$trend = as.numeric(scale(data$trend))
data$common_name = factor(as.character(data$common_name))



# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(trend, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

### MODEL ONE ----------------------------

# fit model with pre/post factor

fit <- glmer(N ~ trend + merlin + merlin:audio_index + audio_index + (1|common_name), 
           data = data,
           family = gaussian(link = 'log'))

# summary(fit)

write_csv(tidy(fit), paste0(results_path, data_string, '_full_model_summary.csv'))

# predict for merlin and no merlin, no trend, constant audio
pred_data <- data.frame(trend = rep(0, 2*length(data$N)),
                        merlin = c(rep('pre', length(data$N)/2), rep('post', length(data$N)/2), rep('pre', length(data$N)/2), rep('post', length(data$N)/2)),
                        audio_index = c(rep(quantile(data$audio_index, 0.1), length(data$N)), rep(quantile(data$audio_index, 0.9), length(data$N))),
                        audio_type = c(rep('low', length(data$N)), rep('high', length(data$N))),
                        common_name = rep(unique(data$common_name), 4))

pred_data$common_name <- as.factor(pred_data$common_name)
pred_data$merlin <- as.factor(pred_data$merlin)

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response')

data_plot <- data %>% pivot_wider(names_from = merlin, values_from = N)

audio_shift <- pred_data %>% 
  pivot_wider(names_from = merlin, values_from = prediction) %>%
  ggplot() +geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index)) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  coord_fixed(ratio = 1, xlim = c(0, 13), ylim = c(0, 13)) +
  labs(x = 'Average count per hour 2016-2019', y = 'Average count per hour 2022-2024', colour = 'Audio Index') + 
  ggtitle('Full data')

ggsave(paste0(results_path, data_string, '_audio_shift_full.png'), audio_shift, width = 6, height = 6)


# NEW DATA ----------------

### DATA PREP ---------------------------


# load in model data
data <- read_csv(paste0(results_path, data_string, '_N_model_new.csv')) %>%
  mutate(merlin = factor(time, levels = c('pre', 'post'))) %>% select(-time)

# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(trend, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

data <- data %>% group_by(common_name) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>% ungroup() %>% select(-n)

data$audio_index = as.numeric(scale(log(data$audio_index)))
data$trend = as.numeric(scale(data$trend))
data$common_name = factor(as.character(data$common_name))

# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(trend, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

### MODEL ONE ----------------------------

# fit model with pre/post factor

fit <- glmer(N ~ trend + merlin + merlin:audio_index + audio_index + (1|common_name), 
             data = data,
             family = gaussian(link = 'log'))

write_csv(tidy(fit), paste0(results_path, data_string, '_new_model_summary.csv'))

# predict for merlin and no merlin, no trend, constant audio
pred_data <- data.frame(trend = rep(0, 2*length(data$N)),
                        merlin = c(rep('pre', length(data$N)/2), rep('post', length(data$N)/2), rep('pre', length(data$N)/2), rep('post', length(data$N)/2)),
                        audio_index = c(rep(quantile(data$audio_index, 0.1), length(data$N)), rep(quantile(data$audio_index, 0.9), length(data$N))),
                        audio_type = c(rep('low', length(data$N)), rep('high', length(data$N))),
                        common_name = rep(unique(data$common_name), 4))

pred_data$common_name <- as.factor(pred_data$common_name)
pred_data$merlin <- as.factor(pred_data$merlin)

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response')

data_plot <- data %>% pivot_wider(names_from = merlin, values_from = N)

audio_shift <- pred_data %>% 
  pivot_wider(names_from = merlin, values_from = prediction) %>%
  ggplot() +geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index)) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10)) +
  labs(x = 'Average count per hour 2016-2019', y = 'Average count per hour 2022-2024', colour = 'Audio Index') + 
  ggtitle('New data')

ggsave(paste0(results_path, data_string, '_audio_shift_new.png'), audio_shift, width = 6, height = 6)
