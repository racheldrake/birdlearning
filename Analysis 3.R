## ---------------------------
##
## Script name: Analysis 3
##
## Purpose of script: Modelling species count rate per hour
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

# where the saved datasets are
ebd_path <- 'analysis_3/data/'

# where I want to save results
results_path <- 'analysis_3/'

# FULL DATA ----------------

### DATA PREP ---------------------------

# load in model data
data <- read_csv(paste0(ebd_path, data_string, '_N_model.csv')) %>%
  mutate(merlin = factor(time, levels = c('pre', 'post'))) %>% select(-time)

# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(Index, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

# scale parameters, make species name factor for Random Effect
data <- data %>% group_by(common_name) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>% ungroup() %>% select(-n)

data$audio_index = (log(data$audio_index)) - min(log(data$audio_index))
data$Index = as.numeric(log(data$Index))
data$common_name = factor(as.character(data$common_name))

# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(Index, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

### MODEL ONE ----------------------------

# fit model with pre/post factor

fit <- glmer(N ~ Index + merlin + merlin:audio_index + (1|common_name),
           data = data,
           family = gaussian(link = 'log'))

summary(fit)

write_csv(tidy(fit), paste0(results_path, data_string, '_full_model_summary.csv'))

# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(Index = rep(mean(data$Index), 2*length(data$N)),
                        merlin = c(rep('pre', length(data$N)/2), rep('post', length(data$N)/2), rep('pre', length(data$N)/2), rep('post', length(data$N)/2)),
                        audio_index = c(rep(quantile(data$audio_index, 0.1), length(data$N)), rep(quantile(data$audio_index, 0.9), length(data$N))),
                        audio_type = c(rep('low', length(data$N)), rep('high', length(data$N))),
                        common_name = rep(unique(data$common_name), 4))

pred_data$common_name <- as.factor(pred_data$common_name)
pred_data$merlin <- as.factor(pred_data$merlin)

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response')

pred_data <- pred_data %>%
  pivot_wider(names_from = 'merlin', values_from = prediction)

extended_lines <- pred_data %>%
  group_by(audio_type) %>%
  summarise(
    audio_index = min(audio_index),
    slope = coef(lm(post ~ pre))[2],
    intercept = coef(lm(post ~ pre))[1],
    .groups = "drop"
  ) %>%
  # Extend x range manually
  mutate(
    pre_min = exp(log(min(data$N[data$merlin == 'pre']))),
    pre_max = exp(log(max(data$N[data$merlin == 'pre']))),
    post_min = intercept + slope * pre_min,
    post_max = intercept + slope * pre_max
  ) %>% pivot_longer(cols = c(pre_min, pre_max, post_min, post_max), names_to = 'ignore', values_to = 'values') %>%
  mutate(type = ifelse(str_detect(ignore, 'pre'), 'pre', 'post'),
         type2 = ifelse(str_detect(ignore, 'min'), 'min', 'max')) %>%
  select(-ignore) %>%
  pivot_wider(names_from = 'type', values_from = 'values')

data_plot <- data %>% select(-Index) %>%
  mutate(merlin = as.character(merlin)) %>% 
  pivot_wider(names_from = merlin, values_from = N) 

audio_shift <- ggplot() + 
  geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index), data = extended_lines) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  coord_equal(xlim = c(0, 13), ylim = c(0, 13)) + 
  labs(x = 'Average count per hour 2016-2019', y = 'Average count per hour 2022-2024', colour = 'Audio Index') + 
  ggtitle('Average count per hour for all observers')

ggsave(paste0(results_path, data_string, '_audio_shift_full.png'), audio_shift, width = 6, height = 6)

audio_shift_log <- ggplot() + 
  geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index), data = extended_lines) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Average count per hour 2016-2019', y = 'Average count per hour 2022-2024', colour = 'Audio Index') + 
  ggtitle('Average count per hour for all observers')

ggsave(paste0(results_path, data_string, '_audio_shift_full_log.png'), audio_shift_log, width = 6, height = 6)

# Predict over changing pre/post merlin for all audio index levels
# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(Index = rep(mean(data$Index), 1000),
                        merlin = c(rep('pre', 500), rep('post', 500)),
                        audio_index = rep(seq(min(data$audio_index), max(data$audio_index), length.out = 500), 2),
                        common_name = rep('new_species', 1000))

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response', allow.new.levels = TRUE)

audio_continuous <- ggplot(pred_data) + 
  geom_line(aes(audio_index, prediction, colour = merlin), linewidth = 0.7) + 
  theme_bw() + xlab('Audio Index') + ylab('Expected N') + 
  labs(colour = 'Time Period') + 
  scale_colour_discrete(labels = c('pre' = 'Pre Merlin', 
                                   'post' = 'Post Merlin')) + 
  coord_cartesian(ylim = c(0, max(pred_data$prediction)), xlim = c(0, max(pred_data$audio_index))) +
  ggtitle('Expected count on checklist wrt audio index pre/post Merlin')

ggsave(paste0(results_path, data_string, '_audio_continuous_full.png'), audio_continuous, width = 6, height = 4)

# Predict over changing pre/post merlin for all audio index levels
# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(Index = c(rep(quantile(data$Index, 0.1), 1000), rep(median(data$Index), 1000), rep(quantile(data$Index, 0.9), 1000)),
                        Index_level = c(rep('10th Quantile', 1000), rep('Median', 1000), rep('90th Quantile', 1000)),
                        merlin = rep(c(rep('pre', 500), rep('post', 500)), 3),
                        audio_index = rep(rep(seq(min(data$audio_index), max(data$audio_index), length.out = 500), 2), 3),
                        common_name = rep('new_species', 3000))

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response', allow.new.levels = TRUE)

audio_continuous_level <- ggplot(pred_data) + 
  geom_line(aes(audio_index, prediction, colour = merlin), linewidth = 0.7) + 
  theme_bw() + xlab('Audio Index') + ylab('Expected N') + 
  labs(colour = 'Time Period') + 
  scale_colour_discrete(labels = c('pre' = 'Pre Merlin', 
                                   'post' = 'Post Merlin')) +
  coord_cartesian(ylim = c(0, max(pred_data$prediction)), xlim = c(0, max(pred_data$audio_index))) +
  ggtitle('Expected count on checklist wrt audio index pre/post Merlin across species level') + 
  facet_wrap(~Index_level)

ggsave(paste0(results_path, data_string, '_audio_continuous_level_full.png'), audio_continuous_level, width = 10, height = 4)



# NEW DATA ----------------
### DATA PREP ---------------------------

# load in model data
data <- read_csv(paste0(ebd_path, data_string, '_N_model_new.csv')) %>%
  mutate(merlin = factor(time, levels = c('pre', 'post'))) %>% select(-time)

# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(Index, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

# scale parameters, make species name factor for Random Effect
data <- data %>% group_by(common_name) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>% ungroup() %>% select(-n)

data$audio_index = (log(data$audio_index)) - min(log(data$audio_index))
data$Index = as.numeric(log(data$Index))
data$common_name = factor(as.character(data$common_name))

# histogram of data to check for skew
data %>% pivot_wider(names_from = merlin, values_from = N) %>% 
  pivot_longer(cols = c(Index, audio_index, pre, post), 
               names_to = 'vars', values_to = 'vals') %>%
  ggplot() + geom_histogram(aes(vals), bins = 60) + 
  facet_wrap(~vars, nrow = 2, ncol = 2, scales = 'free') + 
  theme_bw()

### MODEL ONE ----------------------------

# fit model with pre/post factor

fit <- glmer(N ~ Index + merlin + audio_index + merlin:audio_index + (1|common_name),
             data = data,
             family = gaussian(link = 'log'))

summary(fit)

write_csv(tidy(fit), paste0(results_path, data_string, '_new_model_summary.csv'))

# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(Index = rep(mean(data$Index), 2*length(data$N)),
                        merlin = c(rep('pre', length(data$N)/2), rep('post', length(data$N)/2), rep('pre', length(data$N)/2), rep('post', length(data$N)/2)),
                        audio_index = c(rep(quantile(data$audio_index, 0.1), length(data$N)), rep(quantile(data$audio_index, 0.9), length(data$N))),
                        audio_type = c(rep('low', length(data$N)), rep('high', length(data$N))),
                        common_name = rep(unique(data$common_name), 4))

pred_data$common_name <- as.factor(pred_data$common_name)
pred_data$merlin <- as.factor(pred_data$merlin)

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response')

pred_data <- pred_data %>%
  pivot_wider(names_from = 'merlin', values_from = prediction)

extended_lines <- pred_data %>%
  group_by(audio_type) %>%
  summarise(
    audio_index = min(audio_index),
    slope = coef(lm(post ~ pre))[2],
    intercept = coef(lm(post ~ pre))[1],
    .groups = "drop"
  ) %>%
  # Extend x range manually
  mutate(
    pre_min = exp(log(min(data$N[data$merlin == 'pre']))),
    pre_max = exp(log(max(data$N[data$merlin == 'pre']))),
    post_min = intercept + slope * pre_min,
    post_max = intercept + slope * pre_max
  ) %>% pivot_longer(cols = c(pre_min, pre_max, post_min, post_max), names_to = 'ignore', values_to = 'values') %>%
  mutate(type = ifelse(str_detect(ignore, 'pre'), 'pre', 'post'),
         type2 = ifelse(str_detect(ignore, 'min'), 'min', 'max')) %>%
  select(-ignore) %>%
  pivot_wider(names_from = 'type', values_from = 'values')

data_plot <- data %>% select(-Index) %>%
  mutate(merlin = as.character(merlin)) %>% 
  pivot_wider(names_from = merlin, values_from = N) 

audio_shift <- ggplot() + 
  geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index), data = extended_lines) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  coord_equal(xlim = c(0, 13), ylim = c(0, 13)) + 
  labs(x = 'Average count per hour 2016-2019', y = 'Average count per hour 2022-2024', colour = 'Audio Index') + 
  ggtitle('Average count per hour for all observers')

ggsave(paste0(results_path, data_string, '_audio_shift_new.png'), audio_shift, width = 6, height = 6)

audio_shift_log <- ggplot() + 
  geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index), data = extended_lines) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Average count per hour 2016-2019', y = 'Average count per hour 2022-2024', colour = 'Audio Index') + 
  ggtitle('Average count per hour for all observers')

ggsave(paste0(results_path, data_string, '_audio_shift_new_log.png'), audio_shift_log, width = 6, height = 6)

# Predict over changing pre/post merlin for all audio index levels
# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(Index = rep(mean(data$Index), 1000),
                        merlin = c(rep('pre', 500), rep('post', 500)),
                        audio_index = rep(seq(min(data$audio_index), max(data$audio_index), length.out = 500), 2),
                        common_name = rep('new_species', 1000))

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response', allow.new.levels = TRUE)

audio_continuous <- ggplot(pred_data) + 
  geom_line(aes(audio_index, prediction, colour = merlin), linewidth = 0.7) + 
  theme_bw() + xlab('Audio Index') + ylab('Expected N') + 
  labs(colour = 'Time Period') + 
  scale_colour_discrete(labels = c('pre' = 'Pre Merlin', 
                                   'post' = 'Post Merlin')) + 
  coord_cartesian(ylim = c(0, max(pred_data$prediction)), xlim = c(0, max(pred_data$audio_index))) +
  ggtitle('Expected count on checklist wrt audio index pre/post Merlin')

ggsave(paste0(results_path, data_string, '_audio_continuous_new.png'), audio_continuous, width = 6, height = 4)

# Predict over changing pre/post merlin for all audio index levels
# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(Index = c(rep(quantile(data$Index, 0.1), 1000), rep(median(data$Index), 1000), rep(quantile(data$Index, 0.9), 1000)),
                        Index_level = c(rep('10th Quantile', 1000), rep('Median', 1000), rep('90th Quantile', 1000)),
                        merlin = rep(c(rep('pre', 500), rep('post', 500)), 3),
                        audio_index = rep(rep(seq(min(data$audio_index), max(data$audio_index), length.out = 500), 2), 3),
                        common_name = rep('new_species', 3000))

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response', allow.new.levels = TRUE)

audio_continuous_level <- ggplot(pred_data) + 
  geom_line(aes(audio_index, prediction, colour = merlin), linewidth = 0.7) + 
  theme_bw() + xlab('Audio Index') + ylab('Expected N') + 
  labs(colour = 'Time Period') + 
  scale_colour_discrete(labels = c('pre' = 'Pre Merlin', 
                                   'post' = 'Post Merlin')) + 
  coord_cartesian(ylim = c(0, max(pred_data$prediction)), xlim = c(0, max(pred_data$audio_index))) +
  ggtitle('Expected count on checklist wrt audio index pre/post Merlin across species level') + 
  facet_wrap(~Index_level)

ggsave(paste0(results_path, data_string, '_audio_continuous_level_new.png'), audio_continuous_level, width = 10, height = 4)


