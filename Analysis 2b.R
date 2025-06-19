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
library(glmmTMB)

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
results_path <- 'analysis_2b/'

# coef_labels <- c('int' = 'Random Effect Observer', 'slope' = 'Random Effect Observer:Duration')
dataset_labels <- c('bcr23_2025_2016_19_full' = '2016-2019', 'bcr23_2025_2022_24_full' = '2022-2024', 'bcr23_2025_2016_19_new' = '2016-2019', 'bcr23_2025_2022_24_new' = '2022-2024')
quantile_labels <- c('Mean_bottom' = 'Lower Quantile of Observers', 'Mean_top' = 'Upper Quantile of Observers')

# load in audio index
audio_index <- read_csv(paste0(ebd_path, 'audio_index.csv'))

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

model_data <- SAC_data %>%
  select(species, ratio, common_name, dataset) %>%
  mutate(merlin = ifelse(str_detect(dataset, '2016_19'), 'pre', 'post')) %>%
  select(-dataset) %>%
  group_by(common_name) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>% ungroup() %>% select(-n) %>%
  left_join(audio_index, by = 'common_name') %>%
  mutate(audio_index = log(audio_index) - min(log(audio_index))) %>%
  select(-scientific_name)


# model with audio index

fit <- glmmTMB(ratio ~ merlin*audio_index + (1 | common_name),
  data = model_data,
  family = beta_family())

summary(fit)

# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(merlin = c(rep('pre', length(model_data$common_name)/2), rep('post', length(model_data$common_name)/2), rep('pre', length(model_data$common_name)/2), rep('post', length(model_data$common_name)/2)),
                        audio_index = c(rep(quantile(model_data$audio_index, 0.1), length(model_data$common_name)), rep(quantile(model_data$audio_index, 0.9), length(model_data$common_name))),
                        audio_type = c(rep('low', length(model_data$common_name)), rep('high', length(model_data$common_name))),
                        common_name = rep(unique(model_data$common_name), 4))

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
    pre_min = exp(log(min(model_data$ratio[model_data$merlin == 'pre']))),
    pre_max = exp(log(max(model_data$ratio[model_data$merlin == 'pre']))),
    post_min = intercept + slope * pre_min,
    post_max = intercept + slope * pre_max
  ) %>% pivot_longer(cols = c(pre_min, pre_max, post_min, post_max), names_to = 'ignore', values_to = 'values') %>%
  mutate(type = ifelse(str_detect(ignore, 'pre'), 'pre', 'post'),
         type2 = ifelse(str_detect(ignore, 'min'), 'min', 'max')) %>%
  select(-ignore) %>%
  pivot_wider(names_from = 'type', values_from = 'values')

data_plot <- model_data %>%
  mutate(merlin = as.character(merlin)) %>% 
  pivot_wider(names_from = merlin, values_from = ratio) 

audio_shift <- ggplot() + 
  geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index), data = extended_lines) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + 
  labs(x = 'Novice-expert ratio 2016-2019', y = 'Novice-expert ratio 2022-2024', colour = 'Audio Index') + 
  ggtitle('Novice-expert ratio across time periods')

ggsave(paste0(results_path, data_string, '_audio_shift.png'), audio_shift, width = 6, height = 6)

audio_shift_log <- ggplot() + 
  geom_point(aes(pre, post, colour = audio_index), data = data_plot) + 
  geom_line(aes(pre, post, group = audio_type, colour = audio_index), data = extended_lines) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_colour_viridis_c() + 
  scale_x_log10() +
  scale_y_log10() +
  labs(x = 'Novice-expert ratio 2016-2019', y = 'Novice-expert ratio 2022-2024', colour = 'Audio Index') + 
  ggtitle('Novice-expert ratio across time periods')

ggsave(paste0(results_path, data_string, '_audio_shift_log.png'), audio_shift_log, width = 6, height = 6)




# Predict over changing pre/post merlin for all audio index levels
# predict for merlin and no merlin, no trend, constant audio at two levels
pred_data <- data.frame(merlin = c(rep('pre', 500), rep('post', 500)),
                        audio_index = rep(seq(min(model_data$audio_index), max(model_data$audio_index), length.out = 500), 2),
                        common_name = rep('new_species', 1000))

pred_data$prediction <- predict(fit, newdata = pred_data, type = 'response', allow.new.levels = TRUE)

audio_continuous <- ggplot(pred_data) + 
  geom_line(aes(audio_index, prediction, colour = merlin), linewidth = 0.7) + 
  theme_bw() + xlab('Audio Index') + ylab('Ratio of novice to expert') + 
  labs(colour = 'Time Period') + 
  scale_colour_discrete(labels = c('pre' = 'Pre Merlin', 
                                   'post' = 'Post Merlin')) + 
  coord_cartesian(ylim = c(0, max(pred_data$prediction)), xlim = c(0, max(pred_data$audio_index))) +
  ggtitle('Novice-expert ratio wrt audio index pre/post Merlin')

ggsave(paste0(results_path, data_string, '_audio_continuous.png'), audio_continuous, width = 6, height = 4)


