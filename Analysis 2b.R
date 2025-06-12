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
quantile_labels <- c('Mean_bottom' = 'Lower Quantile of Observers', 'Mean_top' = 'Upper Quantile of Observers')

# load in audio index
audio_index <- read_csv(paste0(ebd_path, 'audio_index.csv'))
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

dat0 <- SAC_data |> 
    select(species, ratio, common_name, dataset) |>
    mutate(dataset = ifelse(dataset == "bcr23_2025_2016_19_full", "y2016", "y2022")) |>
    rename(scientific_name = species)


dat <- dat0 |> 
    left_join(audio_index) |>
    rename(year = dataset)


dat1 <- dat0 |> 
    pivot_wider(values_from = ratio, names_from = dataset) |>
    left_join(audio_index) |>
    filter(!is.na(y2016), !is.na(y2022))


# model with audio index

fit <- brm(
  formula = ratio ~ year*audio_index + (1 | common_name),
  data = dat,
  family = Beta(),
  chains = 4, cores = 4, iter = 10000
)

summary(fit)

coef <- summary(fit)$fixed[,"Estimate"]

ai <- seq(0, 9, by = 0.1)
mu_logit_2016 <- coef[1] + ai*coef[3]
mu_logit_2022 <- coef[1] + coef[2] + ai*(coef[3] + coef[4])

mu_2016 <- exp(mu_logit_2016)/(1+exp(mu_logit_2016))
mu_2022 <- exp(mu_logit_2022)/(1+exp(mu_logit_2022))

par(mfrow = c(1,1))
plot(ai, mu_2016, type = "l", ylim = c(0, 0.5), 
    xlab = "Audio Index", ylab = "Ratio novice to expert", lwd = 2)
lines(ai, mu_2022, col = 'red', lwd = 2)




mu_2016 <- seq(0.05, 0.95, by =0.05)

mu_logit_low <- -1.96 + 4.79*mu_2016 +0.03*ai_q[1] + 0.17*ai_q[1]*mu_2016
mu_low <- exp(mu_logit_low)/(1+exp(mu_logit_low))

mu_logit_high <- -1.96 + 4.79*mu_2016 +0.03*ai_q[2] + 0.17*ai_q[2]*mu_2016
mu_high <- exp(mu_logit_high)/(1+exp(mu_logit_high))

ggplot(dat, aes(x = y2016, y = y2022, color = audio_index)) + 
  geom_point() + 
  scale_color_viridis_c() + 
  theme_minimal() + 
  geom_line(data = data.frame(x = mu_2016, y = mu_logit_low), aes(x = x, y = y))








