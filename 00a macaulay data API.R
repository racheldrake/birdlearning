## ---------------------------
##
## Script name: Macaulay library API
##
## Purpose of script: extract numbers of photos and audio recordings from
##            the macaulay online library in order to construct an audio index
##
## Author: Rachel Drake
##
## Date Created: 08/05/2025
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
library(remotes)
# make sure we have latest eBird taxonomy by installing from github
install_github("CornellLabofOrnithology/auk")
library(auk)
library(RCurl)
library(XML)
library(tidyverse)
library(jpeg)
library(httr)
library(chromote)
library(rvest)
library(stringr)

# resolve namespace conflicts
select <- dplyr::select

## ---------------------------

# prep all of our data locations
data_path <- 'data/'

# grab species codes from eBird (these are the ML page links)
species_codes <- ebird_taxonomy %>% 
  filter(category == "species") %>% 
  pull(species_code)

scrape_data <- function(species_code){
  # function to take the data we want from the ML pages
  
  # need to live read as the numbers aren't static
  webpage <- rvest::read_html_live(paste0("https://search.macaulaylibrary.org/catalog?taxonCode=",species_code))
  # find the exact location on the webpage, correct as of Feb 2025
  numbers <- webpage %>% 
    html_nodes("span") %>% 
    toString() %>% 
    str_extract_all(pattern = "</span> <span data-v-804f50dc=\"\" class=\"\">[^<]+", simplify = TRUE) %>% 
    # clean up to get as a single number
    str_remove_all(pattern = "</span> <span data-v-804f50dc=\"\" class=\"\">") %>% 
    str_remove_all(pattern = ",") %>% 
    as.integer()
  # save to a table
  write.table(matrix(c(species_code, numbers), nrow = 1), file = 'macaulay_api2.csv', append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)
}

# if API fails then add an empty row for the species
scrape_data_fail <- function(species_code){
  write.table(matrix(c(species_code, NA, NA, NA), nrow = 1), file = 'macaulay_api2.csv', append = TRUE, sep = ',', row.names = FALSE, col.names = FALSE)
}

# run for all species codes
for (i in 1:length(species_codes)){
  # use tryCatch as fail rate can be high
  tryCatch({scrape_data(species_codes[i])},
           error = function (e){scrape_data_fail(species_codes[i])})
 
}

remaining <- is.na(species_data[,2])
remaining <- species_codes[remaining]

# do another loop for missing species
for (i in 1:length(remaining)){
  tryCatch({scrape_data(remaining[i])},
           error = function (e){scrape_data_fail(remaining[i])})
  
}

# append missing species to main dataframe
for (i in 1:nrow(species_data)){
  if (is.na(species_data[i, 2])){ 
    species_data[i,] <- extra_data[extra_data$species == species_data[i, 1],]
  }
}

# I think I still had 10 or so species that had failed which I went in
# and edited manually, this file is uploaded though
species_data %>% write_csv(paste0(data_path, 'macaulay_complete.csv'))

