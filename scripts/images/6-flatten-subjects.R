# flatten subjects to include metadata as columns 

# set up libraries
library(tidyverse)
#library(tidyjson)
library(jsonlite)
library(magrittr) #allows piping beyond tidyverse

# source functions 
source("scripts/images/functions/zoo-functions.R")

# read in the data
subjects <- read.csv(file = "data/camera-data/zoo/canyon-critters-subjects_28Jan2025.csv")

# Process metadata column and expand JSON into separate columns 
subjects_metadata <- subjects %>% 
  mutate(metadata_list = map(metadata, fromJSON)) %>%
  # Parse JSON into lists 
  select(-metadata) %>%
  # Remove original JSON column
  tidyr::unnest_wider(metadata_list, names_sep = "_") # Expand JSON fields into columns

# rename manifest columns 
# subjects_flat <- subjects_metadata[,c(1:4,25)]

# Process location column and expand JSON into separate columns 
subjects_location <- subjects %>% 
  mutate(location_list = map(locations, fromJSON)) %>%
  # Parse JSON into lists 
  select(-locations) %>%
  # Remove original JSON column
  tidyr::unnest_wider(location_list, names_sep = "_") # Expand JSON fields into columns
subjects_location <- subjects_location[,c(1:4,11:13)]

# merge unnested column dataframes together 
subjects_flat <- full_join(subjects_metadata, subjects_location, 
                           by = c("subject_id", "project_id", "workflow_id", "subject_set_id" ))

# save flattened subjects 
write.csv(subjects_flat, file = "data/camera-data/zoo/subjects_flat.csv")