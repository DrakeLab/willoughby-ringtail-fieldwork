# Zooniverse Pipeline

# set up libraries
library(tidyverse)
library(tidyjson)
library(jsonlite)
library(magrittr) #allows piping beyond tidyverse
library(lubridate)

# source functions 
source("scripts/functions/zoo-functions.R")

# load data 
## w_export <- read.csv("data/camera-data/zoo/canyon-critters-workflows_09March2023.csv") # subjects
## s_export <- read.csv("data/camera-data/zoo/canyon-critters-subjects_09March2023.csv") # subjects
## c_export <- read.csv("data/camera-data/zoo/canyon-critters-classifications_30Apr2023.csv") # classifications 
## launch1_classifications <- c_export %>% dplyr::filter(workflow_version =="404.38")
## launch1_subjects <- s_export %>% dplyr::filter(subject_id %in% launch1_classifications$subject_ids)
## write.csv(launch1_classifications, file = "data/camera-data/zoo/launch1_classifications.csv" )
## write.csv(launch1_subjects, file = "data/camera-data/zoo/launch1_subjects.csv") 

jdata <- read.csv("data/camera-data/zoo/launch1_classifications.csv", stringsAsFactors = F)
data<-choose_my_workflow(jdata)
# specify the workflow: 11938and version: 404.38 
flat_file<-flatten_json(jdata) #this is slow; could we write faster code?  A problem for another day.
saveRDS(flat_file, file = "data/camera-data/zoo/launch1_flatfile.Rdata") # save output -> don't want to have to re-run
# remove unneeded columns 
flat_file_sub <- flat_file %>%  
  select(-starts_with("filter"))
flat_file_sub$..JSON <- NULL

## SUBJECTS 
# get subject id per subject rather than subject-unique classification 
subj_id_string<-as.character(data$subject_ids)#makes a variable consisting of the strings to recognize.  
data$new_sub_data<-data$subject_data %>% str_replace(subj_id_string, "subject") #replace the strings with the word "subject"

# Parse subject data 
subjects<-data %>%
  select(., subject_ids, user_name, classification_id,
         workflow_version, subject_ids, new_sub_data) %>%
  as.tbl_json(json.column = "new_sub_data") %>%
  spread_values(
    id = jstring(subject,retired,id),
    class.count = jnumber(subject, retired, classifications_count),
    ImageFile1 = jstring(subject, ImageFile1),
    ImageDate1 = jstring(subject, ImageDate1),
    ImageFile2 = jstring(subject,ImageFile2),
    ImageDate2 = jstring(subject,ImageDate2)
  )

# remove unneeded cols 
subjects<-subjects[,-c(1,2,4),] #get rid of subject_ids, user_name, workflow_version
subjects$..JSON <-NULL
# merge subject ids and classifications 
flat_full <-left_join(flat_file_sub, subjects, by='classification_id')
write.csv(flat_full, file = "data/camera-data/zoo/launch1_output.csv")

## select out cols from launch1 that need JSON parsing 
launch1_js <- launch1 %>% dplyr::select(annotations)





## seperate the json data 
launch1_par <- launch1_js %>% 
  mutate(annotations = lapply(annotations, fromJSON)) %>%
  unnest_wider(annotations)



# Function to parse JSON data and ensure uniform structure
parse_json_and_fill <- function(json_str) {
  if (startsWith(json_str, '{')) {
    json <- fromJSON(json_str)
    max_keys <- max(sapply(json, length))
    for (key in names(json)) {
      if (length(json[[key]]) < max_keys) {
        json[[key]] <- c(json[[key]], rep(NA, max_keys - length(json[[key]])))
      }
    }
    return(json)
  } else {
    return(NULL)  # Return NULL for non-JSON strings
  }
}


# Apply the parsing and filling function to each column
launch1_par <- launch1_js %>%
  mutate(across(everything(), ~parse_json_and_fill(.x), .names = "parsed_{.col}"))


# Parse the JSON data in each column
parsed_data <- lapply(launch1 , function(x) sapply(x, function(y) fromJSON(y, simplifyVector = TRUE)))

# Find the maximum number of elements in any JSON column
max_length <- max(sapply(parsed_data, function(x) max(sapply(x, length))))

# Add missing elements (if any) to make each JSON column have the maximum length
for (col in names(parsed_data)) {
  df[[col]] <- lapply(parsed_data[[col]], function(x) c(x, rep(NA, max_length - length(x))))
}

# Combine all the JSON columns into one column
combined_json <- lapply(1:max_length, function(i) sapply(parsed_data, function(x) x[[i]]))
combined_json_df <- as.data.frame(combined_json)

# Rename the combined JSON columns as needed
colnames(combined_json_df) <- paste("json_element", 1:max_length, sep = "_")




# Function to parse JSON and ensure all keys are present
parse_json_and_fill <- function(x) {
  parsed_data <- fromJSON(x)
  max_keys <- max(sapply(parsed_data, length))
  for (i in seq_along(parsed_data)) {
    if (length(parsed_data[[i]]) < max_keys) {
      extra_keys <- setdiff(names(parsed_data[[i]]), names(parsed_data[[1]]))
      for (key in extra_keys) {
        parsed_data[[i]][[key]] <- NA
      }
    }
  }
  parsed_data
}

# Apply the parsing function to each JSON column
json_columns <- colnames(launch1)
for (col in json_columns) {
  launch1[[col]] <- lapply(launch1[[col]], parse_json_and_fill)
}

# Unnest the JSON data into separate columns
df <- df %>%
  unnest_wider(everything(), names_sep = "_")
# Apply the parsing function to the 'data' column
launch1_par <- launch1 %>%
  mutate(json_data = sapply(data, parse_json_or_na)) %>%
  unnest_wider(json_data)


# convert annotations out of JSON 
launch1_a <- launch1 %>%
  as.tbl_json(json.column = "annotations")
# bt_a <- tibble::rowid_to_column(bt_a, "row_n")

# extract out the species choice 
for(r in 1:nrow(launch1_a)){
  launch1_a$choice[r] <- launch1_a$..JSON[r][[1]][[1]]$value[[1]]$choice
}

# extract out the individual count 
# for(r in 1:nrow(bt_a)){
#  bt_a$individuals[r] <- bt_a$..JSON[r][[1]][[2]]$value[[3]]
# }

# select out the subjects and ids 
pic_ids <- select(launch1_a, subject_ids, choice)
pic_ids <- pic_ids %>% group_by(subject_ids, choice) %>%
  summarise(n = n())
pic_ids <- pic_ids %>% pivot_wider(names_from = choice, values_from = n)

for(r in 1:nrow(pic_ids)){
  pic_ids$species_ided[r] <- sum(is.na(pic_ids[r,2:40])==FALSE)
  pic_ids$community_id_count[r] <- colSums(pic_ids[,2:40])
}

