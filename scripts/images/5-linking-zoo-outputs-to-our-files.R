# link zooniverse output to check event information 

# set up libraries
library(tidyverse)

# load data 
## flattened zooniverse output of raw classifications 
raw_classifications <-  read.csv("data/camera-data/zoo/launch1/launch1_output.csv")
## subjects for subject_id and subject_set_id link
l1_subjects <- read.csv(file = "data/camera-data/zoo/launch1/launch1_subjects.csv")
l1_subjects$X <- NULL
## merge the two zoo files 
raw_classifications <- left_join(raw_classifications, l1_subjects, by = c("subject_ids" = "subject_id"))

## Camera Capture Event data for CCE_name and Zooniverse's Subject Set ID
cce <- read.csv(file = "data/camera-data/camera_capture_event.csv")
### limit to needed columns
cce <- dplyr::select(cce, CameraCaptureEvent_id, ZooniverseRelease, subject_set_id)
cce$subject_set_id <- as.integer(cce$subject_set_id) # make integer instead of character string

## after cce uploaded to Zooniverse, zooniverse applies their own id, "subject_ids". We need to merge this id into our master sheet
# load the classification flat file 

## Detection_info 
trigger_ids <- read.csv(file= "data/camera-data/detection_info.csv")

# link the datasheets 
raw_c1 <- left_join(raw_classifications, cce, by = "subject_set_id")

# select out columns needed for a manifest 
raw_c1_viewing <- dplyr::select(raw_c1, classification_id, subject_ids, CameraCaptureEvent_id, subject_set_id, ImageFile1, ImageDate1, ImageFile2, ImageDate2, choice, classifications_count)
birds <- dplyr::filter(raw_c1_viewing, choice %in% c("LARGEBIRD", "SMALLBIRD"))

