#load packages

# data wrangling
library(tidyr)
library(dplyr)

# multiple machine compatibility
library(here)

# set camera capture id to be added
cce_id <- "GCBB_CC_12Sep2021" # change for each folder
site_id <- "GCBB" # change for each folder

# load in data
detection_info <- read.csv(here("data/camera-data/detection_info.csv"))
manifest <- read.csv(file = "/media/caleb/SeagateExpansionDrive/anna/media_prep/Ringtail_Cameras/GCBB_CC_12Sep2021/manifest.csv") 
                      # update path with cce_id
# paths on different machines
  # on Anna's macbook for Seagate 
  # "/Volumes/SeagateExpansionDrive/anna/media_prep/Ringtail_Cameras/

  # on Anna's macbook for Field Hard drive 
  # "/Volumes/Ringtail 1/Ringtail_Cameras/"

  # on CJ's laptop for Drake drive
  # "/media/caleb/SeagateExpansionDrive/anna/media_prep/Ringtail_Cameras/"

# add site information to manifest and correct existing column names
manifest$CameraCaptureEvent_id <- cce_id
manifest$SiteID <- site_id
manifest$TriggerID <- paste0(cce_id, "_", manifest$X)

# photo CCEs
colnames(manifest)[colnames(manifest) == "ImageFile1"] <- "MediaFile1"
colnames(manifest)[colnames(manifest) == "ImageFile2"] <- "MediaFile2"
colnames(manifest)[colnames(manifest) == "ImageDate1"] <- "Media1DateTime"
colnames(manifest)[colnames(manifest) == "ImageDate2"] <- "Media2DateTime"

# video CCEs
# colnames(manifest)[colnames(manifest) == "SourceFile"] <- "MediaFile1"
# colnames(manifest)[colnames(manifest) == "FileModifyDate"] <- "Media1DateTime"
# manifest$MediaFile2 <- "n/a"
# manifest$Media2DateTime <-  "n/a"

# CCE info that needs to be changed to match settings
manifest$Settings <- "2 photos"
manifest$media_type <- "photo"
manifest$SiteType <- "natural"
manifest$detection_duration <- "2"

manifest$trap_day <- "to be added"
manifest$MediaURL <- "n/a"
manifest$MediaEmbedCode <- "n/a"
manifest$media_condition <- "to be added"
manifest$species_detected <- "to be added"
manifest$human_detected <- "to be added"
manifest$individuals <- "to be added"
manifest$notes <- "to be added"

# arrange manifest to be joined
detection_info <- select(detection_info, !X)
manifest <- select(manifest, !X)
manifest <- manifest[c(10, 6, 5, 8, 9, 7, 12, 1, 2, 13, 14, 3, 4, 15, 16, 17, 18, 11, 19)]

# join manifest to detection info
detection_info_step2 <- rbind(detection_info, manifest)

#save new csv
write.csv(detection_info_step2, here("data/camera-data/detection_info.csv"))
