# This script should be run for each photo camera capture event where a trigger event is two photos, after an exif.csv list of file names and dates has been created in a terminal using exiftool.
# This script organizes the metadata so that camera capture events can be uploaded to Zooniverse with both photos in a trigger event organized into one subject.

# set up libraries 
library(dplyr) 
library(readr)
library(tidyr)

# set working directory 

# on Anna's macbook for Seagate 
# setwd("/Volumes/SeagateExpansionDrive/anna/media_prep/")

# on Anna's macbook for Field Hard drive 
# setwd("/Volumes/Ringtail 1/Ringtail_Cameras/")

# on CJ's laptop for Drake drive
# setwd("/media/caleb/SeagateExpansionDrive/anna/media_prep/Ringtail_Cameras/")

setwd("/media/caleb/SeagateExpansionDrive/anna/media_prep/")

# decide which folder you are manipulating 
SiteId <- "GCBB_CC_12Sep2021"

# exif <- read.csv(paste0(“Ringtail_Cameras/", SiteId, "/exif.csv”)) 
exif <- read.csv(file = "Ringtail_Cameras/GCBB_CC_12Sep2021/exif.csv") #edit for appropriate folder name 

# count how many media files 
nrow(exif)
exif$odd_rows <- seq_len(nrow(exif)) %% 2
exif$sfile_name <- ifelse(exif$odd_rows == 1, "SourceFile1", "SourceFile2")
exif$dt_name <- ifelse(exif$odd_rows == 1, "DateTimeOriginal1", "DateTimeOriginal2")


# count trigger events for a 2 photo camera setting
trigger_events <- c(1:(nrow(exif)/2))
# create duplicates for each trigger event in a data frame 
df <- data.frame(trigger_events, trigger_events)
# manipulate data into one column
df <- df %>% 
  pivot_longer(everything(),  
               names_to = c(".value"),
               names_pattern = "(.)"
)
# bind trigger event number to original exif 
exif <- cbind(exif, df$t)
exif$odd_rows <- NULL
# 
exif_wide <- exif %>%
  pivot_wider(id_cols = `df$t`, 
              names_from = c(sfile_name, dt_name), 
              values_from = c(SourceFile, DateTimeOriginal))

exif_wide2 <- rename(exif_wide , 
                   "TriggerID_count" = "df$t",
                   "ImageFile1" = "SourceFile_SourceFile1_DateTimeOriginal1",
                   "ImageFile2" = "SourceFile_SourceFile2_DateTimeOriginal2",
                   "ImageDate1" = "DateTimeOriginal_SourceFile1_DateTimeOriginal1",
                   "ImageDate2" = "DateTimeOriginal_SourceFile2_DateTimeOriginal2")

# add column trigger_id 
exif_wide2$TriggerID <- paste(SiteId, "_", str_sub(exif_wide2$ImageFile1, end = -5), sep = "")
exif_wide2$TriggerID_count <- NULL
# reorder columns 
exif <- exif_wide2[c(5,1,3,2,4)]

# save new csv
write.csv(exif_wide2, "Ringtail_Cameras/GCBB_CC_12Sep2021/manifest.csv") #edit for appropriate folder
