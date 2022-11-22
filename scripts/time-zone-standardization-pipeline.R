# load packages

# data wrangling
library(tidyr)
library(dplyr)

# date formatting
library(lubridate)

# multiple machine compatibility
library(here)

# load in data
# test data -- detections <- read.csv(file = "/home/caleb/Pictures/school/test_timezone_pipeline.csv")
camera_capture_events <- read.csv(here(file = "data/camera-data/camera_capture_event.csv"))
detections <- read.csv(here(file = "data/camera-data/detection_info.csv"))

# select time zone-related data and filter for camera capture events with time zone info filled out
camera_time <- camera_capture_events %>%
  select(CameraCaptureEvent_id, Site, TimeDate_TimeZone, SiteTime_Correct, SiteTime_Adjust_Direction, SiteTime_Adjust_Value) %>%
  filter(!is.na(SiteTime_Adjust_Direction))
    # filtering with !is.na() isn't working? doesn't seem to recognize blanks as NA? unsure how necessary this is long-term

# map time zone data from camera capture events to detections?
detection_times <- left_join(
  camera_time, 
  detections,
  by = "CameraCaptureEvent_id") # for test data, add %>% filter(detection_times$CameraCaptureEvent_id == "EC2_CC_02Oct2021")

# set times to be recognized as dates
detection_times %>%
  as_date(detection_times$Media1DateTime)
# how to transform dates in dataframes to be recognized as dates for addition?

# adjust incorrect camera times to fit site time for Media 1 
detection_times$MediaDateTime1_step1 <- ifelse(detection_times$SiteTime_Correct == "Yes", 
                                               detection_times$MediaDateTime1, 
                                               ifelse(detection_times$SiteTime_Adjust_Direction == "positive",
                                                      detection_times$SiteTime_Correct + detection_times$SiteTime_Adjust_Value, 
                                                      detection_times$SiteTime_Correct - detection_times$SiteTime_Adjust_Value))
                                                      
# adjust incorrect camera times to fit site time for Media 2                                                
detection_times$MediaDateTime2_step1 <-  ifelse(detection_times$SiteTime_Correct == "Yes", 
                                               detection_times$MediaDateTime2, 
                                               ifelse(detection_times$SiteTime_Adjust_Direction == "positive",
                                                      detection_times$SiteTime_Correct + detection_times$SiteTime_Adjust_Value, 
                                                      detection_times$SiteTime_Correct - detection_times$SiteTime_Adjust_Value))

# standardize site times to UTC

# write new csv