# load packages

# data wrangling
library(tidyr)
library(dplyr)

# date formatting
library(lubridate)

# multiple machine compatibility
library(here)

# load in data
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
  by = c("CameraCaptureEvent_id" = "CameraCaptures"))

# adjust incorrect camera times to fit site time
detection_times %>%
  

# standardize site times to UTC

# write new csv