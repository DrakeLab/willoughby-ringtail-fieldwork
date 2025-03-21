# link zooniverse output to check event information 

# set up libraries
library(tidyverse) # wrangling
library(magrittr) # piping
 
# load data 
## flattened zooniverse output of raw classifications 
raw_classifications <-  read.csv("data/camera-data/zoo/launch1_output.csv")
## subjects for subject_id and subject_set_id link
l1_subjects <- read.csv(file = "data/camera-data/zoo/launch1_subjects.csv")
l1_subjects$X <- NULL
## merge the two zoo files 
raw_classifications <- left_join(raw_classifications, l1_subjects, by = c("subject_ids" = "subject_id"))

## Camera Capture Event data for CCE_name and Zooniverse's Subject Set ID
# Download CCE from the Master Media File: 
cce <- read.csv(file = "data/camera-data/camera_capture_event.csv") 
### limit to needed columns
cce <- dplyr::select(cce, CameraCaptureEvent_id, ZooniverseRelease, subject_set_id, data_gap_present)
cce$subject_set_id <- as.integer(cce$subject_set_id) # make integer instead of character string

# filter cce to the Zooniverse launch intended 
launch1_cce <- dplyr::filter(cce, ZooniverseRelease == "Speciate #11938")
## after cce uploaded to Zooniverse, zooniverse applies their own id, "subject_ids". This is manually inputted in our cce sheet as "subject_set_id". We can link the files with this column. 

# load the classification flat file 

## Detection_info 
# trigger_ids <- read.csv(file= "data/camera-data/detection_info.csv")

# link the datasheets 
raw_c1 <- left_join(raw_classifications, cce, by = "subject_set_id", relationship = "many-to-many")

# select out columns needed for a manifest 
raw_c1_viewing <- dplyr::select(raw_c1, classification_id, subject_ids, CameraCaptureEvent_id, subject_set_id, ImageFile1, ImageDate1, ImageFile2, ImageDate2, choice, classifications_count)
birds <- dplyr::filter(raw_c1_viewing, choice %in% c("LARGEBIRD", "SMALLBIRD"))

## linking March 2025 
# read in a CCE manifest 

# merge camera check manifests with zooniverse subject set ids 


# load in subjects <- this has subject id 
subjects <- read.csv(file = "data/camera/zoo/subjects_flat.csv")

# load in cce metadata 
cce <- read.csv(file = "data/camera/camera_capture_event.csv")

# filter cces to only cces with proper filepath manifest 
cce <- cce %>% filter(str_starts(trigger_manifest_csv, "0 - "))
cce$trigger_manifest_csv <-  cce$trigger_manifest_csv %>%  
  str_remove("^0 - ")

# clean up the cce_id for looping 
cce <- cce %>%
  mutate(cce_id = str_remove(cce_id, "_nodg$"))
cce <- cce %>%
  mutate(cce_id = str_remove(cce_id, "_3trigger_v2$"))
cce_loops <- cce %>% 
  dplyr::select(cce_id, site_id, trigger_manifest_csv) %>% 
  unique()

# for solo cce and manual manipulation
cce_loops <- data.frame(
  cce_id = "GCPP_CC_25Jul2021",
  site_id = "GCPP",
  trigger_manifest_csv = "manifest_full.csv",
  stringsAsFactors = FALSE
)


# Create a list to store processed data
processed_data <- list()

# 1 - Link SUBJECT_ID: Loop through each row in the cce data frame to connect 
for (i in seq_len(nrow(cce_loops))) {
  
  # Extract values from the current row
  prefix <- cce_loops$cce_id[i]  # File prefix
  folder_name <- cce_loops$site_id[i]  # Folder name
  file_suffix <- cce_loops$trigger_manifest_csv[i]  # File suffix (full filename)
  
  # Construct the full file path
  file_path <- paste0("data/camera/sites/", folder_name, "/", paste(prefix, file_suffix, sep = "_"))
  
  # Check if the file exists before attempting to read
  if (file.exists(file_path)) {
    
    # Read the CSV file
    data <- read_csv(file = file_path)
    
    # First, try merging with subjects using TriggerID
    data_merged <- left_join(data, subjects, by = c("TriggerID" = "metadata_list_TriggerID"))
    
    # Count distinct subject_ids
    num_subjects <- data_merged %>% 
      filter(!is.na(subject_id)) %>% 
      n_distinct(.$subject_id)
    
    # If no subject_ids were found, try merging by image filenames and dates instead
    if (num_subjects == 0) {
      data_merged <- left_join(data, subjects, by = c("ImageFile1" = "metadata_list_ImageFile1", 
                                                      "ImageDate1" = "metadata_list_ImageDate1",
                                                      "ImageFile2"  = "metadata_list_ImageFile2"))
    }
    
    
    # Filter out empty subject IDs
    data_merged <- filter(data_merged, !is.na(subject_id))
    
    # Remove incorrect workflows
    data_merged <- filter(data_merged, workflow_id != "27359")
    
    # Remove problematic columns
    data_merged$X.1 <- NULL
    data_merged$`...1` <- NULL
    data_merged$X.x <- NULL
    
    # Dynamically determine column selection based on the number of columns
    total_columns <- ncol(data_merged)
    
    selected_columns <- c("TriggerID", "ImageFile1", "ImageDate1", "ImageFile2", "ImageDate2", "subject_id", 
                            "project_id", "workflow_id", "subject_set_id", "classifications_count", "retired_at", 
                            "retirement_reason", "created_at", "location_list_0",	"location_list_1")
                            
    # Select only the relevant columns
    data_merged <- data_merged[, selected_columns]
    # Store the processed data in the list
    processed_data[[prefix]] <-  data_merged
    
    # Construct output file path
    output_file_path <- paste0("data/camera/sites/", folder_name, "/", prefix, "_", str_remove(file_suffix, ".csv"), "_w_subjids.csv")
    
    # Write the processed data back to a CSV file
    write_csv(data_merged, file = output_file_path)
    
  } else {
    message("File not found: ", file_path)
  }
}

# 2 - Link CONSENSUS_ID: Loop through each row in the cce data frame to connect 

# load in cce metadata 
cce <- read.csv(file = "data/camera/camera_capture_event.csv")

# filter cces to only cces with proper filepath manifest 
cce <- cce %>% filter(str_starts(trigger_manifest_csv, "1 - "))
cce$trigger_manifest_csv <-  cce$trigger_manifest_csv %>%  
  str_remove("^1 - ")

# clean up the cce_id for looping 
cce <- cce %>%
  mutate(cce_id = str_remove(cce_id, "_nodg$"))
cce <- cce %>%
  mutate(cce_id = str_remove(cce_id, "_3trigger_v2$"))
cce_loops <- cce %>% 
  dplyr::select(cce_id, site_id, trigger_manifest_csv) %>% 
  unique()

# for solo cce and manual manipulation
cce_loops <- data.frame(
  cce_id = "GCRT_CC_25Jul2021",
  site_id = "GCRT",
  trigger_manifest_csv = "manifest_full_w_subjids.csv",
  stringsAsFactors = FALSE
)

# load in consensus ids 
s1_consensus <- read.csv(file = "data/camera/zoo/consensus_identities/s1_consensus.csv") #11938
s2_consensus <- read.csv(file = "data/camera/zoo/consensus_identities/s2_consensus.csv") #26114
s3_consensus <- read.csv(file = "data/camera/zoo/consensus_identities/s3_consensus.csv") #26238
s4_consensus <- read.csv(file = "data/camera/zoo/consensus_identities/s4_consensus.csv") #26586
s5_consensus <- read.csv(file = "data/camera/zoo/consensus_identities/s5_consensus.csv") #26731
s6_consensus <- read.csv(file = "data/camera/zoo/consensus_identities/s6_consensus.csv") #23895
s7_consensus <- read.csv(file = "data/camera/zoo/consensus_identities/s7_consensus.csv") #27033

# Loop through the cce metadata
for (i in seq_len(nrow(cce_loops))) {
  
  prefix <- cce_loops$cce_id[i]
  folder <- cce_loops$site_id[i]
  suffix <- cce_loops$trigger_manifest_csv[i]  # File suffix (full filename)
  # Build file stub for output
  file_stub <- str_remove(paste0(prefix, "_", suffix), ".csv$")
  
  # Input path for cleaned manifest
  input_path <- paste0("data/camera/sites/", folder, "/", paste(prefix, suffix, sep = "_"))
  
  if (file.exists(input_path)) {
    df <- read_csv(input_path)
    
    df <- df %>% dplyr::filter(workflow_id != "27359") # remove outreach workflows 
    
    # Skip if no workflow_id column
    if (!"workflow_id" %in% colnames(df)) next
    
    # Loop over each unique workflow_id in the file
    for (workflow in unique(df$workflow_id)) {
      
      df_filtered <- df %>% filter(workflow_id == workflow)
      
      selected_columns <- c("TriggerID", "ImageFile1", "ImageDate1", "ImageFile2", "ImageDate2", "subject_id", 
                            "project_id", "workflow_id", "subject_set_id", "classifications_count", "retired_at", 
                            "retirement_reason", "created_at", "location_list_0",	"location_list_1", 
                            "top_identity", "top_value", "consensus_pct")
      
      # Join with the appropriate consensus file and select columns
      if (workflow == 11938) {  # Season 1
        df_joined <- left_join(df_filtered, s1_consensus, by = c("subject_id" = "subject_ids"))
        df_joined <- df_joined[, selected_columns]    # Select only the relevant columns
        
      } else if (workflow == 26114) {  # Season 2
        df_joined <- left_join(df_filtered, s2_consensus, by = c("subject_id" = "subject_ids"))
        df_joined <- df_joined[, selected_columns]    # Select only the relevant columns
        
      } else if (workflow == 26238) {  # Season 3
        df_joined <- left_join(df_filtered, s3_consensus, by = c("subject_id" = "subject_ids"))
        df_joined <- df_joined[, selected_columns]    # Select only the relevant columns
        
      } else if (workflow == 26586) {  # Season 4
        df_joined <- left_join(df_filtered, s4_consensus, by = c("subject_id" = "subject_ids"))
        df_joined <- df_joined[, selected_columns]    # Select only the relevant columns
        
      } else if (workflow == 26731) {  # Season 5
        df_joined <- left_join(df_filtered, s5_consensus, by = c("subject_id" = "subject_ids"))
        df_joined <- df_joined[, selected_columns]    # Select only the relevant columns
        
      } else if (workflow == 23895) {  # Season 6
        df_joined <- left_join(df_filtered, s6_consensus, by = c("subject_id" = "subject_ids"))
        df_joined <- df_joined[, selected_columns]    # Select only the relevant columns
        
      } else if (workflow == 27033) {  # Season 7
        df_joined <- left_join(df_filtered, s7_consensus, by = c("subject_id" = "subject_ids"))
        df_joined <- df_joined[, selected_columns]    # Select only the relevant columns
        
      } else {
        message("Unrecognized workflow: ", workflow, " in ", prefix)
        next
      }
      
      # Construct output path with workflow_id in the filename
      output_path <- file.path("data/camera/sites", folder, paste0(file_stub, "_w", workflow, "_identities.csv"))

      # Write the identity-tagged file
      write_csv(df_joined, output_path)
    }
  } else {
    message("File not found: ", input_path)
  }
}
