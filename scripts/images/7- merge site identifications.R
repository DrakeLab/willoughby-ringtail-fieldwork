# this script merges all cce into a site-specific identity file\

# set up library
library(tidyverse) # data wrangling 
library(lubridate) # dealing with dates
library(magrittr) # piping
`%notin%` <- Negate(`%in%`) # define #notin

# Load cce metadata
cce <- read_csv("data/camera/camera_capture_event.csv", show_col_types = FALSE)

# Get unique folders
site_folders <- unique(cce$site_id)

# manual site folder: 
site_folders <- "GCPR"

# Step 1: Loop through each folder to check that columns will align
for (folder in site_folders) {
  
  folder_path <- file.path("data/camera/sites/GRCA", folder)
  
  # Find identity files
  identity_files <- list.files(path = folder_path, pattern = "_identities\\.csv$", full.names = TRUE)
  
  folder_profiles <- list()
  column_names_list <- list()
  
  for (file in identity_files) {
    tryCatch({
      col_names <- names(read_csv(file, n_max = 0, show_col_types = FALSE))
      
      # Save raw column names for comparison
      column_names_list[[file]] <- col_names
      
      # Save row for visual comparison
      col_row <- as_tibble(set_names(as.list(col_names), paste0("column_", seq_along(col_names))))
      col_row$file <- basename(file)
      
      folder_profiles[[length(folder_profiles) + 1]] <- col_row
      
    }, error = function(e) {
      message("❌ Error reading columns from: ", file)
      message("   ↳ ", conditionMessage(e))
    })
  }
  
  if (length(folder_profiles) > 0) {
    
    # Check if all column names match
    unique_names <- unique(column_names_list)
    
    if (length(unique_names) > 1) {
      message("🚫 Mismatched column names in folder: ", folder, " — skipping merge.")
      
      # Optionally write mismatched summary
      folder_summary <- bind_rows(folder_profiles) %>% relocate(file)
      output_path <- file.path(folder_path, paste0(folder, "_column_profiles_MISMATCH.csv"))
      write_csv(folder_summary, output_path)
      
      next  # Skip this folder
    }
    
    # If consistent, write column profile as normal
    folder_summary <- bind_rows(folder_profiles) %>% relocate(file)
    output_path <- file.path(folder_path, paste0(folder, "_column_profiles.csv"))
    write_csv(folder_summary, output_path)
    message("✅ Columns match in folder: ", folder, " — summary written.")
  }
}

# Master list of all identities across sites
all_identities <- list()

# Step 2: Loop through folders to merge all detection events into a site-specific file 
for (folder in site_folders) {
  
  folder_path <- file.path("data/camera/sites/GRCA", folder)
  
  # Find all *_identities.csv files in folder
  identity_files <- list.files(path = folder_path, pattern = "_identities\\.csv$", full.names = TRUE)
  
  site_identities <- list()
  
  for (file in identity_files) {

      df <- read_csv(file, show_col_types = FALSE)
      if ("ImageDate1" %in% names(df)) df$ImageDate1 <- as.character(df$ImageDate1)
      if ("ImageDate2" %in% names(df)) df$ImageDate2 <- as.character(df$ImageDate2)
      
      df <- df %>%
        mutate(
          top_value = as.character(top_value), # make all top_values character to accommodate "arw"
          consensus_pct = as.character(consensus_pct), # make all consensus_pct character to accommodate "arw"
          subject_id = as.factor(subject_id) # accommodate empties
        ) 
      
      # Extract cce_id like GC15H_CC_08Mar2022
      filename <- basename(file)
      cce_id <- str_extract(filename, "^[^_]+_CC_\\d{2}[A-Za-z]{3}\\d{4}")
      
      df <- df %>% mutate(cce_id = cce_id)
      
      # Store with filename tracking
      site_identities[[file]] <- df
      all_identities[[file]] <- df
    
  }
  
  # Combine and write folder-specific file
  if (length(site_identities) > 0) {
    # Check for column consistency
    site_column_names <- lapply(site_identities, names)
    
    if (length(unique(site_column_names)) > 1) {
      warning("❌ Column mismatch in folder: ", folder)
      next
    }
    
    site_combined <- bind_rows(site_identities)
    site_outfile <- file.path("data/camera/sites/GRCA", folder, paste0(folder, "_identities_combined.csv"))
    write_csv(site_combined, site_outfile)
  }
}

# Step 3: Override with expert identifications 
# read in expert data 
arw_identities <- read.csv(file = "data/camera/arw_identities.csv")

# Loop through folders to replace rows with arw identified 
for (folder in site_folders) {
 
  # make file path
  file_path <- file.path("data/camera/sites", folder, paste0(folder, "_identities_combined.csv"))
  
  # Skip if file doesn't exist
  if (!file.exists(file_path)) {
    message("⚠️ Skipping ", folder, " — no identities_combined.csv found.")
    next
  }

  # Read in the site's combined identity file
  combined_identity_file <- read_csv(file.path(
    "data/camera/sites", folder, paste0(folder, "_identities_combined.csv")
  ), show_col_types = FALSE)
  
  # Check for matching subject_ids in arw_identities
  matching_ids <- arw_identities %>%
    filter(subject_id %in% combined_identity_file$subject_id)
  
  if (nrow(matching_ids) == 0) {
    message("⚠️ Skipping ", folder, " — no matching subject_ids in arw_identities.")
    next
  }
  
  # Filter expert rows that match subject_ids in the combined file
  expert_identities_subset <- arw_identities %>%
    filter(subject_id %in% combined_identity_file$subject_id)
  
  # Remove those rows from the original combined file
  combined_identity_file <- combined_identity_file %>%
    filter(subject_id %notin% expert_identities_subset$subject_id)
  
  # Harmonize column types before merging
  combined_identity_file <- combined_identity_file %>%
    mutate(
      top_value = as.character(top_value),
      consensus_pct = as.character(consensus_pct),
      subject_id = as.character(subject_id)
    )
  
  expert_identities_subset <-  expert_identities_subset %>%
    mutate(
      top_value = as.character(top_value),
      consensus_pct = as.character(consensus_pct),
      subject_id = as.character(subject_id)
    )
  
  # Add the expert rows back in
  combined_identity_file <- bind_rows(combined_identity_file, expert_identities_subset)
  
  # Save the updated file
  site_outfile <- file.path("data/camera/sites", folder, paste0(folder, "_identities_combined_2.csv"))
  write_csv(combined_identity_file, site_outfile)
  
  message("✅ Updated identities for: ", folder)
}


# Step 4: A. Loop through folders to remove all empties and pare to hourly occurrences
for (folder in site_folders) {
  
  folder_path <- file.path("data/camera/sites", folder)
  
  # Prefer combined_2 file if it exists
  file_2 <- file.path(folder_path, paste0(folder, "_identities_combined_2.csv"))
  file_1 <- file.path(folder_path, paste0(folder, "_identities_combined.csv"))
  
  # Safely pick which file to use
  if (file.exists(file_2)) {
    input_file <- file_2
  } else if (file.exists(file_1)) {
    input_file <- file_1
  } else {
    message("⚠️ No identity file found for folder: ", folder)
    next
  }
  
  # Read and clean
  df <- read_csv(input_file, show_col_types = FALSE) %>%
    filter(top_identity != "NOTHINGHERE", !is.na(top_identity)) %>%
    mutate(
      ImageDate1 = coalesce(
        ymd_hms(ImageDate1, tz = "UTC", quiet = TRUE),
        ymd_hm(ImageDate1, tz = "UTC", quiet = TRUE),
        mdy_hm(ImageDate1, tz = "UTC", quiet = TRUE)
      )
    ) %>%
    arrange(ImageDate1)
  
  if (all(is.na(df$ImageDate1))) {
    message("🚫 Could not parse ImageDate1 in folder: ", folder)
    next
  }
  
  # -----------------------------
  # A. Species-independent 60-min filter
  # -----------------------------
  filtered_rows <- list()
  last_seen <- list()
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    identity <- pull(row, top_identity)
    time <- pull(row, ImageDate1)
    
    if (is.na(identity) || is.na(time)) next
    
    if (is.null(last_seen[[identity]]) || difftime(time, last_seen[[identity]], units = "mins") > 60) {
      filtered_rows[[length(filtered_rows) + 1]] <- row
      last_seen[[identity]] <- time
    }
  }
  
  if (length(filtered_rows) == 0) {
    message("⚠️ No rows passed hourly filter in: ", folder)
    next
  }
  
  df_filtered <- bind_rows(filtered_rows)
  
  output_hourly <- file.path(folder_path, paste0(folder, "_filtered_60min.csv"))
  write_csv(df_filtered, output_hourly)
  message("✅ Wrote hourly-filtered file: ", output_hourly, " (", nrow(df_filtered), " rows)")
  
  # -----------------------------
  # B. Species-per-day filter
  # -----------------------------
  df_daily <- df_filtered %>%
    mutate(day = as.Date(ImageDate1)) %>%
    group_by(top_identity, day) %>%
    slice_min(order_by = ImageDate1, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  output_daily <- file.path(folder_path, paste0(folder, "_filtered_daily.csv"))
  write_csv(df_daily, output_daily)
  message("✅ Wrote daily-filtered file: ", output_daily, " (", nrow(df_daily), " rows)")
}

# Step 5: merge all the standardized data together 

# Helper function to standardize column types
standardize_columns <- function(df, folder) {
  df %>%
    mutate(
      top_value = as.character(top_value),
      consensus_pct = as.character(consensus_pct),
      subject_id = as.character(subject_id),
      ImageDate1 = as.character(ImageDate1),
      ImageDate2 = as.character(ImageDate2),      
      site_id = folder,
      cce_id = as.character(cce_id)
    )
}

# Master list of all filtered data by hour
all_filtered <- list()

for (folder in site_folders) {
  file_path <- file.path("data/camera/sites", folder, paste0(folder, "_filtered_60min.csv"))
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path, show_col_types = FALSE)
    
    df <- standardize_columns(df, folder)
    
    all_filtered[[folder]] <- df
    message("✅ Added: ", folder)
  } else {
    message("⚠️ No filtered_60min file in folder: ", folder)
  }
}

# Combine into one big data frame
filtered_all_sites <- bind_rows(all_filtered)

# Save it
write_csv(filtered_all_sites, "data/camera/GRCA_filtered_60min.csv")
message("🎉 All sites combined into: GRCA_filtered_60min.csv")

# Master list of all filtered data by day
all_filtered <- list()

for (folder in site_folders) {
  file_path <- file.path("data/camera/sites", folder, paste0(folder, "_filtered_daily.csv"))
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path, show_col_types = FALSE)
    
    df <- standardize_columns(df, folder)
    
    all_filtered[[folder]] <- df
    message("✅ Added: ", folder)
  } else {
    message("⚠️ No filtered_daily file in folder: ", folder)
  }
}

# Combine into one big data frame
filtered_all_sites <- bind_rows(all_filtered)

# Save it
write_csv(filtered_all_sites, "data/camera/GRCA_filtered_daily.csv")
message("🎉 All sites combined into: GRCA_filtered_daily.csv")