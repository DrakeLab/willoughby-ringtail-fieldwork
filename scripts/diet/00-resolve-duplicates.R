# resolve duplicate ID issue 
library(tidyverse)

JV2_18S <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB2404-18S-read-data.csv")
JV2_18S_l <- JV2_18S %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads")
processed_samples <- processed_samples <- data.frame(JV_barcode = as.vector(colnames(JV2_18S[,c(13:82)])))
processed_samples$JV_barcode <- str_sub(processed_samples$JV_barcode, end = -3) # remove the ".1"

# Load my ids 
scat <- read.csv(file = "data/scat-data/morpho-data/scat.csv") # ringtail scat metadata
rs_samples <- read.csv(file = "data/scat-data/morpho-data/rsq_fe.csv")
# 
processed_samples$rt_id_match <- ifelse(processed_samples$JV_barcode %in% scat$JV_Barcode, "yes", "no")
processed_samples$rs_id_match <- ifelse(processed_samples$JV_barcode %in% rs_samples$JVB_ID, "yes", "no")