## Get unique sequence data 
library(tidyverse)

# load carnivore data 
carni_data <- read.csv(file = "data/scat-data/molecular-data/results_carnivores.csv")
carni_d <- dplyr::select(carni_data, TestId, ESVId, sequence, JV_sample, segment_id, check_event, site_type)

unique_seq <- carni_d %>% 
  group_by(sequence) %>% 
  summarise(n_ESVs = n(), 
            n_TestIds = n_distinct(TestId),
            n_segments = n_distinct(segment_id), 
            n_ce = n_distinct(check_event))

# create a sequence Id 
unique_seq$seq_id <- paste("carnivore_seq",row_number(unique_seq$sequence) , sep = "_")
# write file 
write.csv(unique_seq, file = "data/scat-data/molecular-data/unique-carnivores.csv")