# Filter JVB Results to only ringtail data 
# set up libraries 
library(tidyverse)
# Load molecular data 
## 18 S
JV1_18S <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB1230-18S-read-data.csv")
JV1_18S_l <- JV1_18S %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)
JV1_18S_l$X <- NULL
JV2_18S <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB2404-18S-read-data.csv")
JV2_18S_l <- JV2_18S %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)


JV2_18S_l$X <- NULL
JV18S <- rbind(JV1_18S_l, JV2_18S_l)
JV18S$target <- "parasites"

## plants 
JV1_chl <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB1230-trnL-read-data.csv")
JV1_chl_l <- JV1_chl %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)
JV1_chl_l$X <- NULL
JV2_chl <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB2404-trnL-read-data.csv")
JV2_chl_l <- JV2_chl %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)
JV2_chl_l$X <- NULL
JVchl <- rbind(JV1_chl_l, JV2_chl_l)
JVchl$target <- "plants"


## inverts 
JV1_COI <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB1230-ArthCOI-read-data.csv")
JV1_COI_l <- JV1_COI %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)
JV1_COI_l$X <- NULL
JV2_COI <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB2404-ArthCOI-read-data.csv")
JV2_COI_l <- JV2_COI %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)
JV2_COI_l$X <- NULL
JVCOI <- rbind(JV1_COI_l, JV2_COI_l)
JVCOI$target <- "inverts"

## verts 
JV1_12S <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB1230-Ac12S-read-data.csv")
JV1_12S_l <- JV1_12S %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)
JV1_12S_l$X <- NULL
JV2_12S <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/read-data/JVB2404-12SVert-read-data.csv")
JV2_12S_l <- JV2_12S %>% 
  pivot_longer(cols = starts_with("S0"), 
               names_to = "JV_sample", 
               values_to = "reads") %>% 
  dplyr::filter(reads != 0)
JV2_12S_l$X <- NULL
JV12S <- rbind(JV1_12S_l, JV2_12S_l)
JV12S$target <- "verts"

## All sequence data 
JV_data <- rbind(JV12S, JVchl, JVCOI, JV18S)
JV_data$JV_sample <- substr(JV_data$JV_sample, 1,7)
## filter to only ringtail
# read in sample id data 
jv_ids <- read.csv(file = "data/scat-data/molecular-data/jv_outputs/jv_submission_ids.csv")
JV_data <- left_join(JV_data, jv_ids, by = "JV_sample")
m_data <- JV_data %>% 
  dplyr::filter(species == "Bassariscus astutus")
write.csv(m_data, file = "data/scat-data/molecular-data/raw_results.csv")

# load data
m_data <- read.csv(file = "data/scat-data/molecular-data/raw_results.csv") # jonah ventures results
scat <- read.csv(file = "data/scat-data/morpho-data/scat.csv") # scat metadata
scat_ce <- dplyr::select(scat, diameter_mm, segment_id, check_event, site_type, mol_sample, JV_Barcode) ## select cols from scat 

# merge metadata with sequence data 
m_data <- left_join(m_data, scat_ce, by = c("JV_sample" = "JV_Barcode"))
# remove carnivore data from 12S 
m_data_nc <- m_data %>% 
  dplyr::filter(!(Order == "Carnivora")) %>% 
  droplevels()
# save file 
write.csv(m_data_nc, file = "data/scat-data/molecular-data/results_no_carnivores.csv")

# separate for only carnivore data from 12S 
carni_data <- m_data %>% 
  dplyr::filter((Order == "Carnivora")) %>% 
  droplevels()
carni_data$ringtail_confirmed <- ifelse(carni_data$sequence == "CTTAGCCCTAAACACAAACAATTAACCTAACAAAATTGTCTGCCAGAGAACTACTAGCAACAGCTTAAAACTCAAAGGACTTGGCGGTGCTTTATATCCCTCTAGAGGAGCCTGTTCTATAATCGATAAACCCCGATAAACCTCACCACCTCTAGCTAAACCAGTCTATATACCGCCATCTTCAGCAAACCCTTAAAAGGAAGAATAGTAAGCACAATAATAATACATAAAAAAGTTAGGTCAAGGTGTAACCCATGAGGTGGAAAGAAATGGGCTACATTTTCTAAATAAGAACACACCCACGGAAGTTTTTATGAAACTAAAAACTGAAGGCGGATTTAGTAGTAAATTAAGAATAGAGAGCTTAATTGAATCGGGCCATGAAGCACGCAC",
                                        "ringtail_12S_match", "not_ringtail_12S_match")

# save file 
write.csv(carni_data, file = "data/scat-data/molecular-data/results_carnivores.csv")

