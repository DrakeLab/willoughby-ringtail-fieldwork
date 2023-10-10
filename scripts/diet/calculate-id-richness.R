# load libraries 
library(tidyverse)

# load data
m_data_nc <- read.csv(file = "data/scat-data/molecular-data/results_no_carnivores.csv") # jonah ventures results
scat <- read.csv(file = "data/scat-data/morpho-data/scat.csv") # scat metadata
scat_ce <- dplyr::select(scat, diameter_mm, segment_id, check_event, site_type, mol_sample, JV_Barcode) ## select cols from scat 

# define the order of diet categories 
diet_cat_order <-  c("plants", "inverts", "verts", "parasites")


# summarise OTUs by primer group
OTU_summary <- m_data_nc %>% 
  group_by(segment_id, target) %>% 
  summarise(n_OTUs = n())
all_combinations <- expand.grid(segment_id = unique(m_data_nc$segment_id), target = unique(m_data_nc$target))
OTU_summary <- OTU_summary %>%
  full_join(all_combinations, by = c("segment_id", "target")) %>%
  mutate(n_OTUs = ifelse(is.na(n_OTUs), 0, n_OTUs))
site_type <- 
OTU_summary <- left_join(OTU_summary, scat_ce, by = "segment_id")
OTU_summary$target <- factor(OTU_summary$target, levels = diet_cat_order)  # Reorder the levels of the factor

# plot richness boxplots by site type 
OTUst_plot<- ggplot(OTU_summary, aes (x = site_type, y = n_OTUs)) +
    geom_boxplot() + 
    geom_jitter() + 
    facet_wrap(target ~ ., scales = "free_y", ncol = 4) + 
    theme_classic()
OTUst_plot


## STATISTICS for OTUs 

inverts <- OTU_summary %>% filter(target == "inverts")
invert_building <- inverts$n_OTUs[inverts$site_type == "building"]
invert_outdoor <- inverts$n_OTUs[inverts$site_type == "outdoor"]
# confirm variances equal 
var.test(invert_building, invert_outdoor) # similar: use Students 
# run Students T test
t.test(invert_building, invert_outdoor, var.equal = TRUE) # NOT SIGNIFICANT

verts <- OTU_summary %>% filter(target == "verts")
vert_building <- verts$n_OTUs[verts$site_type == "building"]
vert_outdoor <- verts$n_OTUs[verts$site_type == "outdoor"]
# confirm variances equal 
var.test(vert_building, vert_outdoor) # different: use Welches
t.test(vert_building, vert_outdoor) # NOT SIGNIFICANT

plants <- OTU_summary %>% filter(target == "plants")
plants_building <- plants$n_OTUs[plants$site_type == "building"]
plants_outdoor <- plants$n_OTUs[plants$site_type == "outdoor"]
# confirm variances equal 
var.test(plants_building, plants_outdoor) # similar: use Students 
t.test(plants_building, plants_outdoor, var.equal = TRUE) # NOT SIGNIFICANT

para <- OTU_summary %>% filter(target == "parasites")
para_building <- para$n_OTUs[para$site_type == "building"]
para_outdoor <- para$n_OTUs[para$site_type == "outdoor"]
# confirm variances equal 
var.test(para_building, para_outdoor) # similar: use Students 
t.test(para_building, para_outdoor, var.equal = TRUE)  # NOT SIGNIFICANT

# summarise Families by primer group
fam_summary <- m_data_nc %>% 
  group_by(segment_id, target) %>% 
  summarise(n_Families = n_distinct(Family))
fam_summary <- fam_summary %>%
  full_join(all_combinations, by = c("segment_id", "target")) %>%
  mutate(n_Families = ifelse(is.na(n_Families), 0, n_Families))
fam_summary <- left_join(fam_summary, scat_ce, by = "segment_id")
fam_summary$target <- factor(fam_summary$target, levels = diet_cat_order)  # Reorder the levels of the factor

## STATISTICS for FAMILIES
# PLANTS
plants_fam <- fam_summary %>% filter(target == "plants")
plants_fam_building <- plants_fam$n_Families[plants_fam$site_type == "building"]
plants_fam_outdoor <- plants_fam$n_Families[plants_fam$site_type == "outdoor"]
# confirm variances equal 
var.test(plants_fam_building, plants_fam_outdoor) # similar: use Students 
t.test(plants_fam_building, plants_fam_outdoor, var.equal = TRUE) # SIGNIFICANT

# INVERTS
inverts_fam <- fam_summary %>% filter(target == "inverts")
invert_fam_building <- inverts_fam$n_Families[inverts_fam$site_type == "building"]
invert_fam_outdoor <- inverts_fam$n_Families[inverts_fam$site_type == "outdoor"]
# confirm variances equal 
var.test(invert_fam_building, invert_fam_outdoor) # disimilar: use Welches 
# run T test
t.test(invert_fam_building, invert_fam_outdoor) # SIGNIFICANT

# VERTS
verts_fam <- fam_summary %>% filter(target == "verts")
vert_fam_building <- verts_fam$n_Families[verts_fam$site_type == "building"]
vert_fam_outdoor <- verts_fam$n_Families[verts_fam$site_type == "outdoor"]
# confirm variances equal 
var.test(vert_fam_building, vert_fam_outdoor) # similar: use Students
t.test(vert_fam_building, vert_fam_outdoor, var.equal = TRUE) # NOT SIGNIFICANT

# Parasites
para_fam <- fam_summary %>% filter(target == "parasites")
para_fam_building <- para_fam$n_Families[para_fam$site_type == "building"]
para_fam_outdoor <- para_fam$n_Families[para_fam$site_type == "outdoor"]
# confirm variances equal 
var.test(para_fam_building, para_fam_outdoor) # similar: use Students 
t.test(para_fam_building, para_fam_outdoor, var.equal = TRUE) # NOT SIGNIFICANT

# plot richness boxplots by site type 
fams_plot<- ggplot(fam_summary, aes (x = site_type, y = n_Families)) +
  geom_boxplot() + 
  geom_jitter() + 
  facet_wrap(target ~ ., scales = "free_y", ncol = 4) + 
  theme_classic()  
  # annotate("text", x = c(building, outdoor), y = -Inf, label = "*", hjust = 0.5, size = 6)
fams_plot