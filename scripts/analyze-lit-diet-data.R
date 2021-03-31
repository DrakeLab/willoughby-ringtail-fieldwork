# Literature Diet Analyses
library(dplyr)
library(ggplot2)
library(GGally)
library(geomnet)
library(ggnetwork)
library(igraph)

'%notin%' <- function(x,y)!('%in%'(x,y))

# load data 
diet_studies <- read.csv(file = "data/literature-data/diet-study-traits.csv")
diet_items <- read.csv(file = "data/literature-data/study-diet-associations.csv")

# select only study and diet name columns 
# filter to species 
diet_sp <- filter(diet_items, taxon_rank == "species")
# count unique taxonomic ranks
## SPECIES 
study_sp_count <- diet_sp %>% 
  group_by(Source) %>% 
  summarise(n_species = n_distinct(Species))
## GENERA
diet_genus <- filter(diet_items, taxon_rank %in% c("species", "genus"))
study_genus_count <- diet_sp %>% 
  group_by(Source) %>% 
  summarise(n_genera = n_distinct(Genus)) 
## FAMILY 
diet_fam <- filter(diet_items, taxon_rank %in% c("species", "genus", "family"))
study_fam_count <- diet_sp %>% 
  group_by(Source) %>% 
  summarise(n_fam = n_distinct(Family)) 

## ORDER 
diet_order <- filter(diet_items, taxon_rank %in% c("species", "genus", "family", "order"))
study_order_count <- diet_sp %>% 
  group_by(Source) %>% 
  summarise(n_order = n_distinct(Order))

# merge in study trait info 
diet_studies <- left_join(diet_studies, study_sp_count, by = c("source_id" = "Source"))
diet_studies <- left_join(diet_studies, study_genus_count, by = c("source_id" = "Source"))
diet_studies <- left_join(diet_studies, study_fam_count, by = c("source_id" = "Source"))
diet_studies <- left_join(diet_studies, study_order_count, by = c("source_id" = "Source"))

# filter to scat studies 
scat_studies <- filter(diet_studies, n_Scat %notin% c("n/a", "unclear", ""))
plot(scat_studies$n_Scat, scat_studies$n_genera)

di_al <- select(diet_items, Source, ReportedName)
di_al_m <- circlize::adjacencyList2Matrix(di_al)
network <- graph_from_incidence_matrix(di_al_m )

ds.net <- network(diet_items$edges[, 1:2], directed = FALSE)