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
# count unique species 
study_sp_count <- diet_sp %>% 
  group_by(Source) %>% 
  summarise(n_species = n_distinct(Species))
# merge in study trait info 
diet_studies <- left_join(diet_studies, study_sp_count, by = c("source_id" = "Source"))
# filter to scat studies 
scat_studies <- filter(diet_studies, n_Scat %notin% c("n/a", "unclear", ""))

di_al <- select(diet_items, Source, ReportedName)
di_al_m <- circlize::adjacencyList2Matrix(di_al)
network <- graph_from_incidence_matrix(di_al_m )

ds.net <- network(diet_items$edges[, 1:2], directed = FALSE)