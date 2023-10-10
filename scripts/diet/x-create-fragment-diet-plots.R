# create diet item NMDS plot
# load library
library(tidyverse) #for data cleaning and manipulations
library(vegan) # for NMDS analysis
library(igraph) # adj lists and matrices
`%notin%` <- Negate(`%in%`) # not in function

# read in data

# this is each latrine data
latrines <- read.csv(file = "data/ZNP-2019_Fecal_Latrines.csv") %>% 
  filter(LatrineBag != "") # filter out unassessed latrines
l_id <- latrines %>% select(Latrine, LatrineBag, LatrineAreaType) # only latrine and latrine bag column 

# this is each scat segment data 
segments <- read.csv(file = "data/ZNP-2019_Fecal_Segments.csv") %>% 
  filter(Initials != "ZB") # remove Zoe's samples as they were pooled by latrine 
seg_id <- segments %>% 
  select(Segment, segment_id)

# this is each categorical diet fragment 
data <- read.csv(file = "data/ZNP-2019_Fecal_Fragments.csv") %>% 
  filter(Initials != "ZB") # remove Zoe's samples as they were pooled by latrine 
data <- data[,1:10] # remove extra columns 
data <- left_join(data, seg_id, by = "Segment")
data <- left_join(data, l_id, by = "LatrineBag")

# Count how many segments there are per building type 

data %>% 
  group_by(LatrineAreaType) %>%
  summarise(count_segments = n_distinct(segment_id)) 

# There are 67 segments analyses from staff buildings and 57 from the lodge 

# create a edge list of segment x diet item including segment and fragment weights

seg_id <- segments %>% 
  select(segment_id,, dry_weight_g)
diet_asc <- left_join(data, seg_id, by = "segment_id") %>%
  filter(dry_weight_g != "") # filtering out incomplete segments for now 

# calculate relative frequency by weight
diet_asc$relative_weight <- diet_asc$DriedFragmentWeight / diet_asc$dry_weight_g 

# columns needed latrine 
diet_asc <- diet_asc %>% 
  select(LatrineAreaType, Latrine, segment_id, FragmentType, DriedFragmentWeight, dry_weight_g, relative_weight) # you have multiple vertebrates 
write.csv(diet_asc, file = "data/latrine-fragment-data.csv")

# sum to the so segment-fragment type is unique

segment_level <- diet_asc %>% 
  group_by(as.factor(segment_id), as.factor(FragmentType), .drop = FALSE) %>% 
  summarise(fragment_weight = sum(DriedFragmentWeight),
            segment_weight = mean(dry_weight_g)) 
# replace NaN with missing value (NA) 
segment_level$segment_weight <- ifelse(segment_level$segment_weight == "NaN", NA, segment_level$segment_weight)

segment_level <- segment_level %>%
  fill(segment_weight, .direction = "downup")

# merge in Latrine information

latrine_segment <- left_join(segments, l_id, by = "LatrineBag") %>% 
  select(Latrine,color, segment_id, LatrineAreaType)

segment_level <- left_join(segment_level, latrine_segment, by = c("as.factor(segment_id)" = "segment_id"))

# sum to the so latrine-fragment type is unique
latrine_level <- segment_level %>% 
  group_by(LatrineAreaType, color, Latrine, `as.factor(FragmentType)`) %>% 
  summarise(fragment_weight = sum(fragment_weight),
            latrine_weight = sum(segment_weight))

# calculate relative weight of the latrine (eg pooled 10 scat samples)
latrine_level$relative_weight <- latrine_level$fragment_weight / latrine_level$latrine_weight

# remove contaminant and organic to reduce clutter 
latrine_level <- dplyr::filter(latrine_level, `as.factor(FragmentType)` %notin% c("Contaminant", "Organic"))

x1 <- ggplot(latrine_level, aes(y = relative_weight, x = `as.factor(FragmentType)`, fill = LatrineAreaType)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(position=position_jitterdodge()) +
  theme(axis.text.y = element_text(colour = "black", size = 10), 
        axis.text.x = element_text(colour = "black", size = 10), 
        legend.text = element_text(size = 12, colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        plot.background = element_rect(fill = "#d8d8d8ff"),
        legend.key=element_blank()) + 
  labs(x = "Diet Category", colour = "Building", y = "relative frequency by weight")  + 
  scale_fill_manual(values =  c("#1D2D44", "#A59132",
                                 "#1D2D44", "#A59132", 
                                 "#1D2D44", "#A59132", 
                                 "#1D2D44", "#A59132", 
                                 "#1D2D44", "#A59132"))
x1
ggsave("figures/diet_latrine-boxplot.png") 

# columns needed for NMDS analyses
diet_asc <- diet_asc %>% select(segment_id, FragmentType, relative_weight) # you have multiple vertebrates 


# edge list to matrix
diet_matrix <- pivot_wider(diet_asc, 
                           names_from = FragmentType, 
                           values_from = relative_weight, 
                           values_fn = sum, 
                           values_fill = 0)
diet_matrix$Contaminant <- NULL
diet_matrix$Organic <- NULL
diet_matrix$Unknown <- NULL

# remove segments that contain none of the vertebrate, inverterbrate, plant, or anthropogenic categories 
diet_matrix <- diet_matrix %>% 
  filter(segment_id %notin% c("s80", "s71", "", "s72", "s73", "s75", "s121"))

# create matrix
diet_matrix <- diet_matrix %>% 
  remove_rownames %>% 
  column_to_rownames(var="segment_id") %>% 
  as.matrix()

# run NMDS plot 
set.seed(75)
diet_NMDS=metaMDS(diet_matrix, # Our segment-by-fragment type matrix
                  distance = "bray",
                  trymax =105 ) # The number of reduced dimensions

NMDS <- as.data.frame(diet_NMDS$points)
plot(diet_NMDS)
ordiplot(diet_NMDS,type="n")
str(diet_NMDS) # gives stress values

# merge in latrine variable for annotation
plotted_segments <- rownames(diet_NMDS$points) %>% as.data.frame()
plotted_segments <- left_join(plotted_segments, segments, by = c("." = "segment_id")) %>% unique()
plotted_segments <- left_join(plotted_segments, l_id, by = "LatrineBag")  %>% unique()

building=plotted_segments$LatrineAreaType # create group distinguish
colors = plotted_segments$color

#Plot convex hulls with colors baesd on treatment
ord <- ordiellipse(diet_NMDS, groups = plotted_segments$LatrineAreaType, draw="polygon", kind = "se", conf = 0.95, col = c("#1D2D44", "#6B2B06") ,label = T)
orditorp(diet_NMDS,display="species",col="#A59132",cex = 1.25, air=0.01)
orditorp(diet_NMDS,display="sites",col = colors, cex=0.6,air=0.01)


# Using ggplot 

NMDS <- cbind(NMDS, plotted_segments)
write.csv(NMDS, file = "data/diet_NMDS_output.csv")
xx = ggplot(NMDS, aes(x = MDS1, y = MDS2, col = LatrineAreaType)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        plot.background = element_rect(fill = "#d8d8d8ff"),
        #panel.background = element_rect(fill = "#d8d8d8ff"), panel.border = element_rect(colour = "black",fill = "#d8d8d8ff", size = 1.2),
        legend.key=element_blank()) +
  geom_point(size = 4) +
  stat_ellipse() +
  labs(x = "NMDS1", colour = "Building", y = "NMDS2")  + 
  scale_colour_manual(values = c("#1D2D44", "#A59132")) + 
  annotate("text", x = c(-1, 0, 0.5, 1), y = c(-0.25, 0.75, -1, 0.4), 
           label = c("Vertebrate","Invertebrate", "Anthropogenic", "Plant"), 
           colour = c("#800000", "#800000", "#800000", "#800000"), 
           size = c(12, 12, 12, 12))
xx
ggsave("figures/diet_NMDS.png")

# now we want to do some statistics 

anosim_building = anosim(diet_matrix, NMDS$LatrineAreaType)
anosim_building # take a look at results
summary(anosim_building)
plot(anosim_building)


adonis_building = adonis(diet_matrix ~ LatrineAreaType, NMDS)

