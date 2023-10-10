# Get sample counts and Prevalences 
library(epitools)
m_data_nc <- read.csv(file = "data/scat-data/molecular-data/results_no_carnivores.csv") # jonah ventures results

# samples by site type count
samples_st <- m_data_nc %>% 
  group_by(site_type) %>%
  summarise(n_samples = n_distinct(segment_id))

# samples by primer target 
samples <- m_data_nc %>% 
  group_by(site_type, target) %>% 
  summarise(n_samples = n_distinct(segment_id)) %>% 
  pivot_wider(names_from = target, 
              values_from = n_samples) 


samples_st <- m_data_nc %>% 
  group_by(site_type) %>%
  summarise(n_samples = n_distinct(segment_id))


samples <- m_data_nc %>% 
  group_by(site_type, target) %>% 
  summarise(n_samples = n_distinct(segment_id)) %>% 
  pivot_wider(names_from = target, 
              values_from = n_samples)

plant_data <- matrix(c(35, 2, 21, 2), ncol = 2)
colnames(plant_data) <- c("Building", "Outdoor")
rownames(plant_data) <- c("OutcomeYes", "OutcomeNo")
plantOR <- oddsratio(plant_data)
print(plantOR)

inv_data <- matrix(c(31, 6, 17, 6), ncol = 2)
colnames(inv_data) <- c("Building", "Outdoor")
rownames(inv_data) <- c("OutcomeYes", "OutcomeNo")
invOR <- oddsratio(inv_data)
print(invOR)

vert_data <- matrix(c(20, 17, 12, 11), ncol = 2)
colnames(vert_data) <- c("Building", "Outdoor")
rownames(vert_data) <- c("OutcomeYes", "OutcomeNo")
odds_ratio_result <- odds.ratio(table_data)
print(odds_ratio_result)

table_data <- matrix(c(50, 10, 20, 30), ncol = 2)
colnames(inv_data) <- c("Building", "Outdoor")
rownames(inv_data) <- c("OutcomeYes", "OutcomeNo")



# Calculate the odds ratio
odds_ratio_result <- odds.ratio(table_data)
print(odds_ratio_result)