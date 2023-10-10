# Create Matrix of site x sequence presence 

# load results for prey species 
mdata_asc <- read.csv(file = "data/scat-data/molecular-data/results_no_carnivores.csv")

# load site data so we can link with check events 
sites <- read.csv(file = "data/camera-data/site_info.csv")