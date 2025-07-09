library(geodata)       
library(terra)         
library(virtualspecies) 
#-------------------
# 6. Replicating data for model prediction averaging

# Load VS and region mask data
sim_sp1_pa <- readRDS("data/MyVirtualSpecies.RDS")
australia_clim1km <- rast("data/australia_clim1km.tif")

# Load functions
load("scripts/0_Functions.R")